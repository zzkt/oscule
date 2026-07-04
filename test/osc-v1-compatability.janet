# -*- mode: janet; -*-
# OSC tests for v1 compatability

(import oscule :as osc)

(var *pass* 0)
(var *fail* 0)

(defn- check [v msg]
  (if v (++ *pass*)
      (do (++ *fail*)
          (print (string/format "  FAIL [%d]: %s" *fail* msg)))))


(print "OSC v1 compatability. required types: i,f,s,b")

(print " • int32 encoding/decoding")
# encode 16843009 → 0x01010101
(check (= (osc/encode-int32 16843009) (string/from-bytes 1 1 1 1))
        "encode-int32 basic")
# encode -16843010 → 0xFEFEFEFE
(check (= (osc/encode-int32 -16843010) (string/from-bytes 254 254 254 254))
        "encode-int32 negative")
# decode math/int32-max → 2147483647
(check (= (osc/decode-int32 (string/from-bytes 127 255 255 255) 0) math/int32-max)
        "decode-int32 max")
# decode math/int32-min → -2147483648
(check (= (osc/decode-int32 (string/from-bytes 128 0 0 0) 0) math/int32-min)
        "decode-int32 min")
# decode 0x7FFFFFFF (int32-max as hex)
(check (= (osc/decode-int32 (string/from-bytes 127 255 255 255) 0) 0x7FFFFFFF)
        "decode-int32 0x7FFFFFFF")
# decode 0xFFFFFFFF → -1
(check (= (osc/decode-int32 (string/from-bytes 255 255 255 255) 0) -1)
        "decode-int32 -1")

# unsigned decode of 0xFFFFFFFF returns same as signed decode (blshift overflow)
(check (= (osc/decode-uint32 (string/from-bytes 255 255 255 255) 0)
           (osc/decode-int32  (string/from-bytes 255 255 255 255) 0))
        "decode-uint32 matches decode-int32")

(print " • float32 encoding/decoding")
# encode 1.00001 → 0x3F800054
(check (= (osc/encode-float32 1.00001) (string/from-bytes 63 128 0 84)) "encode-float32")
# decode 0x01010101 → very small positive float (denormalized)
(check (= (osc/decode-float32 (string/from-bytes 1 1 1 1) 0) 2.3694278276172396e-38) "decode-float32 small")
# encode -2.3694278e33 → 0xF6E9A4C4
(check (= (osc/encode-float32 -2.3694278e33) (string/from-bytes 246 233 164 196)) "encode-float32 negative")
# decode 0x7F7FFFFF → FLT_MAX (~3.4e38)
(check (= (osc/decode-float32 (string/from-bytes 127 127 255 255) 0) 3.4028234663852886e+38) "decode-float32 FLT_MAX")
# decode 0x7FFFFFFF → NaN (only value not equal to itself)
(check (not= (osc/decode-float32 (string/from-bytes 127 255 255 255) 0)
              (osc/decode-float32 (string/from-bytes 127 255 255 255) 0))
        "decode-float32 NaN")


(print " • strings")
# decode "null padded\0" → "null padded"
(check (= (osc/decode-string (string/from-bytes 110 117 108 108 32 112 97 100 100 101 100 0) 0)
           "null padded")
        "decode-string")

# encode "OSC string encoding test" → padded to 4-byte boundary
(check (= (osc/encode-string "OSC string encoding test")
           (string/from-bytes 79 83 67 32 115 116 114 105 110 103 32 101 110 99 111 100 105 110 103 32 116 101 115 116 0 0 0 0))
        "encode-string")


(print " • blobs")
# encode #{1 1 1 1} → 4-byte length prefix + data
(check (= (osc/encode-blob (string/from-bytes 1 1 1 1))
           (string/from-bytes 0 0 0 4 1 1 1 1))
        "encode-blob")


(print " • messages")
# decode "/test/int" with single int32 arg -1
(def m1 (osc/decode-message (string/from-bytes 47 116 101 115 116 47 105 110 116 0 0 0 44 105 0 0 255 255 255 255)))
(check (= (m1 :address) "/test/int") "msg address")
(check (= (get (m1 :args) 0) -1) "msg arg -1")

# decode "/test/one" with args 1, 2, 3.3 (int32, int32, float32)
(def m6 (osc/decode-message (string/from-bytes 47 116 101 115 116 47 111 110 101 0 0 0 44 105 105 102 0 0 0 0 0 0 0 1 0 0 0 2 64 83 51 51)))
(check (= (m6 :address) "/test/one") "t6 address")
(check (= (get (m6 :args) 0) 1) "t6 arg0")
(check (= (get (m6 :args) 1) 2) "t6 arg1")
(check (= (get (m6 :args) 2) 3.2999999523162842) "t6 arg2")

# encode message "/asdasd" with two 32bit floats (3.6, 4.5)
(check (= (osc/encode-message "/asdasd" 3.6 4.5)
           (string/from-bytes 47 97 115 100 97 115 100 0 44 102 102 0 64 102 102 102 64 144 0 0))
        "encode floats")

# round-trip "/asdasd" with two floats; decode after encode produces matching struct
(def m13 (osc/decode-message (osc/encode-message "/asdasd" 3.6 4.5)))
(check (= (m13 :address) "/asdasd") "t13 address")
(check (= (get (m13 :args) 0) 3.5999999046325684) "t13 arg0")
(check (= (get (m13 :args) 1) 4.5) "t13 arg1")
(check (= ((osc/decode-message (osc/encode-message "/asdasd" 3.6 4.5)) :address) "/asdasd") "recode")

# encode message "/blob/x" with a blob argument (9 bytes padded to 12)
(check (= (osc/encode-message "/blob/x" (buffer (string/from-bytes 1 2 3 4 5 6 7 8 9)))
           (string/from-bytes 47 98 108 111 98 47 120 0 44 98 0 0 0 0 0 9 1 2 3 4 5 6 7 8 9 0 0 0)) "encode blob msg")

# encode message "/s/t0" with a single string arg "four"
(check (= (osc/encode-message "/s/t0" "four")
           (string/from-bytes 47 115 47 116 48 0 0 0 44 115 0 0 102 111 117 114 0 0 0 0))
        "encode /s/t0")
(def sp1 (osc/decode-message (osc/encode-message "/s/t0" "four")))
(check (= (sp1 :address) "/s/t0") "sp1 address")
(check (= (get (sp1 :args) 0) "four") "sp1 arg")

# encode message "/s/t0" with mixed args (int32, string, int32)
(check (= (osc/encode-message "/s/t0" 2 "xxxxx" 3)
           (string/from-bytes 47 115 47 116 48 0 0 0 44 105 115 105 0 0 0 0 0 0 0 2 120 120 120 120 120 0 0 0 0 0 0 3))
        "encode /s/t0 mixed")
(def sp2 (osc/decode-message (osc/encode-message "/s/t0" 2 "xxxxx" 3)))
(check (= (sp2 :address) "/s/t0") "sp2 address")
(check (= (get (sp2 :args) 0) 2) "sp2 arg0")
(check (= (get (sp2 :args) 1) "xxxxx") "sp2 arg1")
(check (= (get (sp2 :args) 2) 3) "sp2 arg2")

(print "OSC v1 compatability. optional types: T,F,N,I,t,h,d,S,c,r,m,and [ ].")

(print " • see v1.1 tests")

(print (string/format "\nSUMMARY: %d passed, %d failed, %d total\n"
                      *pass* *fail* (+ *pass* *fail*)))
(when (> *fail* 0) (os/exit 1))
