# -*- mode: janet; -*-
# OSC tests for v1.1 compatability

(import oscule :as osc)

(var *pass* 0)
(var *fail* 0)

(defn- check [v msg]
  (if v (++ *pass*)
      (do (++ *fail*)
          (print (string/format "  FAIL [%d]: %s" *fail* msg)))))

(print "OSC v1.1 compatability. required types: i,f,s,b,T,F,N,I,t")

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


(print " • timetags")
# timetag :now → 8 zero bytes + 0x00000001 (immediate)
(check (= (osc/encode-timetag :now) (string/from-bytes 0 0 0 0 0 0 0 1)) "encode-timetag :now")



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


(print " • bundles")
# decode raw bundle with 3 messages (documentation, stringmessage, tm/start)
(def b7 (osc/decode-bundle (string/from-bytes
  35 98 117 110 100 108 101 0 0 0 0 0 0 0 0 1
  0 0 0 32
  47 100 111 99 117 109 101 110 116 97 116 105 111 110 47 97
  108 108 45 109 101 115 115 97 103 101 115 0 44 0 0 0
  0 0 0 44
  47 102 111 111 47 115 116 114 105 110 103 109 101 115 115 97
  103 101 0 0 44 115 115 115 0 0 0 0
  97 0 0 0 102 101 119 0 115 116 114 105 110 103 115 0 0 0 0 28
  47 118 111 105 99 101 115 47 48 47 116 109 47 115 116 97
  114 116 0 0 44 102 0 0 0 0 0 0)))
(check (= (b7 :timetag) '(0 1)) "bundle timetag")
(def b7e (b7 :elements))
(check (= (length b7e) 3) "bundle 3 elements")
(check (= ((b7e 0) :address) "/documentation/all-messages") "b7 elt0")
(check (= ((b7e 1) :address) "/foo/stringmessage") "b7 elt1")
(check (= ((b7e 2) :address) "/voices/0/tm/start") "b7 elt2")

# round-trip bundle: encode then decode, verify timetag and element addresses
(def brt (osc/encode-bundle :now
          (osc/encode-message "/voices/0/tm/start" 0.0)
          (osc/encode-message "/foo/stringmessage" "a" "few" "strings")
          (osc/encode-message "/documentation/all-messages")))
(def brtd (osc/decode-bundle brt))
(check (= (brtd :timetag) '(0 1)) "bundle rt timetag")
(def brte (brtd :elements))
(check (= (length brte) 3) "bundle rt 3 elements")
(check (= ((brte 0) :address) "/voices/0/tm/start") "brt elt0")
(check (= ((brte 1) :address) "/foo/stringmessage") "brt elt1")
(check (= ((brte 2) :address) "/documentation/all-messages") "brt elt2")


(print " • keyword tags")
# keywords encoded as tags without data: true, false, nil, :impulse (T, F, N, I typetags)
(check (= (osc/encode-message "/tags" true false nil :impulse)
           (string/from-bytes 47 116 97 103 115 0 0 0 44 84 70 78 73 0 0 0))
        "encode keyword tags")
(def tt (osc/decode-message (osc/encode-message "/tags" true false nil :impulse)))
(check (= (get (tt :args) 0) true) "decode true")
(check (= (get (tt :args) 1) false) "decode false")
(check (= (get (tt :args) 2) nil) "decode nil")
(check (= (get (tt :args) 3) :impulse) "decode :impulse")


(print "OSC v1.1 compatability. optional types: h,d,S,c,r,m,and [ ].")


(print " • int64 uint64 encoding/decoding")
# encode 1 → 0x0000000000000001
(check (= (osc/encode-int64 1) (string/from-bytes 0 0 0 0 0 0 0 1)) "encode-int64 1")
# encode 16843009 → 0x0000000001010101
(check (= (osc/encode-int64 16843009) (string/from-bytes 0 0 0 0 1 1 1 1)) "encode-int64 basic")
# encode -1 → 0xFFFFFFFFFFFFFFFF
(check (= (osc/encode-int64 -1) (string/from-bytes 255 255 255 255 255 255 255 255)) "encode-int64 -1")
# encode -16843009 → 0xFFFFFFFFFEFEFEFF
(check (= (osc/encode-int64 -16843009) (string/from-bytes 255 255 255 255 254 254 254 255)) "encode-int64 -16843009")
# encode 2^53-1 → 0x001FFFFF_FFFFFFFF
(check (= (osc/encode-int64 (-(math/pow 2 53) 1))
           (string/from-bytes 0 31 255 255 255 255 255 255)) "encode-int64 2^53-1")
# decode 0x0000000001010101 → 16843009
(check (= (osc/decode-int64 (string/from-bytes 0 0 0 0 1 1 1 1) 0) 16843009) "decode-int64 basic")
# decode-uint64 0x0000000001010101 → 16843009
(check (= (osc/decode-uint64 (string/from-bytes 0 0 0 0 1 1 1 1) 0) 16843009) "decode-uint64 basic")
# decode 0xFFFFFFFFFFFFFFFF → -1
(check (= (osc/decode-int64 (string/from-bytes 255 255 255 255 255 255 255 255) 0) -1) "decode-int64 -1")
# decode 0x001FFFFF_FFFFFFFF → 9007199254740991
(check (= (osc/decode-int64 (string/from-bytes 0 31 255 255 255 255 255 255) 0) 9007199254740991) "decode-int64 2^53-1")
# round-trip int64: encode then decode matches original
(def int64-rt-values [1 -1 16843009 -16843009 0 2147483647 -2147483648 (-(math/pow 2 53) 1)])
(each v int64-rt-values
  (check (= (osc/decode-int64 (osc/encode-int64 v) 0) v) (string "int64 round-trip " v)))


(print " • float64 encoding/decoding")
# decode 23.1 (0x403719999999999A) → 23.1
(check (= (osc/decode-float64 (string/from-bytes 64 55 25 153 153 153 153 154) 0) 23.1)
        "decode-float64 as number")

(check (= (osc/encode-float64 23.1) (string/from-bytes 64 55 25 153 153 153 153 154))
        "encode-float64 as string")

# +inf and -inf i.e. 0x7FF0000000000000 and 0xFFF0000000000000
(check (= (osc/decode-float64 (string/from-bytes 255 240 0 0 0 0 0 0) 0) (- math/inf))
        "decode-float64 -inf")
(check (= (osc/encode-float64 (- math/inf)) (string/from-bytes 255 240 0 0 0 0 0 0))
        "encode-float64 -inf")
(check (= (osc/decode-float64 (string/from-bytes 127 240 0 0 0 0 0 0) 0) math/inf)
        "decode-float64 +inf")
(check (= (osc/encode-float64 math/inf) (string/from-bytes 127 240 0 0 0 0 0 0))
        "encode-float64 +inf")

# largest normal number. 2^1023 × (2 − 2^−52) i.e. 0x7FEFFFFFFFFFFFFF
(def max-normal-float64 (* (math/pow 2 1023) (- 2 (math/pow 2 -52))))
(check (= (osc/decode-float64 (string/from-bytes 127 239 255 255 255 255 255 255) 0)
           max-normal-float64)
        "decode-float64 largest normal number")
(check (= (osc/encode-float64 max-normal-float64)
           (string/from-bytes 127 239 255 255 255 255 255 255))
        "encode-float64 largest normal number")

# largest subnormal number. 2^1022 × (1 − 2^−52) i.e. 0x7FD0000000000000
(def max-subnormal-float64 (* (math/pow 2 1022) (- 2 (math/pow 1 -52))))
(check (= (osc/decode-float64 (string/from-bytes 127 208 0 0 0 0 0 0) 0)
           max-subnormal-float64) "decode-float64 largest subnormal number")
(check (= (osc/encode-float64 max-subnormal-float64) (string/from-bytes 127 208 0 0 0 0 0 0))
        "decode-float64 largest subnormal number")

# closest approximation to pi i.e. 0x400921FB54442D18
(check (= (osc/decode-float64 (string/from-bytes 64 9 33 251 84 68 45 24) 0) math/pi)
        "decode-float64 closest approximation to π")
(check (= (osc/encode-float64 math/pi) (string/from-bytes 64 9 33 251 84 68 45 24))
        "encode-float64 closest approximation to π")



(print " • Alternate OSC-string")
# ascii strings auto-encoded as 's'
(def a1 (osc/encode-message "/alt" "strung"))
(let [d (osc/decode-message a1)] (check (= (get (d :args) 0) "strung") "alt-string (ascii)"))

# non-ascii auto-encoded as 'S' (assume utf8 by default)
(each s ["Смрт фашизму, слобода народу!"
         "ཨོཾ་མ་ཎི་པདྨེ་ཧཱུྂ"
         "größtmögliche Fußgängerüberweggeräuschendämpfung"]
  (def enc (osc/encode-message "/alt" s))
  (let [d (osc/decode-message enc)]
    (check (= (get (d :args) 0) s) (string "alt-string " s))))

# unicode strings
```
 ཨོཾ་མ་ཎི་པདྨེ་ཧཱུྂ -> (string/from-bytes 32 224 189 168 224 189 188 224 189 190 224 188 139 224 189 152 224 188 139 224 189 142 224 189 178 224 188 139 224 189 148 224 189 145 224 190 168 224 189 186 224 188 139 224 189 167 224 189 177 224 189 180 224 190 130)

größtmögliche Fußgängerüberweggeräuschendämpfung -> (string/from-bytes 103 114 195 182 195 159 116 109 195 182 103 108 105 99 104 101 32 70 117 195 159 103 195 164 110 103 101 114 195 188 98 101 114 119 101 103 103 101 114 195 164 117 115 99 104 101 110 100 195 164 109 112 102 117 110 103)

Смрт фашизму, слобода народу! -> (string/from-bytes 208 161 208 188 209 128 209 130 32 209 132 208 176 209 136 208 184 208 183 208 188 209 131 44 32 209 129 208 187 208 190 208 177 208 190 208 180 208 176 32 208 189 208 176 209 128 208 190 208 180 209 131 33)
```


(print " • char")
(check (= (osc/encode-char 65) (string/from-bytes 0 0 0 65)) "encode-char 65")
(check (= (osc/encode-char "A") (string/from-bytes 0 0 0 65)) "encode-char from string")
(check (= (osc/decode-char (string/from-bytes 0 0 0 97) 0) 97) "decode-char 97")


(print " • rgba")
(check (= (osc/encode-rgba 255 0 128 255) (string/from-bytes 255 0 128 255))
       "encode-rgba")
(check (= (osc/encode-rgba 255 128 64 0) (string/from-bytes 255 128 64 0))
       "encode-rgba")
(check (= (osc/decode-rgba (string/from-bytes 64 128 192 255) 0) '(64 128 192 255))
       "decode-rgba")
(check (= (osc/decode-rgba (string/from-bytes 255 128 64 0) 0) '(255 128 64 0))
       "decode-rgba")


(print " • MIDI")
(check (= (osc/encode-midi 1 144 60 127) (string/from-bytes 1 144 60 127))
       "encode-midi")
(check (= (osc/decode-midi (string/from-bytes 1 144 60 127) 0) '(1 144 60 127))
       "decode-midi")


(print " • arrays (not implemented)")

(print (string/format "\nSUMMARY: %d passed, %d failed, %d total\n"
                      *pass* *fail* (+ *pass* *fail*)))

(when (> *fail* 0) (os/exit 1))
