# -*- mode: janet; -*-
# OSC (Open Sound Control) examples

(import oscule :prefix "")

(print "Encode/decode simple message")

(def msg (encode-message "/test" 42 3.14159 "hello"))
(printf "  encoded: %d bytes" (length msg))

(def decoded (decode-message msg))
(printf "  address: %s args: %p" (decoded :address) (decoded :args))


(print "\nVarious argument types")

(def msg2 (encode-message "/types"
  42          # int32
  3.14159     # float32
  "world"     # string
  true        # true
  false       # false
  nil         # nil
  :impulse))  # impulse
(print "  encoded: " (length msg2) " bytes")

(def d2 (decode-message msg2))
(printf "  decoded args: %p" (d2 :args))
(each a (d2 :args)
  # (print (string "    " (type a) ": " a)))
  (printf "    %s: %p" (type a) a))


(print "\n64-bit types")

(def msg3 (string (encode-string "/64bit")
                  (encode-string ",h")
                  (encode-int64 (- (math/pow 2 53) 1))))  # largest int/s64
(def d3 (decode-message msg3))
(printf "  address: %s args: %p" (d3 :address) (d3 :args))


(print "\nBundle encoding/decoding")

(def msg-a (encode-message "/a" 1 2 3))
(def msg-b (encode-message "/b" "foo" "bar"))
(def bundle (encode-bundle :now msg-a msg-b))
(print "  bundle: " (length bundle) " bytes")

(def db (decode-bundle bundle))
(print "  elements: " (length (db :elements)))

(each elt (db :elements)
  (if (elt :address)
    (print (string "    message: " (elt :address)))
    (print (string "    sub-bundle: " (elt :timetag)))))


(print "\nBlobs")

(def blob-data @"\x00\x01\x02\x03\xff\xfe")
(def msg5 (encode-message "/blob" blob-data))
(def d5 (decode-message msg5))
(def blob (get (d5 :args) 0))
(print (string "  blob (" (length blob) " bytes): "
  (string/join (map |(string/format "%02x" $0) (string/bytes blob)) " ")))


(print "\nRound-trip")

(print "  " (dump-message
             (decode-message
              (encode-message "/slink" "slonk" (- (math/pow 2 53) 1)))))

(print "\nNetworking (UDP send/receive)")

(def *port* 9911)

(defn listener []
  (def msg (osc-receive *port*))
  (print "  received: " (message->string msg)))

(ev/call listener)
(ev/sleep 0.2)
(osc-send "127.0.0.1" *port* "/ping" "oscillate with oscule" 432)
(ev/sleep 0.3)
