# -*- mode: janet; -*-
# Benchmark encoding/decoding performance

(import oscule :as osc)

(def iterations 100000)

(defn- format-time [sec]
  (if (< sec 1)
    (string/format "%0.1f ms" (* sec 1000))
    (string/format "%0.3f s" sec)))

(defmacro benchmark [name & body]
  ~(let [start (os/clock)]
     (repeat iterations (do ,;body))
     (def elapsed (- (os/clock) start))
     (def per-call (/ elapsed iterations))
     (print (string/format "  %-30s %8s total, %0.3f µs/call"
                           ,name (format-time elapsed) (* per-call 1e6)))))

(print "\n oscule benchmark (" iterations " iterations each)\n")

(print "\n** encode (scalar)\n")

(benchmark "encode-int32 (42)"
  (osc/encode-int32 42))

(benchmark "encode-float32 (3.14)"
  (osc/encode-float32 3.14))

(benchmark "encode-string (\"hello\")"
  (osc/encode-string "hello"))

(benchmark "encode-int64 (858993459200)"
  (osc/encode-int64 858993459200))

(print "\n** encode (message)\n")

(benchmark "encode int message"
  (osc/encode-message "/test/int" 42))

(benchmark "encode float message"
  (osc/encode-message "/test/float" 3.14))

(benchmark "encode mixed (int+str+float)"
  (osc/encode-message "/test/mixed" 1 "two" 3.14))

(benchmark "encode big msg (10 args)"
  (osc/encode-message "/test/big" 1 2.0 "three" 4 5.0 "six" 7 8.0 "nine" 10))

(benchmark "encode long string (256 chars)"
  (osc/encode-message "/test/long" (string/repeat "x" 256)))

(print "\n** decode (scalar)\n")

(def int32-bytes (osc/encode-int32 42))
(def float32-bytes (osc/encode-float32 3.14))
(def string-bytes (osc/encode-string "hello"))
(def int64-bytes (osc/encode-int64 858993459200))

(benchmark "decode-int32"
  (osc/decode-int32 int32-bytes 0))

(benchmark "decode-float32"
  (osc/decode-float32 float32-bytes 0))

(benchmark "decode-string"
  (osc/decode-string string-bytes 0))

(benchmark "decode-int64"
  (osc/decode-int64 int64-bytes 0))

(print "\n** decode (message)\n")

(def int-msg (osc/encode-message "/test/int" 42))
(def float-msg (osc/encode-message "/test/float" 3.14))
(def mixed-msg (osc/encode-message "/test/mixed" 1 "two" 3.14))
(def big-msg (osc/encode-message "/test/big" 1 2.0 "three" 4 5.0 "six" 7 8.0 "nine" 10))
(def long-msg (osc/encode-message "/test/long" (string/repeat "x" 256)))

(benchmark "decode int message"
  (osc/decode-message int-msg))

(benchmark "decode float message"
  (osc/decode-message float-msg))

(benchmark "decode mixed (int+str+float)"
  (osc/decode-message mixed-msg))

(benchmark "decode big msg (10 args)"
  (osc/decode-message big-msg))

(benchmark "decode long string (256 chars)"
  (osc/decode-message long-msg))

(print "\n oscule benchmark complete\n")
