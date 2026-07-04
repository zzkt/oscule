# -*- mode: janet; -*-
# UDP peer-to-peer network tests using localhost.

(import oscule :as osc)

(var *pass* 0)
(var *fail* 0)
(defn- check [v msg]
  (if v (++ *pass*)
      (do (++ *fail*)
          (print (string/format "  FAIL [%d]: %s" *fail* msg)))))

(def f32-eps 1.1920928955e-7)   # 2^-23
(def f32-abs-min 1.401298464e-45) # 2^-149
(defn- f32= [a b] (<= (math/abs (- a b)) (+ (* (max (math/abs a) (math/abs b)) f32-eps) f32-abs-min)))
(def f64-eps 2.220446049250313e-16)  # 2^-52
(def f64-abs-min 4.9406564584124654e-324) # 2^-1074
(defn- f64= [a b] (<= (math/abs (- a b)) (+ (* (max (math/abs a) (math/abs b)) f64-eps) f64-abs-min)))

(defn- vals-ok? [original received]
  (var ok true)
  (loop [i :range [0 (min (length original) (length received))] :when ok]
    (def ov (in original i))
    (def rv (in received i))
    (if (and (number? ov) (number? rv) (not= (math/trunc ov) ov))
      (set ok (f32= rv ov))
      (set ok (= rv ov))))
  ok)

# script to run as separate process

(def helper-script
  ```(defn- hex->buf [s]
  (def buf (buffer/new (math/ceil (/ (length s) 2))))
  (loop [i :range [0 (length s) 2]]
    (buffer/push buf (scan-number (string "0x" (string/slice s i (+ i 2))))))
  buf)
(defn recv [port]
  (def s (net/listen "0.0.0.0" (scan-number port) :datagram))
  (def buf (buffer/new 8192))
  (:recv-from s 8192 buf) (net/close s)
  (print (string/join (map (fn [b] (string/format "%02x" b)) buf) "")))
(defn send [port hex]
  (def bytes (hex->buf hex))
  (def s (net/connect "127.0.0.1" (scan-number port) :datagram))
  (:write s bytes) (net/close s))
(let [mode (get (dyn :args) 1) a1 (get (dyn :args) 2) a2 (get (dyn :args) 3)]
  (case mode "recv" (recv a1) "send" (send a1 a2))))]
 ```)

(defn- helper-path []
  (def p (string "/tmp/osc-p2p-helper-" (os/getpid) ".janet"))
  (spit p helper-script)
  p)

(defn- spawn-helper [& args]
  (def full-args @["janet" (helper-path)])
  (each a args (array/push full-args a))
  (def outfile (string "/tmp/osc-p2p-out-" (os/getpid) ".txt"))
  (def errfile (string "/tmp/osc-p2p-err-" (os/getpid) ".txt"))
  (try (os/rm outfile) ([e] nil)) (try (os/rm errfile) ([e] nil))
  (def out-fh (file/open outfile :wb))
  (def err-fh (file/open errfile :wb))
  (def proc (os/spawn full-args :p {:out out-fh :err err-fh}))
  (file/close out-fh) (file/close err-fh)
  {:proc proc :outfile outfile :errfile errfile})

(defn- capture [ctx]
  (os/proc-wait (ctx :proc))
  (def out (string/trim (slurp (ctx :outfile))))
  (def err (string/trim (slurp (ctx :errfile))))
  (try (os/rm (ctx :outfile)) ([e] nil)) (try (os/rm (ctx :errfile)) ([e] nil))
  {:out out :err err})

(defn- send-udp [host port bytes]
  (def s (net/connect host port :datagram))
  (:write s bytes)
  (net/close s))

(defn- hex->buf [s]
  (when (> (length s) 1)
    (def buf (buffer/new (/ (length s) 2)))
    (loop [i :range [0 (length s) 2]]
      (buffer/push buf (scan-number (string "0x" (string/slice s i (+ i 2))))))
    buf))

(def test-cases @[
    {:label "int32"   :address "/r/int32"   :args [42]}
    {:label "neg32"   :address "/r/neg32"   :args [-1]}
    {:label "float32" :address "/r/float32" :args [3.14]}
    {:label "float64" :address "/r/float64" :args [1.0e50]}
    {:label "string"  :address "/r/string"  :args ["hello"]}
    {:label "int64"   :address "/r/int64"   :args [858993459200]}
    {:label "mixed"   :address "/r/mixed"   :args [1 "two" 3.14]}
    {:label "true"    :address "/r/true"    :args [true]}
    {:label "false"   :address "/r/false"   :args [false]}
    {:label "null"    :address "/r/null"    :args [nil]}
    {:label "impulse" :address "/r/impulse" :args [:impulse]}])


# Test: A encodes & sends  →  B receives & decodes

(defn test-a-to-b []
  (print "  send from test -> recv in helper...") (flush)
  (var port 57200)
  (each tc test-cases
    (def p port) (++ port)
    (def ctx (spawn-helper "recv" (string p)))
    (os/sleep 0.5)
    (def msg (osc/encode-message (tc :address) ;(tc :args)))
    (send-udp "127.0.0.1" p msg)
    (def res (capture ctx))
    (def received (try (osc/decode-message (hex->buf (res :out))) ([e] nil)))
    (check (and received (= (received :address) (tc :address))
              (= (length (received :args)) (length (tc :args)))
              (vals-ok? (tc :args) (received :args)))
         (string (tc :label) ": " (res :out) " / " (when received (received :address)))))
  (print (string/format "    a→b: %d/%d passed" *pass* (+ *pass* *fail*)))
  (flush))


# Test: B encodes & sends  →  A receives & decodes

(defn test-b-to-a []
  (print "  encode in test -> send via helper -> recv in test...") (flush)
  (var port 57300)
  (each tc test-cases
    (def p port) (++ port)
    (def msg (osc/encode-message (tc :address) ;(tc :args)))
    (def hex (string/join (map (fn [b] (string/format "%02x" b)) msg) ""))
    # Listen. Then spawn the helper to send
    (def s (net/listen "127.0.0.1" p :datagram))
    (def ctx (spawn-helper "send" (string p) hex))
    (def buf (buffer/new 8192))
    (def result (:recv-from s 8192 buf))
    (net/close s)
    (capture ctx)
    (if buf
      (let [received (osc/decode-message buf)]
        (check (and (= (received :address) (tc :address))
                  (= (length (received :args)) (length (tc :args)))
                  (vals-ok? (tc :args) (received :args)))
             (string (tc :label) " address mismatch: " (received :address))))
      (check false (string (tc :label) " no response"))))
  (print (string/format "    b→a: %d/%d passed" *pass* (+ *pass* *fail*)))
  (flush))


# Test: bundles

(defn test-bundles []
  (print "  bundles...") (flush)
  # A sends bundle -> B receives
  (def msg1 (osc/encode-message "/b/a" 1))
  (def msg2 (osc/encode-message "/b/b" 2.0 "three"))
  (def bundle (osc/encode-bundle :now msg1 msg2))
  (def ctx (spawn-helper "recv" "57400"))
  (os/sleep 0.5)
  (send-udp "127.0.0.1" 57400 bundle)
  (def res (capture ctx))
  (def decoded (when (> (length (res :out)) 0)
                 (try (osc/decode-bundle (hex->buf (res :out))) ([e] nil))))
  (check (and decoded (in decoded :elements) (> (length (decoded :elements)) 0))
       (string "bundle a→b: " (res :out) " / decoded=" (if decoded "yes" "no")))

  # B sends bundle -> A receives
  (def bundle2 (osc/encode-bundle :now (osc/encode-message "/b/c" 42) (osc/encode-message "/b/d" "x")))
  (def hex (string/join (map (fn [b] (string/format "%02x" b)) bundle2) ""))
  (def s2 (net/listen "127.0.0.1" 57401 :datagram))
  (def ctx2 (spawn-helper "send" "57401" hex))
  (def b2 (buffer/new 8192))
  (def r2 (:recv-from s2 8192 b2))
  (net/close s2)
  (capture ctx2)
  (if (and r2 (> (length b2) 0))
    (let [decoded2 (try (osc/decode-bundle b2) ([e] nil))]
      (check (and decoded2 (in decoded2 :elements) (> (length (decoded2 :elements)) 0))
           "bundle b→a"))
    (check false "bundle b→a no response"))
  (flush))


# Test: echo round-trip (A sends to B, B sends back to A)

(defn test-echo []
  (print "  echo round-trip...") (flush)
  (def echo-port 57410)
  (def msg (osc/encode-message "/t/echo" 42 "hello" 3.14))
  (def hex (string/join (map (fn [b] (string/format "%02x" b)) msg) ""))
  # Listen FIRST, then spawn helper that sends
  (def s (net/listen "127.0.0.1" echo-port :datagram))
  (def ctx (spawn-helper "send" (string echo-port) hex))
  (def buf (buffer/new 8192))
  (def result (:recv-from s 8192 buf))
  (net/close s)
  (capture ctx)
  (if buf
    (let [received (try (osc/decode-message buf) ([e] nil))]
      (check (and received (= (received :address) "/t/echo"))
           (string "echo: " (if received (received :address) "decode failed"))))
    (check false "echo: no response"))
  (flush))

(defn test-p2p []
  (print "* OSC P2P network test: oscule ↔ oscule\n")
  (flush)
  (var total-pass 0)
  (var total-fail 0)
  # a -> b
  (test-a-to-b)
  (set total-pass (+ total-pass *pass*))
  (set total-fail (+ total-fail *fail*))
  (set *pass* 0) (set *fail* 0)
  # b -> a
  (test-b-to-a)
  (set total-pass (+ total-pass *pass*))
  (set total-fail (+ total-fail *fail*))
  (set *pass* 0) (set *fail* 0)
  # bundles
  (test-bundles)
  (set total-pass (+ total-pass *pass*))
  (set total-fail (+ total-fail *fail*))
  (set *pass* 0) (set *fail* 0)
  # echo
  (test-echo)
  (set total-pass (+ total-pass *pass*))
  (set total-fail (+ total-fail *fail*))
  # summary
  (print (string/format "\nSUMMARY: %d passed, %d failed, %d total\n"
                        total-pass total-fail (+ total-pass total-fail)))
  (when (> total-fail 0) (os/exit 1))
  (flush))

(test-p2p)
