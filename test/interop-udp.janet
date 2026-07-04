# -*- mode: janet; -*-
# Network interop test: janet ↔ sbcl over UDP.
#
# Note: avoid fibers for UDP receive since (:recv-from ...) blocks.
# All UDP listeners run on the main thread.

(import oscule :as osc)

# Check SBCL + cl-osc availability
(when (not (protect (os/execute ["sbcl" "--version"] :p)))
  (print "SBCL not found. skipping lisp interop tests")
  (os/exit 0))
(let [out-fh (file/open "/dev/null" :wb)
      proc (os/spawn
             ["sbcl" "--noinform" "--eval" "(handler-case (ql:quickload :cl-osc :silent t) (error (e) (sb-ext:exit :code 1)))" "--quit"]
             :p {:out out-fh :err out-fh})]
  (file/close out-fh)
  (when (not= 0 (os/proc-wait proc))
    (print "cl-osc not installed. skipping lisp interop tests")
    (os/exit 0)))

(defn- run-lisp-background [code]
  (def outfile (string/format "/tmp/osc-lisp-out-%d.txt" (os/getpid)))
  (def errfile (string/format "/tmp/osc-lisp-err-%d.txt" (os/getpid)))
  (try (os/rm outfile) ([e] nil)) (try (os/rm errfile) ([e] nil))
  (def out-fh (file/open outfile :wb))
  (def err-fh (file/open errfile :wb))
  (def proc (os/spawn
    ["sbcl" "--noinform" "--load" "test/interop/lisp-helper.lisp"
     "--eval" code "--quit"]
    :p {:out out-fh :err err-fh}))
  (file/close out-fh) (file/close err-fh)
  {:proc proc :outfile outfile :errfile errfile})

(defn- wait-for-lisp [ctx]
  (def exit-code (os/proc-wait (ctx :proc)))
  (def out (slurp (ctx :outfile))) (def err (slurp (ctx :errfile)))
  (try (os/rm (ctx :outfile)) ([e] nil)) (try (os/rm (ctx :errfile)) ([e] nil))
  {:return-code exit-code :out out :err err})

(defn- send-udp [host port bytes]
  (def s (net/connect host port :datagram))
  (:write s bytes)
  (net/close s))

# Test: janet encodes → sends → lisp receives & decodes

(defn test-janet-to-lisp []
  (print "\n** janet → lisp") (flush)
  (def test-cases @{
    :int32   {:msg (osc/encode-message "/test/int32" 42)
              :match "(\"/test/int32\" 42)"}
    :neg32   {:msg (osc/encode-message "/test/neg32" -1)
              :match "(\"/test/neg32\" -1)"}
    :float32 {:msg (osc/encode-message "/test/float32" 3.14)
              :match "(\"/test/float32\" 3.14)"}
    :string  {:msg (osc/encode-message "/test/string" "hello")
              :match "(\"/test/string\" \"hello\")"}
    :int64   {:msg (osc/encode-message "/test/int64" 858993459200)
              :match "(\"/test/int64\" 858993459200)"}
    :mixed   {:msg (osc/encode-message "/test/mixed" 1 "two" 3.14)
              :match "(\"/test/mixed\" 1 \"two\" 3.14)"}})
  (var passed 0) (var failed 0) (var p 57120)
  (each [label c] (pairs test-cases)
    (def port p) (++ p)
    (def ctx (run-lisp-background (string/format "(receive-one %d)" port)))
  (os/sleep 2)
  (send-udp "127.0.0.1" port (in c :msg))
  (def result (wait-for-lisp ctx))
  (print "    got result for " label ": " (string/trim (result :out))) (flush)
  (if (string/find (in c :match) (result :out))
      (++ passed)
      (do (++ failed) (print "  FAIL " label ": " (string/trim (result :out)))))
    (flush))
  (print (string/format "  janet→lisp: %d/%d passed" passed (+ passed failed))) (flush)
  (when (> failed 0) (error "janet→lisp failed")))


# Test: lisp encodes → sends → janet receives & decodes

(defn test-lisp-to-janet []
  (print "\n** lisp → janet") (flush)
  (def test-cases @{
    :int32   "/test/int32"
    :neg32   "/test/neg32"
    :float32 "/test/float32"
    :string  "/test/string"
    :int64   "/test/int64"
    :mixed   "/test/mixed"
    :true    "/test/true"
    :false   "/test/false"
    :null    "/test/null"
    :impulse "/test/impulse"
  })
  (var passed 0) (var failed 0) (var p 57130)
  (each [label addr] (pairs test-cases)
    (def port p) (++ p)
    (def s (net/listen "127.0.0.1" port :datagram))
    (def buf (buffer/new 8192))
    (def out-fh (file/open "/dev/null" :wb))
    (def err-fh (file/open "/dev/null" :wb))
    (def proc (os/spawn
      ["sbcl" "--noinform" "--load" "test/sbcl-helper.lisp"
       "--eval" (string/format "(send-one %d :%s)" port label) "--quit"]
      :p {:out out-fh :err err-fh}))
    (file/close out-fh) (file/close err-fh)
    (def recv-addr (:recv-from s 8192 buf))
    (net/close s)
    (os/proc-wait proc)
    (def n (length buf))
    (def received (if (> n 0) (osc/decode-message buf) nil))
    (if (and received (= (received :address) addr))
      (++ passed)
      (do (++ failed) (print "  FAIL " label ": " (if received (received :address) "no response"))))
    (flush))
  (print (string/format "  lisp→janet: %d/%d passed" passed (+ passed failed))) (flush)
  (when (> failed 0) (error "lisp→janet failed")))

# Test: bundles

(defn test-bundles []
  (print "\n** bundle tests") (flush)
  # janet bundle → lisp — use receive-bundle which calls osc:decode-bundle
  (print "  bundle janet→lisp...") (flush)
  (def bundle (osc/encode-bundle :now
                (osc/encode-message "/test/a" 1)
                (osc/encode-message "/test/b" 2.0 "three")
                (osc/encode-message "/test/c" 42)))
  (def ctx (run-lisp-background "(receive-bundle 57140)"))
  (os/sleep 2)
  (send-udp "127.0.0.1" 57140 bundle)
  (def result (wait-for-lisp ctx))
  (def lisp-out (result :out))
  (if (string/find "BUNDLE" lisp-out)
    (print "    OK")
    (do (print "    error: " (string/trim (or lisp-out "(empty)"))) (error "bundle janet→lisp")))

  # lisp bundle → janet — no fibers, blocking recv
  (print "  bundle lisp→janet...") (flush)
  (def s (net/listen "127.0.0.1" 57141 :datagram))
  (def buf (buffer/new 8192))
  (def out-fh (file/open "/dev/null" :wb))
  (def err-fh (file/open "/dev/null" :wb))
  (def proc (os/spawn
    ["sbcl" "--noinform" "--load" "test/interop/lisp-helper.lisp"
     "--eval" "(send-bundle-one 57141)" "--quit"]
    :p {:out out-fh :err err-fh}))
  (file/close out-fh) (file/close err-fh)
  (def recv-addr (:recv-from s 8192 buf))
  (net/close s)
  (os/proc-wait proc)
  (def n (length buf))
  (def b (if (> n 0) (osc/decode-bundle buf) nil))
  (if (and b (in b :timetag) (in b :elements))
    (print "    OK")
    (print "    FAIL"))
  (flush))


# Test: echo round-trip

(defn test-echo []
  (print "\n** echo round-trip") (flush)
  (def port-in 57150) (def port-out 57151)
  (def s (net/listen "127.0.0.1" port-out :datagram))
  (def buf (buffer/new 8192))
  (def echo-ctx (run-lisp-background (string/format "(receive-and-echo %d %d)" port-in port-out)))
  (os/sleep 2)
  (def test-msg (osc/encode-message "/test/echo" 42 "hello" 3.14))
  (send-udp "127.0.0.1" port-in test-msg)
  (def recv-addr (:recv-from s 8192 buf))
  (net/close s)
  (def n (length buf))
  (def echo (if (> n 0) (osc/decode-message buf) nil))
  (wait-for-lisp echo-ctx)
  (if (and echo (= (echo :address) "/test/echo"))
    (print "    OK: echoed message received")
    (print "    FAIL: no echo"))
  (flush))

(defn test-interop-udp []
  (print "\n OSC Networking tests: oscule (janet) ↔ cl-osc (sbcl)\n")
  (test-janet-to-lisp)
  (test-lisp-to-janet)
  (test-bundles)
  (test-echo))

(test-interop-udp)
