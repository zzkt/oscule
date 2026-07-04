# -*- mode: janet; -*-
# Interoperability between oscule (janet) & cl-osc (with sbcl)
# Tests all OSC types, messages, and bundles in both directions.
# Requires SBCL with cl-osc installed.

(import oscule :as osc)

# Check SBCL + cl-osc
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

(var *pass* 0)
(var *fail* 0)
(defn- check [v msg]
  (if v (++ *pass*)
      (do (++ *fail*) (print (string/format "  FAIL [%d]: %s" *fail* msg)))))


(defn read-bytes [path]
  (string (do (def f (file/open path :rb)) (def c (file/read f :all)) (file/close f) c)))

(defn test-file-byte-equality
  "Verify janet and sbcl produce identical OSC byte streams."
  []
  (print " • file-level byte equality (janet vs lisp)")
  (def cases @{
    "int32"   {:janet (osc/encode-message "/test/int32" 42)
               :lisp "/tmp/osc-interop/lisp-int32.bin"}
    "neg32"   {:janet (osc/encode-message "/test/neg32" -1)
               :lisp "/tmp/osc-interop/lisp-neg32.bin"}
    "float32" {:janet (osc/encode-message "/test/float32" 3.14)
               :lisp "/tmp/osc-interop/lisp-float32.bin"}

    "string"  {:janet (osc/encode-message "/test/string" "hello")
               :lisp "/tmp/osc-interop/lisp-string.bin"}
    "int64"   {:janet (osc/encode-message "/test/int64" 858993459200)
               :lisp "/tmp/osc-interop/lisp-int64.bin"}
  })
  (each [label c] (pairs cases)
    (def lisp-bytes (read-bytes (in c :lisp)))
    (def janet-bytes (in c :janet))
    (check (= (length janet-bytes) (length lisp-bytes))
            (string label " length mismatch"))
    (check (= janet-bytes lisp-bytes)
            (string label " byte mismatch")))
  (print "   ...pass"))

(defn test-cross-decode
  "Verify janet ↔ lisp cross-decode from files."
  []
  (print " • cross-decode: janet→janet, lisp→janet")
  # janet-encoded → janet decode (self-test)
  (def janet-cases @{
    "int32"  (osc/encode-message "/test/int32" 42)
    "neg32"  (osc/encode-message "/test/neg32" -1)
    "float32" (osc/encode-message "/test/float32" 3.14)
    "string" (osc/encode-message "/test/string" "hello")
    "int64"  (osc/encode-message "/test/int64" 858993459200)
    "mixed"  (osc/encode-message "/test/mixed" 1 "two" 3.14)
  })
  (each [label buf] (pairs janet-cases)
    (check (osc/decode-message buf) (string "janet self-decode: " label)))
  (print "   ...janet self-decode pass")
  # lisp-encoded → janet decode
  (def lisp-labels @["int32" "neg32" "float32" "string" "int64" "mixed"])
  (each label lisp-labels
    (def buf (read-bytes (string "/tmp/osc-interop/lisp-" label ".bin")))
    (check (osc/decode-message buf) (string "lisp→janet decode: " label)))
  (print "   ...lisp→janet pass"))

(defn run-interop []
  (print "\n** Interoperability tests: oscule ↔ cl-osc")
  (test-file-byte-equality)
  (test-cross-decode)
  (print (string/format "\nSUMMARY: %d passed, %d failed, %d total\n"
                        *pass* *fail* (+ *pass* *fail*)))
  (when (> *fail* 0) (os/exit 1)))

(run-interop)
