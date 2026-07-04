;; Benchmark oscule vs cl-osc encoding/decoding performance.
;;
;; Usage: sbcl --noinform --load benchmark.lisp --quit

(defparameter *iterations* 100000)

(defun format-time (sec)
  (if (< sec 1)
      (format nil "~,1f ms" (* sec 1000))
      (format nil "~,3f s" sec)))

(defmacro benchmark (name &body body)
  `(progn
     (format t "  ~30a " ,name)
     (finish-output)
     (let* ((start (get-internal-real-time))
            (result (dotimes (i *iterations* ,(car (last body))) (progn ,@body)))
            (end (get-internal-real-time))
            (total (/ (- end start) internal-time-units-per-second))
            (per-call (/ total *iterations*)))
       (format t "~8a total, ~,3f µs/call~%" (format-time total) (* per-call 1e6))
       result)))

(format t "~%* cl-osc benchmark (~D iterations each)~%~%" *iterations*)

(format t "~%** encode (scalar)~%")

(benchmark "encode-int32 (42)"
  (osc::encode-int32 42))

(benchmark "encode-float32 (3.14)"
  (osc::encode-float32 3.14))

(benchmark "encode-string (\"hello\")"
  (osc::encode-string "hello"))

(benchmark "encode-int64 (858993459200)"
  (osc::encode-int64 858993459200))

(format t "~%** encode (message)~%")

(benchmark "encode int message"
  (osc:encode-message "/test/int" 42))

(benchmark "encode float message"
  (osc:encode-message "/test/float" 3.14))

(benchmark "encode mixed (int+str+float)"
  (osc:encode-message "/test/mixed" 1 "two" 3.14))

(benchmark "encode big msg (10 args)"
  (osc:encode-message "/test/big" 1 2.0 "three" 4 5.0 "six" 7 8.0 "nine" 10))

(benchmark "encode long string (256 chars)"
  (let ((s (make-string 256 :initial-element #\x)))
    (osc:encode-message "/test/long" s)))

(format t "~%** decode (scalar)~%")

(defparameter *int32-bytes* (osc::encode-int32 42))
(defparameter *float32-bytes* (osc::encode-float32 3.14))
(defparameter *string-bytes* (osc::encode-string "hello"))
(defparameter *int64-bytes* (osc::encode-int64 858993459200))

(benchmark "decode-int32"
  (osc::decode-int32 *int32-bytes*))

(benchmark "decode-float32"
  (osc::decode-float32 *float32-bytes*))

(benchmark "decode-string"
  (osc::decode-string *string-bytes*))

(benchmark "decode-int64"
  (osc::decode-int64 *int64-bytes*))

(format t "~%** decode (message)~%")

(defparameter *int-msg* (osc:encode-message "/test/int" 42))
(defparameter *float-msg* (osc:encode-message "/test/float" 3.14))
(defparameter *mixed-msg* (osc:encode-message "/test/mixed" 1 "two" 3.14))
(defparameter *big-msg* (osc:encode-message "/test/big" 1 2.0 "three" 4 5.0 "six" 7 8.0 "nine" 10))
(defparameter *long-msg* (osc:encode-message "/test/long" (make-string 256 :initial-element #\x)))

(benchmark "decode int message"
  (osc:decode-message *int-msg*))

(benchmark "decode float message"
  (osc:decode-message *float-msg*))

(benchmark "decode mixed (int+str+float)"
  (osc:decode-message *mixed-msg*))

(benchmark "decode big msg (10 args)"
  (osc:decode-message *big-msg*))

(benchmark "decode long string (256 chars)"
  (osc:decode-message *long-msg*))

(format t "~% cl-osc benchmark complete ~%")

(quit)
