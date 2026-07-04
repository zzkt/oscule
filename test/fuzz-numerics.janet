# -*- mode: janet; -*-
# Fuzzing tests for numeric encoding/decoding

(import oscule :as osc)

(var pass 0)
(var fail 0)

(defn- ok? [v msg] (if v (++ pass) (do (++ fail) (print "FAIL: " msg))))

# IEEE 754 float32 has 23 mantissa bits. round-trip error is bounded by 2^-23
# relative to the value, plus an absolute floor for subnormals.
(def f32-eps 1.1920928955e-7)   # 2^-23
(def f32-abs-min 1.401298464e-45) # 2^-149 (smallest float32 denormal)
(defn- f32= [a b] (<= (math/abs (- a b)) (+ (* (max (math/abs a) (math/abs b)) f32-eps) f32-abs-min)))

# IEEE 754 float64 has 52 mantissa bits. round-trip error is bounded by 2^-52
# relative to the value, plus an absolute floor for subnormals.
(def f64-eps 2.220446049250313e-16)  # 2^-52
(def f64-abs-min 4.9406564584124654e-324) # 2^-1074 (smallest float64 denormal)
(defn- f64= [a b] (<= (math/abs (- a b)) (+ (* (max (math/abs a) (math/abs b)) f64-eps) f64-abs-min)))

# seeded prng for reproducible fuzzing (use doubles to avoid 32-bit overflow)
(var *seed* 42.0)
(def seed-a 1103515245.0)
(def seed-c 12345.0)
(defn- rand-i32 []
  (set *seed* (mod (+ (* *seed* seed-a) seed-c) 2147483648.0))
  (band (brshift (math/trunc *seed*) 16) 0x7FFFFFFF))

(defn- rand-f32 [] (def d (max (rand-i32) 1)) (/ (rand-i32) d))
(defn- rand-range [lo hi] (+ lo (mod (rand-i32) (- hi lo))))

(print "\n • int32 fuzzing ")

(loop [i :range [0 10000]]
  (def n (- (rand-i32) 0x3FFFFFFF))
  (def enc (osc/encode-int32 n))
  (ok? (= (osc/decode-int32 enc 0) n) (string "int32 round-trip " n)))

# edge cases
(each n [math/int32-min math/int32-max 0 1 -1
         16843009 -16843010 0x01010101]
  (def enc (osc/encode-int32 n))
  (ok? (= (osc/decode-int32 enc 0) n) (string "int32 edge " n)))

(print (string "  " pass " passed, " fail " failed" (if (> fail 0) " *** FAILURES ***" "")))

(print "\n • uint32 fuzzing ")

(loop [i :range [0 10000]]
  (def n (rand-i32))
  (def enc (osc/encode-uint32 n))
  (ok? (= (osc/decode-uint32 enc 0) n) (string "uint32 round-trip " n)))

(print (string "  " pass " passed, " fail " failed"
               (if (> fail 0) " *** FAILURES ***" "")))

(print "\n • int64 fuzzing ")

(loop [i :range [0 5000]]
  (def n (- (mod (* (rand-i32) (rand-i32)) 9007199254740991) 4503599627370495))
  (def enc (osc/encode-int64 n))
  (ok? (= (osc/decode-int64 enc 0) n) (string "int64 round-trip " n)))

(loop [i :range [0 5000]]
  (def n (rand-range 0 9007199254740991))
  (def enc (osc/encode-int64 n))
  (ok? (= (osc/decode-uint64 enc 0) n) (string "uint64 round-trip " n)))

(each n [0 1 -1 9007199254740991 -9007199254740991
         858993459200 -858993459200 16843009 -16843009
         math/int32-max math/int32-min]
  (def enc (osc/encode-int64 n))
  (ok? (= (osc/decode-int64 enc 0) n) (string "int64 edge " n)))

(print (string "  " pass " passed, " fail " failed"
               (if (> fail 0) " *** FAILURES ***" "")))

(print "\n • float32 fuzzing ")

(loop [i :range [0 10000]]
  (def sign (if (= (band (rand-i32) 1) 0) 1 -1))
  (def exp (rand-range -126 128))
  (def mant (rand-f32))
  (def f (* sign mant (math/pow 2 exp)))
  (when (<= osc/float32-min (math/abs f) osc/float32-max)
    (def enc (osc/encode-float32 f))
    (def dec (osc/decode-float32 enc 0))
    (ok? (f32= dec f) (string "float32 round-trip " f " got " dec))))

# float32 edge cases (exact for 0, ±1; approx for inexact float32 values)
(each [n label] [[0.0 "0.0"] [-0.0 "-0.0"] [1.0 "1.0"] [-1.0 "-1.0"]
                 [3.14159 "pi"] [osc/float32-max "FLT_MAX"] [(- osc/float32-max) "-FLT_MAX"]
                 [osc/float32-min "FLT_MIN"] [(- osc/float32-min) "-FLT_MIN"]
                 [1.00001 "1.00001"] [-2.3694278e33 "neg-large"]
                 [3.4028234663852886e+38 "~FLT_MAX"]]
  (def enc (osc/encode-float32 n))
  (def dec (osc/decode-float32 enc 0))
  (ok? (f32= dec n) (string "float32 edge " label " " n " got " dec)))

# float32: nan, inf, -inf
(def nan-enc (osc/encode-float32 math/nan))
(def nan-dec (osc/decode-float32 nan-enc 0))
(ok? (not= nan-dec nan-dec) "float32 NaN preserves NaN")

(def pinf-enc (osc/encode-float32 math/inf))
(ok? (= (osc/decode-float32 pinf-enc 0) math/inf) "float32 +inf")

(def ninf-enc (osc/encode-float32 (- math/inf)))
(ok? (= (osc/decode-float32 ninf-enc 0) (- math/inf)) "float32 -inf")

# ensure NaN has unique bytes
(def n1 (osc/encode-float32 math/nan))
(def n2 (osc/encode-float32 math/nan))
(ok? (= n1 n2) "float32 NaN encoding is deterministic")

(print (string "  " pass " passed, " fail " failed" (if (> fail 0) " *** FAILURES ***" "")))

(print "\n • float64 fuzzing ")

(loop [i :range [0 10000]]
  (let [sign (if (= (band (rand-i32) 1) 0) 1 -1)
        exp (rand-range -1022 1024)
        mant (rand-f32)
        f (* sign mant (math/pow 2 exp))]
  (when (and (not= f math/inf) (not= f (- math/inf)))
    (def enc (osc/encode-float64 f))
    (def dec (osc/decode-float64 enc 0))
    (ok? (= dec f) (string "float64 round-trip " f " got " dec)))))

# float64 edge cases
(def max-normal (* (math/pow 2 1023) (- 2 (math/pow 2 -52))))
(def max-subnormal (* (math/pow 2 1022) (- 2 (math/pow 1 -52))))
(def min-positive-denormal (math/pow 2 -1074))

(each [n label] [[0.0 "0.0"] [-0.0 "-0.0"] [1.0 "1.0"] [-1.0 "-1.0"]
                 [23.1 "23.1"] [math/pi "pi"] [math/e "e"]
                 [max-normal "DBL_MAX"] [(- max-normal) "-DBL_MAX"]
                 [max-subnormal "max-subnormal"] [(- max-subnormal) "-max-subnormal"]
                 [min-positive-denormal "min-denormal"]
                 [(- min-positive-denormal) "-min-denormal"]
                 [1.0e10 "1e10"] [1.0e-10 "1e-10"]
                 [1.7976931348623157e+308 "~DBL_MAX"]
                 [4.49423283715579e+307 "~max-subnormal"]]
  (def enc (osc/encode-float64 n))
  (def dec (osc/decode-float64 enc 0))
  (ok? (= dec n) (string "float64 edge " label " " n " got " dec)))

# special float64: nan, inf, -inf + check byte-exact match
(def dnan-enc (osc/encode-float64 math/nan))
(def dnan-dec (osc/decode-float64 dnan-enc 0))
(ok? (not= dnan-dec dnan-dec) "float64 NaN preserves NaN")

(def dpinf-enc (osc/encode-float64 math/inf))
(ok? (= (osc/decode-float64 dpinf-enc 0) math/inf) "float64 +inf")
(ok? (= dpinf-enc (string/from-bytes 127 240 0 0 0 0 0 0)) "float64 +inf bytes")

(def dninf-enc (osc/encode-float64 (- math/inf)))
(ok? (= (osc/decode-float64 dninf-enc 0) (- math/inf)) "float64 -inf")
(ok? (= dninf-enc (string/from-bytes 255 240 0 0 0 0 0 0)) "float64 -inf bytes")

# ensure deterministic NaN
(def dn1 (osc/encode-float64 math/nan))
(def dn2 (osc/encode-float64 math/nan))
(ok? (= dn1 dn2) "float64 NaN encoding is deterministic")

# check NaN has correct sign (positive quiet NaN)
(ok? (= (in dn1 0) 127) "float64 NaN high byte is 127 (0x7F)")

(print (string "  " pass " passed, " fail " failed"
               (if (> fail 0) " *** FAILURES ***" "")))

(print "\n • message fuzzing ")

(loop [i :range [0 2000]]
  (let [addr (string "/test/" (rand-i32))
        n-args (rand-range 1 8)
        args @[]]
    (loop [j :range [0 n-args]]
      (def tag (rand-range 0 7))
      (array/push args
                  (if (= tag 0) (band (rand-i32) 0x7FFFFFFF)
                      (if (= tag 1) (rand-f32)
                          (if (= tag 2) (string "s" (rand-i32))
                              (if (= tag 3) (* (rand-f32) (math/pow 2 (rand-range -10 10)))
                                  (if (= tag 4) (- (band (rand-i32) 0x3FFFFFFF) 0x3FFFFFFF)
                                      (if (= tag 5) true
                                          false))))))))
    (def enc (apply osc/encode-message addr args))
    (def dec (osc/decode-message enc))
    (ok? (= (get dec :address) addr) (string "msg address: " addr))
    (let [dec-args (get dec :args)]
      (ok? (= (length dec-args) (length args))
           (string "msg arg count: " (length args) " vs " (length dec-args)))
      (loop [j :range [0 (min (length args) (length dec-args))]]
        (def a (in args j))
        (def b (in dec-args j))
        (ok? (= (type a) (type b))
             (string "msg arg " j " type: " (type a) " vs " (type b) " val=" a))
        (when (not= (type a) :number)
          (ok? (= a b) (string "msg arg " j " value: " a " vs " b)))))))

(print (string "  " pass " passed, " fail " failed" (if (> fail 0) " *** FAILURES ***" "")))

(print "\n • deterministic encoding ")

# encoding same value twice should give identical bytes
(loop [i :range [0 1000]]
  (def n (rand-i32))
  (ok? (= (osc/encode-int32 n) (osc/encode-int32 n))
        (string "det int32 " n))
  (def f (* (rand-f32) (math/pow 2 (rand-range -10 10))))
  (ok? (= (osc/encode-float32 f) (osc/encode-float32 f))
        (string "det float32 " f))
  (when (<= (math/abs f) (/ osc/float32-min))
    (ok? (= (osc/encode-float64 f) (osc/encode-float64 f))
          (string "det float64 " f))))

(print (string "  " pass " passed, " fail " failed"
               (if (> fail 0) " *** FAILURES ***" "")))

(print (string "\n" (+ pass fail)
               " total assertions (" pass " passed, " fail " failed)"))

(if (> fail 0) (error "fuzzing FAILED"))
