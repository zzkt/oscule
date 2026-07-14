# -*- mode: janet; coding: utf-8; -*-
#
# An implementation of the OSC (Open Sound Control) protocol
#
# Copyright (c) 2026 FoAM oü
#
#  oscule is free software: you can redistribute it and/or modify it
#  under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
# Authors: nik gaffney <nik@fo.am>
#
#  translated from cl-osc (https://github.com/zzkt/osc)
#  based on the OSC specification at http://OpenSoundControl.org


# Implementation specific numeric constants
(def u32-range 4294967296)
(def u32-half 2147483648)
(def mantissa-23 8388608.0)
(def mantissa-52 4503599627370496.0)
(def f32-inf-bits 0x7F800000)
(def f32-nan-bits 0x7FC00000)
(def f64-inf-hi 0x7FF00000)
(def f64-nan-lo 0x00080000)
(def f32-mantissa-mask 0x7FFFFF)
(def f64-mantissa-mask 0xFFFFF)
(def float32-max 3.402823466e+38)
(def float32-min 1.175494351e-38)
(def -inf (- math/inf))
(def u32-max 0xFFFFFFFF)

# OSC type-tags

# OSC v1 compatability. required types: i,f,s,b
(def TAG-i 105)
(def TAG-f 102)
(def TAG-s 115)
(def TAG-b 98)
# OSC v1.1 compatability. required types: i,f,s,b,T,F,N,I,t
(def TAG-T 84)
(def TAG-F 70)
(def TAG-N 78)
(def TAG-I 73)
(def TAG-t 116)
# OSC v1.1 compatability. optional types: h,d,S,c,r,m,[ ].
(def TAG-h 104)
(def TAG-d 100)
(def TAG-S 83)
(def TAG-c 99)
(def TAG-r 114)
(def TAG-m 109)


# #   #  #   #   # #      #   #              #
#  IEEE 754 Floats
# # ##    #     #

# precomputed 2^i for i 0..52 (exact integer-valued floats)
(def pow2-table (array/new 53))
(for i 0 53 (array/push pow2-table (math/pow 2 i)))

(defn- float-normalize
  "Return (sign unbiased-exponent mantissa) for a non-zero float."
  [f]
  (let [s (if (< f 0) 1 0)
        a (math/abs f)]
    (var e 0) (var m a)
    (when (or (>= m 4) (< m 0.5))
      (def p (math/floor (/ (math/log m) (math/log 2))))
      (set e (min p 1023))
      (set m (/ m (math/pow 2 e))))
    (while (>= m 2) (set m (/ m 2)) (++ e))
    (while (< m 1) (set m (* m 2)) (-- e))
    [s e (- m 1)]))

(defn- float32-raw
  "Encode a float as a raw IEEE 754 32-bit signed integer."
  [f]
  (cond
    (not= f f) f32-nan-bits
    (= f -inf) (bor (blshift 1 31) f32-inf-bits)
    (>= (math/abs f) math/inf) f32-inf-bits
    (zero? f) 0
    (let [[s e m] (float-normalize f)
          be (min 255 (+ e 127))]
      (if (= be 255) (bor (blshift s 31) f32-inf-bits)
        (let [shift (if (< be 1) (+ be 22) 23)
              frac (if (< be 1) (+ m 1) m)
              frac-bits (math/trunc (+ (* frac (in pow2-table shift)) 0.5))]
          (bor (blshift s 31)
               (if (< be 1)
                 (band frac-bits f32-mantissa-mask)
                 (bor (blshift be 23) (band frac-bits f32-mantissa-mask)))))))))

(defn- raw-to-float32
  "Decode a raw 32-bit signed integer back into a float."
  [bits]
  (let [s (if (< bits 0) -1 1)
        be (band (brshift bits 23) 0xFF)
        f (band bits f32-mantissa-mask)]
    (cond (zero? be) (if (zero? f)
                       (* s 0.0)
                       (* s (/ (/ f mantissa-23) (math/pow 2 126))))
          (= be 255) (if (zero? f)
                       (* s math/inf)
                       math/nan)
          (* s (+ 1 (/ f mantissa-23)) (math/pow 2 (- be 127))))))

(defn- float64-raw
  "Encode a float as raw IEEE 754 [high low] 32-bit signed integers."
  [f]
  (cond
    (not= f f) [(bor (blshift 0 31) f64-inf-hi) f64-nan-lo]
    (= f -inf) [(bor (blshift 1 31) f64-inf-hi) 0]
    (>= (math/abs f) math/inf) [(bor (blshift 0 31) f64-inf-hi) 0]
    (zero? f) [0 0]
    (let [[s e m] (float-normalize f)
          be (min 2047 (+ e 1023))
          shift (if (< be 1) (+ be 51) 52)
          frac (if (< be 1) (+ m 1) m)
          frac-bits (math/trunc (+ (* frac (in pow2-table shift)) 0.5))
          lo (mod frac-bits u32-range)
          hi (math/trunc (/ (- frac-bits lo) u32-range))]
      (if (= be 2047) [(bor (blshift s 31) f64-inf-hi) 0]
        (if (< be 1)
          [(bor (blshift s 31) (band hi f64-mantissa-mask)) lo]
          [(bor (blshift s 31) (blshift be 20) (band hi f64-mantissa-mask)) lo])))))

(defn- raw-to-float64
  "Decode raw [high low] 32-bit signed integers back into a float."
  [high low]
  (let [s (if (< high 0) -1 1)
        be (band (brshift high 20) 0x7FF)
        lo (if (< low 0) (+ low u32-range) low)
        num (+ (* (band high f64-mantissa-mask) u32-range) lo)]
    (cond (zero? be) (if (zero? num) (* s 0.0) (* s (/ (/ num mantissa-52) (math/pow 2 1022))))
          (= be 2047) (if (zero? num) (* s math/inf) math/nan)
          (* s (+ 1 (/ num mantissa-52)) (math/pow 2 (- be 1023))))))


#  # #    #  #            #
# Strings
# #    #  #   #   # #

(defn- nulls [n]
  (string (buffer/new-filled n 0)))

(defn- padded-length [n]
  (band (+ n 3) -4))

(defn- pad4 [s]
  (let [l (length s)]
    (string s (nulls (- (padded-length (inc l)) l)))))


# #   #  #   #     #  #   #              #
# Element encoding
# #  #   #     #  #   #

(defn encode-int32
  "Encode a 32-bit signed integer as 4 bytes in network byte order."
  [n]
  (string/from-bytes
    (band (brshift n 24) 0xFF) (band (brshift n 16) 0xFF)
    (band (brshift n 8) 0xFF) (band n 0xFF)))

(defn encode-uint32
  "Encode a 32-bit unsigned integer as 4 bytes."
  [n]
  (encode-int32 (if (> n math/int32-max) (- n u32-range) n)))

(defn- to-u32 [v] (if (< v 0) (+ v u32-range) v))
(defn- to-s32 [v] (if (>= v u32-half) (- v u32-range) v))

(defn encode-int64
  "Encode a 64-bit integer as 8 bytes in network byte order."
  [n]
  (let [abs-n (math/abs n)]
    (when (>= abs-n 9007199254740992)
      (error (string "can only encode ints smaller than 2^53: " n)))
    (var hi (math/floor (/ abs-n u32-range)))
    (var lo (- abs-n (* hi u32-range)))
    (when (< n 0)
      (def carry (if (= lo 0) 1 0))
      (set lo (+ (bxor (to-s32 lo) -1) 1))
      (set hi (+ (bxor hi -1) carry)))
    (string (encode-uint32 hi) (encode-uint32 lo))))

(defn encode-float32 [f]
  (encode-int32 (float32-raw f)))

(defn encode-float64 [f]
  (let [[h l] (float64-raw f)] (string (encode-int32 h) (encode-uint32 l))))

(defn encode-string [s] (pad4 s))
(defn encode-string-utf8 [s] (pad4 s))

(defn encode-blob [b]
  (let [len (length b) pad (mod (- 4 (mod len 4)) 4)]
    (string (encode-int32 len) b (nulls pad))))

(defn- pack4 [a b c d]
  (string/from-bytes (band a 0xFF)
                     (band b 0xFF)
                     (band c 0xFF)
                     (band d 0xFF)))

(defn encode-char [c]
  (encode-int32 (if (string? c) (in c 0) (band c 0xFF))))

(defn encode-rgba [r g b a]
  (pack4 r g b a))

(defn encode-midi [p s d1 d2]
  (pack4 p s d1 d2))


##  # #  #   #  #  #  #     #
# Element decoding
# #  #     #  #    #

(def- NULL (string/from-bytes 0))

(def- osc-header
  (peg/compile
    ~{:addr (any (if-not ,NULL 1))
      :tags (any (if-not ,NULL 1))
      :main (* (capture :addr) (drop (any ,NULL)) (capture :tags))}))

(defn decode-uint32
  "Decode 4 bytes starting at pos into a 32-bit unsigned integer."
  [data pos]
  (+ (blshift (in data pos) 24)
     (blshift (in data (+ pos 1)) 16)
     (blshift (in data (+ pos 2)) 8)
     (in data (+ pos 3))))

(defn decode-int32 [data pos]
  (let [v (decode-uint32 data pos)]
    (if (>= v u32-half) (- v u32-range) v)))

(defn decode-uint64 [data pos]
  (let [hi (to-u32 (decode-uint32 data pos))
        lo (to-u32 (decode-uint32 data (+ pos 4)))]
    (+ (* hi u32-range) lo)))

(defn decode-int64 [data pos]
  (let [hi (decode-uint32 data pos)
        lo (to-u32 (decode-uint32 data (+ pos 4)))]
    (+ (* hi u32-range) lo)))

(defn decode-float32 [data pos]
  (raw-to-float32 (decode-uint32 data pos)))

(defn decode-float64 [data pos]
  (raw-to-float64 (decode-uint32 data pos) (decode-uint32 data (+ pos 4))))

(defn decode-string [data pos]
  (var end pos)
  (while (and (< end (length data)) (not= (in data end) 0)) (++ end))
  (string/slice data pos end))

(defn decode-string-utf8 [data pos]
  (decode-string data pos))

(defn decode-blob [data pos]
  (let [size (decode-int32 data pos) start (+ pos 4)]
    [(string/slice data start (+ start size)) (+ start (padded-length size))]))

(defn- tuple4 [data pos]
  [(in data pos) (in data (+ pos 1)) (in data (+ pos 2)) (in data (+ pos 3))])

(defn decode-char [data pos]
  (band (decode-int32 data pos) 0xFF))

(defn decode-rgba [data pos]
  (tuple4 data pos))

(defn decode-midi [data pos]
  (tuple4 data pos))


# ## # #   # #    #    #
# Timetags
# ## #  # #   #     #

(def OSC-NOW (string/from-bytes 0 0 0 0 0 0 0 1))

(defn encode-timetag
  "Encode a timetag as 8 bytes. Use :now for immediate execution."
  [&opt secs frac]
  (if (and (= secs :now) (nil? frac)) OSC-NOW
    (string (encode-uint32 (or secs 0)) (encode-uint32 (or frac 0)))))

(defn decode-timetag [data pos]
  [(decode-int32 data pos) (decode-int32 data (+ pos 4))])


# # #   #  #      #   #    #    #
# Argument dispatch
# # ## #   #  #        #

(defn- ascii?
  "Return true is s is an ASCII string, otherwise false."
  [s]
  (var ok true)
  (each b s (when (>= b 128)
              (set ok false) (break)))
  ok)

(defn- decode-args
  "Decode tagged arguments from data into an array, advancing pos."
  [tags data start]
  (var pos start)
  (def args @[])
  (each tag tags
    (case tag
      # numbers
      TAG-i (do (array/push args (decode-int32 data pos)) (+= pos 4))
      TAG-f (do (array/push args (decode-float32 data pos)) (+= pos 4))
      TAG-h (do (array/push args (decode-int64 data pos)) (+= pos 8))
      TAG-d (do (array/push args (decode-float64 data pos)) (+= pos 8))
      # strings
      TAG-s (let [s (decode-string data pos)]
              (array/push args s) (set pos (padded-length (+ pos (length s) 1))))
      TAG-S (let [s (decode-string data pos)]
              (array/push args s) (set pos (padded-length (+ pos (length s) 1))))
      # The BLOB
      TAG-b (let [[v next] (decode-blob data pos)] (array/push args v) (set pos next))
      # timetag
      TAG-t (do (array/push args (decode-timetag data pos)) (+= pos 8))
      # boolean & flags
      TAG-T (array/push args true)
      TAG-F (array/push args false)
      TAG-N (array/push args nil)
      TAG-I (array/push args :impulse)
      # characters, colors, and MIDI
      TAG-c (do (array/push args (decode-char data pos)) (+= pos 4))
      TAG-r (do (array/push args (decode-rgba data pos)) (+= pos 4))
      TAG-m (do (array/push args (decode-midi data pos)) (+= pos 4))
      (error (string "unknown OSC type tag: " (string tag)))))
  args)

(defn- pack-arg
  "Pack a Janet value as OSC [typetag encoded-bytes]."
  [x]
  (cond
    (= (type x) :number)
      (if (= x (math/trunc x))
        (if (<= math/int32-min x math/int32-max)
          [TAG-i (encode-uint32 x)]
          (if (<= (math/abs x) 9007199254740991)
            [TAG-h (encode-int64 x)]
            [TAG-d (encode-float64 x)]))
        (if (<= float32-min x float32-max)
          [TAG-f (encode-float32 x)]
          [TAG-d (encode-float64 x)]))
    (= (type x) :string)
      (if (ascii? x)
        [TAG-s (encode-string x)]
        [TAG-S (encode-string x)])
    (= x true) [TAG-T ""]
    (= x false) [TAG-F ""]
    (nil? x) [TAG-N ""]
    (= x :impulse) [TAG-I ""]
    (= (type x) :buffer) [TAG-b (encode-blob x)]
    (error (string "unsupported OSC arg: " (describe x)))))


# # ## # #   # #    #    #
# Message encoding / decoding
# # ## # #   # #    #    #

(defn encode-message
  "Encode an address pattern and arguments into an OSC message."
  [address & args]
  (let [tags (buffer/new 0)
        data (buffer/new 0)]
  (each arg args
    (let [[t b] (pack-arg arg)]
      (buffer/push-byte tags t)
      (buffer/push data b)))
  (string (encode-string address) (encode-string (string "," tags)) data)))

(defn decode-message
  "Decode an OSC message into {:address addr :args [arg...]}."
  [data]
  (let [m (peg/match osc-header data)]
    (unless m (error "invalid OSC message"))
    (let [[addr-str tags-str] m
          data-pos (+ (padded-length (inc (length addr-str)))
                      (padded-length (inc (length tags-str))))]
      {:address addr-str :args (decode-args (string/slice tags-str 1) data data-pos)})))


# # # # #     # #  #   #      #    #           #
# Bundle encoding / decoding
# # ## # #   #     #    #    #

(def BUNDLE-TAG (pad4 "#bundle"))

(defn encode-bundle
  "Encode messages into an OSC bundle with the given timetag."
  [timetag & msgs]
  (let [tag (if (= timetag :now) OSC-NOW (apply encode-timetag timetag))
        content @""]
    (each msg msgs
      (buffer/push content (encode-int32 (length msg)))
      (buffer/push content msg))
    (string BUNDLE-TAG tag content)))

(defn decode-bundle
  "Decode an OSC bundle into {:timetag [sec frac] :elements [...]}."
  [data]
  (unless (= (string/slice data 0 8) BUNDLE-TAG) (error "not an OSC bundle"))
  (let [tt (decode-timetag data 8)
        elements @[]]
    (var pos 16)
    (while (< pos (length data))
      (let [size (decode-int32 data pos) start (+ pos 4)
            elt (string/slice data start (+ start size))
            inner (if (= (string/slice elt 0 8) BUNDLE-TAG)
                    (decode-bundle elt) (decode-message elt))]
        (array/push elements inner)
        (set pos (+ start size))))
    {:timetag tt :elements elements}))


## #   #   #    # #   #  #   #
# Utilities
# #   #   #     #   #

(defn message->string [msg]
  "Convert an OSC message to a string."
  (string (msg :address) " "
          (string/join (map |(string/format "%q" $0) (msg :args)) " ")))

(defn osc-send
  "Send an OSC message to host:port via UDP."
  [host port address & args]
  (let [socket (net/connect host port :datagram)
        msg (apply encode-message address args)]
    (:write socket msg)
    (:close socket)))

(defn osc-receive
  "Receive an OSC message on the given UDP port."
  [port]
  (let [socket (net/listen "0.0.0.0" port :datagram)
        buffer @""]
    (:recv-from socket 65536 buffer)
    (:close socket)
    (decode-message buffer)))
