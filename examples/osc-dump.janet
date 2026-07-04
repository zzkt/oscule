# osc-dump: listen on a UDP port and print received OSC messages
# Usage: janet examples/osc-dump.janet [-p port]

(use oscule)

(var port 9009)
(def args (dyn :args))
(var i 2)
(while (args i)
  (if (= "-p" (args i))
    (set port (scan-number (args (++ i))))
    (print "unknown flag: " (args i)))
  (++ i))

(print "osc-dump listening on port " port " (Ctrl-C to stop)")
(def s (net/listen "0.0.0.0" port :datagram))
(while true
  (def buf @"")
  (def who (:recv-from s 65536 buf))
  (def msg (decode-message buf))
  (def [host sender-port] (net/address-unpack who))
  (print (string "from " host ":" sender-port "  " (dump-message msg))))
