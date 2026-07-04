(declare-project
  :name "oscule"
  :description "Open Sound Control (OSC) protocol."
  :author "nik gaffney <nik@fo.am>"
  :url "https://codeberg.org/zzkt/oscule"
  :license "GPL-3.0-or-later"
  :version "1.1.0"
  :source-paths ["oscule"]
  :test-paths ["test"])

(declare-source
  :source ["oscule/init.janet" "oscule/osc.janet"]
  :prefix "oscule")
