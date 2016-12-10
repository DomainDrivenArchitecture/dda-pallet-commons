(ns org.domaindrivenarchitecture.pallet.servertest.fact.netstat
  (:require
    [org.domaindrivenarchitecture.pallet.servertest.fact :refer :all]
    [org.domaindrivenarchitecture.pallet.servertest.tests :refer :all]
    [pallet.stevedore :refer :all]
    [pallet.script :as script]
    [pallet.script.lib :refer :all]))

(def res-id-netstat ::netstat)
(defn define-resources-netstat
  "Defines the netstat resource. 
   This is automatically done serverstate crate is used."
  []
  (define-session-resource-from-script res-id-netstat "netstat -tulpen"))