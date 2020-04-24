; Licensed to the Apache Software Foundation (ASF) under one
; or more contributor license agreements. See the NOTICE file
; distributed with this work for additional information
; regarding copyright ownership. The ASF licenses this file
; to you under the Apache License, Version 2.0 (the
; "License"); you may not use this file except in compliance
; with the License. You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.
(ns dda.provision.pallet
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as logging]
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :refer [instrument]]
   [orchestra.core :refer [defn-spec]]
   [dda.provision :as p]
   [pallet.actions :as actions]
   [selmer.parser :as selmer]))

(defn-spec
  ^{:private true}
  copy-resources-to-path any?
  [user ::p/user
   module-path string?
   sub-module ::p/sub-module
   files ::p/files]
  (let [all-module-path (str module-path "/" sub-module)]
    (actions/directory
     module-path
     :group user
     :owner user)
    (actions/directory
     all-module-path
     :group user
     :owner user)
    (doseq [resource files]
      (let [template? (contains? resource ::p/config)
            filename (::p/filename resource)
            filename-on-target (str all-module-path "/" filename)
            filename-on-source (if template?
                                 (str sub-module "/" filename ".template")
                                 (str sub-module "/" filename))
            config (if template?
                     (::p/config resource)
                     {})
            mode (cond
                   (contains? resource ::p/mode) (::p/mode resource)
                   (string/ends-with? filename ".sh") "700"
                   :default "600")]
        (actions/remote-file
         filename-on-target
         :literal true
         :group user
         :owner user
         :mode mode
         :content (selmer/render-file filename-on-source config))))))

(defn ^{:private true} user-path
  [user facility]
  (str "/home/" user "/resources/" facility))

(defn ^{:private true} tmp-path
  [facility]
  (str "/tmp/" facility))

(defmethod p/copy-resources-to-user ::pallet
  [provisioner user module sub-module files]
  (copy-resources-to-path user (user-path user module) sub-module files))
(s/fdef p/copy-resources-to-user
  :args (s/cat :provisioner ::p/provisioner
               :user ::p/user
               :module ::p/module
               :sub-module ::p/sub-module
               :files ::p/files))

(defmethod p/exec-as-user ::pallet
  [provisioner user module sub-module filename]
  (let [module-path (user-path user module)
        all-module-path (str module-path "/" sub-module)]
    (actions/exec-checked-script
     (str "execute " sub-module "/" filename)
     ("cd" ~all-module-path)
     ("sudo" "-H" "-u" ~user "bash" "-c" ~(str "./" filename)))))
(s/fdef p/exec-as-user
  :args (s/cat :provisioner ::p/provisioner
               :user ::p/user
               :module ::p/module
               :sub-module ::p/sub-module
               :filename ::p/filename))

(defmethod p/copy-resources-to-tmp ::pallet
  [provisioner module sub-module files]
  (copy-resources-to-path "root" (tmp-path module) sub-module files))
(s/fdef p/copy-resources-to-tmp
  :args (s/cat :provisioner ::p/provisioner
               :module ::p/module
               :sub-module ::p/sub-module
               :files ::p/files))

(defmethod p/exec-as-root ::pallet
  [provisioner module sub-module filename]
  (let [module-path (tmp-path sub-module)
        all-module-path (str module-path "/" sub-module)]
    (actions/exec-checked-script
     (str "execute " sub-module "/" filename)
     ("cd" ~all-module-path)
     ("bash" ~filename))))
(s/fdef p/exec-as-root
  :args (s/cat :provisioner ::p/provisioner
               :module ::p/module
               :sub-module ::p/sub-module
               :filename ::p/filename))

(defmethod p/provision-log ::pallet
  [provisioner module sub-module log-level log-message]
  (actions/as-action (logging/log log-level (str module "/" sub-module " - " log-message))))
(s/fdef p/provision-log
  :args (s/cat :provisioner ::p/provisioner
               :module ::p/module
               :sub-module ::p/log-level
               :log-level ::p/log-level
               :log-message ::p/log-message))


(instrument `p/copy-resources-to-user)
(instrument `p/exec-as-user)
(instrument `p/copy-resources-to-tmp)
(instrument `p/exec-as-root)
(instrument `p/provision-log)
