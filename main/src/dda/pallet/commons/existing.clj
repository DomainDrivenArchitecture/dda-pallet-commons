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

(ns dda.pallet.commons.existing
  (:require
   [schema.core :as s]
   [pallet.compute.node-list :as node-list]
   [pallet.compute :as compute]
   [dda.pallet.commons.secret :as secret]
   [dda.pallet.commons.external-config :as ext-config]))

(def ExistingNode
  "Represents a target node with ip and its name."
  {:node-name s/Str
   :node-ip s/Str
   (s/optional-key :node-port) s/Int})

(def ExistingNodes
  "A sequence of ExistingNodes."
  {s/Keyword [ExistingNode]})

(def ProvisioningUser
  "User used for provisioning."
  {:login s/Str
   (s/optional-key :password) secret/Secret})

(def Targets
  "Targets to be used during provisioning."
  {:existing [ExistingNode]
   (s/optional-key :provisioning-user) ProvisioningUser})

(def ProvisioningUserResolved (secret/create-resolved-schema ProvisioningUser))

(def TargetsResolved (secret/create-resolved-schema Targets))

(s/defn resolve-targets :- TargetsResolved
  [targets :- Targets]
  (secret/resolve-secrets targets Targets))

(s/defn ^:always-validate
  load-targets :- Targets
  [file-name :- s/Str]
  (ext-config/parse-config file-name))

(s/defn ^:always-validate
  single-remote-node
  [group :- s/Keyword
   existing-node :- ExistingNode]
  (let [{:keys [node-name node-ip node-port] :or {node-port 22}} existing-node]
    (node-list/make-node
      node-name
      (name group)
      node-ip
      :ubuntu
      :ssh-port node-port)))

(s/defn ^:always-validate
  remote-node
  ([node-ip node-name group-name node-port]
   (node-list/make-node
    node-name
    group-name
    node-ip
    :ubuntu
    :ssh-port (or node-port 22)))
  ([node-ip node-name group-name]
   (remote-node node-ip node-name group-name nil))
  ([group :- s/Keyword
    existing-nodes :- [ExistingNode]]
   (map #(single-remote-node group %) existing-nodes)))

(s/defn ^:always-validate
  provider
  ([provisioning-ip :- s/Str
    node-id :- s/Str
    group-name  :- s/Str
    & [node-port] :- [s/Int]
    ]
   (compute/instantiate-provider
     "node-list"
     :node-list [(remote-node provisioning-ip node-id group-name node-port)]))
  ([existing-nodes :- ExistingNodes]
   (compute/instantiate-provider
      "node-list"
      :node-list
      (into
        []
        (flatten (map (fn [[k v]] (remote-node k v)) existing-nodes))))))

(defn node-spec [provisioning-user]
  {:image
   {:login-user provisioning-user}})
