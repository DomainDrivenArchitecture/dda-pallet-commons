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

(ns dda.pallet.commons.existing-test
  (:require
    [clojure.test :refer :all]
    [pallet.compute :refer [nodes]]
    [dda.pallet.commons.existing :as sut]))

(def node-port 2222)

(def existing-nodes
  {:existing [{:node-name "node"
               :node-ip "localhost"}]})

(def existing-nodes-with-port
  (assoc-in existing-nodes [:existing 0 :node-port] node-port))

(defn extract-ports-from-node-list [node-list]
  (map :ssh-port (nodes node-list)))

(deftest test-provider
  (testing
    "test using existing node with default ssh-port"
      (is (= [22] (extract-ports-from-node-list (sut/provider "localhost" "id" "group-name"))))
      (is (= [22] (extract-ports-from-node-list (sut/provider existing-nodes)))))
  (testing
    "test using existing node with custom ssh-port"
    (is (= [node-port] (extract-ports-from-node-list (sut/provider "localhost" "id" "gruop-name" node-port))))
    (is (= [node-port] (extract-ports-from-node-list (sut/provider existing-nodes-with-port))))))
