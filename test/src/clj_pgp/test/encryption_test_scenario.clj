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
(ns clj-pgp.test.encryption-test-scenario
  (:require
    [clojure.java.io :as io]
    [clojure.test :refer :all]
    [clojure.test.check.generators :as gen]
    [schema.core :as s]
    [byte-streams :refer [bytes=]]
    [clj-pgp.core :as pgp]
    [clj-pgp.generate :as pgp-gen]
    [clj-pgp.keyring :as keyring]
    [clj-pgp.tags :as tags]    
    [clj-pgp.message :as pgp-msg]
    [dda.pallet.commons.encrypted-credentials :as encrypted-credentials]))


(def pubring
  (-> "clj_pgp/test/keys/pubring.gpg"
      io/resource
      io/file
      keyring/load-public-keyring))

(def secring 
  (encrypted-credentials/load-secret-keyring
    (merge (encrypted-credentials/default-encryption-configuration)
           {:secring-path "clj_pgp/test/keys/secring.gpg"})))

(defn get-privkey
  [id]
  (some-> secring
          (keyring/get-secret-key id)
          (pgp/unlock-key "test password")))


(def master-pubkey (keyring/get-public-key pubring "923b1c1c4392318a"))

(def pubkey  (encrypted-credentials/get-secret-key
               {:user-home "/home/user/"
               :secring-path "clj_pgp/test/keys/secring.gpg"
               :key-id "3f40edec41c6cb7d"}))
(def seckey  (encrypted-credentials/get-secret-key
               {:user-home "/home/user/"
               :secring-path "clj_pgp/test/keys/secring.gpg"
               :key-id "3f40edec41c6cb7d"}))
(def privkey (pgp/unlock-key seckey "test password"))



;; ## Generative Utilities

(defn gen-rsa-keyspec
  "Returns a generator for RSA keys with the given strengths."
  [strengths]
  (gen/fmap
    (partial vector :rsa :rsa-general)
    (gen/elements strengths)))


(defn gen-ec-keyspec
  "Returns a generator for EC keys with the given algorithm and named curves."
  [algorithm curves]
  (gen/fmap
    (partial vector :ec algorithm)
    (gen/elements curves)))


(defn spec->keypair
  "Generates a keypair from a keyspec."
  [[key-type & opts]]
  (case key-type
    :rsa (let [[algo strength] opts
               rsa (pgp-gen/rsa-keypair-generator strength)]
           (pgp-gen/generate-keypair rsa algo))
    :ec (let [[algo curve] opts
              ec (pgp-gen/ec-keypair-generator curve)]
          (pgp-gen/generate-keypair ec algo))))

(def key-cache
  "Stores generated keys by their key-specs to memoize key generation calls."
  (atom {}))

(defn memospec->keypair
  "Returns a keypair for a keyspec. Uses the key-cache var to memoize the
  generated keys."
  [spec]
  (or (when (string? spec) spec)
      (get @key-cache spec)
      (let [k (spec->keypair spec)]
        (swap! key-cache assoc spec k)
        k)))

(defn generate-rsa-keypair []
  (let [rsa (pgp-gen/rsa-keypair-generator 1024)]
    (pgp-gen/generate-keypair rsa :rsa-general)))

(defn test-encryption-scenario
  "Tests that encrypting and decrypting data with the given keypairs/passphrases
  returns the original data."
  [keyspecs data compress cipher armor]
  (testing (str "Encrypting " (count data) " bytes with " cipher
                " for keys " (pr-str keyspecs)
                (when compress (str " compressed with " compress))
                " encoded in " (if armor "ascii" "binary"))
    (let [encryptors (map memospec->keypair keyspecs)
          ciphertext (pgp-msg/encrypt
                       data encryptors
                       :compress compress
                       :cipher cipher
                       :armor armor)]
      (is (not (bytes= data ciphertext))
        "ciphertext bytes differ from data")
      (doseq [decryptor encryptors]
        (is (bytes= data (pgp-msg/decrypt ciphertext decryptor))
            "decrypting the ciphertext returns plaintext"))
      [encryptors ciphertext])))
