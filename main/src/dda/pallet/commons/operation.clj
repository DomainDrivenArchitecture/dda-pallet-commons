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
(ns dda.pallet.commons.operation
  (:require
    [schema.core :as s]
    [pallet.api :as api]
    [pallet.repl :as pr]))

(defn- session-out
  [summarize? session]
  (when summarize?
    (pr/session-summary session))
  session)

(defn- provision-user [group]
  (let [provision-user (get-in group [:image :login-user])]
    (cond
      (empty? provision-user) nil
      (string? provision-user) (api/make-user provision-user :no-sudo (= provision-user "root"))
      (map? provision-user) (let [login (-> provision-user :login)
                                  pwd (-> provision-user :password)]
                              (api/make-user login :password pwd :no-sudo (= login "root"))))))

(defn do-apply-configure
  "applies only the settings and configuration phase to a target.
function awaits the login user set in (-> group :image :login-user)."
  [provider group & options]
  (let [{:keys [summarize-session]
         :or {summarize-session true}} options
          user (provision-user group)]
    (session-out
      summarize-session
      (if (some? user)
        (api/lift
          group
          :compute provider
          :phase '(:settings :configure)
          :user (provision-user group))
        (api/lift
          group
          :compute provider
          :phase '(:settings :configure))))))

(defn do-apply-install
    "applies the settings, init, install and configuration to a target.
function awaits the login user set in (-> group :image :login-user)."
  [provider group & options]
  (let [{:keys [summarize-session]
         :or {summarize-session true}} options
         user (provision-user group)]
   (session-out
      summarize-session
      (if (some? user)
        (api/lift
          group
          :compute provider
          :phase '(:settings :init :install :configure)
          :user (provision-user group))
        (api/lift
          group
          :compute provider
          :phase '(:settings :init :install :configure))))))

(defn do-converge-install
    "Converges [count] nodes and applies the settings, init, install and configuration phase to a target.
function awaits the login user set in (-> group :image :login-user)."
  [provider group & options]
  (let [{:keys [summarize-session]
         :or {summarize-session true}} options
         user (provision-user group)]
   (session-out
     summarize-session
     (if (some? user)
       (api/converge
         group
         :compute provider
         :phase '(:settings :init :install :configure)
         :user (provision-user group))
       (api/converge
         group
         :compute provider
         :phase '(:settings :init :install :configure))))))

(defn do-app-rollout
    "app-rollout applies the settings and app-rollout phase to a target.
function awaits the login user set in (-> group :image :login-user)."
  [provider group & options]
  (let [{:keys [summarize-session]
         :or {summarize-session true}} options
         user (provision-user group)]
   (session-out
     summarize-session
     (if (some? user)
       (api/converge
         group
         :compute provider
         :phase '(:settings :app-rollout)
         :user (provision-user group))
       (api/converge
         group
         :compute provider
         :phase '(:settings :app-rollout))))))

(defn do-test
    "applies only the settings and test (without side effects by convention) phase of group to a target.
function awaits the login user set in (-> group :image :login-user)."
  [provider group & options]
  (let [{:keys [summarize-session]
         :or {summarize-session true}} options
         user (provision-user group)]
   (session-out
     summarize-session
     (if (some? user)
       (api/lift
         group
         :compute provider
         :phase '(:settings :test)
         :user (provision-user group))
       (api/lift
         group
         :compute provider
         :phase '(:settings :test))))))
