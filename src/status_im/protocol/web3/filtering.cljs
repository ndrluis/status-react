(ns status-im.protocol.web3.filtering
  (:require [status-im.protocol.web3.utils :as u]
            [cljs.spec :as s]
            [taoensso.timbre :refer-macros [debug]]))

(def status-topic "status-dapp-topic")
(defonce filters (atom {}))

(s/def ::options (s/keys :opt-un [:message/to :message/topics]))

(defn remove-filter! [web3 options]
  (when-let [filter (get-in @filters [web3 options])]
    (.stopWatching filter)
    (debug :stop-watching options)
    (swap! filters update web3 dissoc options)))

(defn add-filter!
  [web3 {:keys [topics to] :as options} callback]
  (remove-filter! web3 options)
  (debug :add-filter options)
  (let [shh (u/shh web3)
        encrypted? (boolean to)
        do-add-filter (fn []
                        (let [filter (.filter shh (clj->js options) callback)]
                          (swap! filters assoc-in [web3 options] filter)))]
    (if encrypted?
      (do-add-filter)
      (let [topic (first topics)]
        (.hasSymKey
          shh topic
          (fn [error res]
            (if-not res
              (.addSymKey
                shh topic (.toHex web3 "status-key-name")
                (fn [error res]
                  (when-not error (do-add-filter))))
              (do-add-filter))))))))

(defn remove-all-filters! []
  (doseq [[web3 filters] @filters]
    (doseq [options (keys filters)]
      (remove-filter! web3 options))))
