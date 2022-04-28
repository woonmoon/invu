(ns invu.logger
    (:require [clojure.set :as set])
    (:require [invu.util :as util]))

(defn log-step [step players]
    (print "{" step ": " (into [] (map #(:id %) players)) "} "))

(defn log-bridge [state]
    (let [bridge (into (sorted-map) (dissoc (:active-players @state) 0))]
        (doseq [[step players] bridge]
            (log-step step players))))

(defn log-state [state] 
    (println "Tick: " (:tick @state))
    (println "Active Players: " (count (apply set/union (vals (:active-players @state)))))
    (do
        (print "Bridge: ") 
        (log-bridge state)
        (newline))
    (println "Dead Players: " (count (:dead-players @state)))
    (println "Survivors: " (count (:survivors @state)))
    (println "Common Knowledge: " (:common-knowledge @state))
    (newline))