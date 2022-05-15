(ns invu.logger
    (:require [clojure.set :as set])
    (:require [invu.util :as util])
    (:require [clojure.java.io :as io]))

(defprotocol Logger
    (init-writer [this name])
    (log [this message]))

(defrecord StateLogger [] Logger
    (init-writer [_ name]
        (io/writer name))

    (log [_ state]
        (with-open [wrtr (io/writer "log-state.txt" :append true)]
            (.write wrtr (util/fmtln "Tick:" (:tick @state)))
            (.write wrtr (util/fmtln "Active Players:" (count (apply set/union (vals (:active-players @state))))))
            (.write wrtr (util/fmtln "Bridge:")) 
            (let [bridge (into (sorted-map) (dissoc (:active-players @state) 0))]
                (doseq [[step players] bridge]
                    (.write wrtr (util/fmt "{" step ":" (into [] (map #(:id %) players)) "} "))))
            (.write wrtr (util/fmtln "Dead Players:" (count (:dead-players @state))))
            (.write wrtr (util/fmtln "Survivors:" (count (:survivors @state))))
            (.write wrtr (util/fmtln (into [] (map #(:id %) (:survivors @state)))))
            (.write wrtr (util/fmtln "Tempered Steps:" (:tempered-steps @state)))
            (.write wrtr (util/fmtln "Common Knowledge:" (:common-knowledge @state)))
            (.write wrtr (util/fmtln "Common Cooperation:" (:common-cooperation @state)))
            (.write wrtr (util/fmtln "Moves Made:" (:moves-made @state)))
            (.write wrtr (util/fmtln "Chance of Death:" (:chance-of-death @state))))))

;; (defn log-player [player]
;;     (println
;;         (:id player) ":"
;;         "will-to-live:" (format "%.3f" @(:will-to-live player)) 
;;         "aggression:" (format "%.3f" @(:aggression player)) 
;;         "cooperation:" (format "%.3f" @(:cooperation player))))

;; (defn log-active-players [state]
;;     (println "Active players at tick" (:tick @state))
;;     (let [active-players (:active-players @state)]
;;         (doseq [[step players] active-players
;;                 player players]
;;             (log-player player)))
;;     (newline))