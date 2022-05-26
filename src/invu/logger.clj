(ns invu.logger
    (:require [clojure.set :as set]
              [clojure.string :as string]
              [clojure.java.io :as io]
              [invu.util :as util]))

(defonce endl "\n")

(defn fmt [& args]
    (string/join " " args))

(defn fmtln [& args]
    (str (string/join " " args) endl))

(defmulti log (fn [log-type & args] log-type))

(defmethod log :log-state [_ state]
    (with-open [wrtr (io/writer "log-state.txt" :append true)]
        (.write wrtr (fmtln "Tick:" (:tick state)))
        (.write wrtr (fmtln "Active Players:" (count (util/get-active-players state))))
        (.write wrtr (fmt "Bridge: ")) 
        (let [bridge (:bridge state)]
            (doseq [[step player] bridge]
                (.write wrtr (fmt "{" step ":" (:id player) "} "))))
        (.write wrtr endl)
        (.write wrtr (fmtln "Dead Players:" (count (:dead-players state))))
        (.write wrtr (fmtln "Survivors:" (count (:survivors state))))
        (.write wrtr (fmtln (into [] (map #(:id %) (:survivors state)))))
        (.write wrtr (fmtln "Tempered Steps:" (:tempered-steps state)))
        (.write wrtr (fmtln "Common Knowledge:" (:common-knowledge state)))
        (.write wrtr (fmtln "Common Cooperation:" (:common-cooperation state)))
        (.write wrtr (fmtln "Moves Made:" (:moves-made state)))
        (.write wrtr (fmtln "Chance of Death:" (:chance-of-death state)))
        (.write wrtr endl)))

(defmethod log :log-player [_ state]
    (with-open [wrtr (io/writer "log-players.txt" :append true)]
        (let [active-players (util/get-active-players state)]
            (doseq [[step players] active-players
                    player players]
                (.write wrtr
                    (fmtln
                        (:id player) ":"
                        "will-to-live:" (format "%.3f" (:will-to-live player)) 
                        "aggression:" (format "%.3f" (:aggression player)) 
                        "cooperation:" (format "%.3f" (:cooperation player))))))
        (doseq [player (:dead-players state)]
            (.write wrtr
                (fmtln
                    (:id player) ":"
                    "will-to-live:" (format "%.3f" (:will-to-live player)) 
                    "aggression:" (format "%.3f" (:aggression player)) 
                    "cooperation:" (format "%.3f" (:cooperation player)))))
        (.write wrtr endl)))

(defmethod log :test-jump 
    [_ player fuzzy-coop fuzzy-aggr coop-desire wtl-desire common-coop will-jump]
        (with-open [wrtr (io/writer "test-jumps.txt" :append true)]
            (.write wrtr 
                (fmtln 
                    (:id player)
                    (:cooperation player)
                    (:aggression player)
                    (:will-to-live player)
                    fuzzy-coop
                    fuzzy-aggr
                    coop-desire
                    wtl-desire
                    common-coop
                    will-jump))))

(defmethod log :log-players [_ state]
    (let [active-players (util/get-active-players state)]
        (with-open [wrtr (io/writer "log-players.txt" :append true)]
            (doseq [player active-players]
                (.write wrtr
                    (fmtln
                        (:id player)
                        (:location player)
                        (:will-to-live player)
                        (:aggression player)
                        (:cooperation player)))))))