(ns invu.database
    (:require [clojure.set :as set]
              [clojure.data :as data]
              [clojure.java.io :as io]
              [clojure.string :as string]
              [clojure.edn :as edn])
    (:gen-class))

(defrecord GameStateRow
    [
        num-players
        num-steps
        num-ticks
        uncooperative-aggressive
        uncooperative-unaggressive
        cooperative-aggressive
        cooperative-unaggressive
        num-survivors
        knowledge-mined
    ])

(defrecord PlayerStateRow 
    [
        id 
        init-will-to-live 
        init-cooperation
        init-aggression
        final-will-to-live
        final-cooperation
        final-aggression
        status ; Dead or alive
        tick ; As of what tick.
    ])

(defn create-game-state-row [config final-state]
    (->GameStateRow
        (:num-players config)
        (:num-steps config)
        (:num-ticks config)
        (:uncooperative-aggressive config)
        (:uncooperative-unaggressive config)
        (:cooperative-aggressive config)
        (:cooperative-unaggressive config)
        (count (:survivors final-state))
        (double (/ (count (:common-knowledge final-state)) (:num-steps config)))))

(defn create-player-row [init-player [final-player [status tick]]]
    (->PlayerStateRow
        (:id init-player)
        (:will-to-live init-player)
        (:cooperation init-player)
        (:aggression init-player)
        (:will-to-live final-player)
        (:cooperation final-player)
        (:aggression final-player)
        status
        tick))
    
(defonce endl "\n")

(defn fmtln [& args]
    (str (string/join " " args) endl))

(defn log-game [output config-name experiment-id]
    (with-open [wrtr (io/writer "outputs/half_die/log_state" :append true)]
        (.write 
            wrtr 
            (fmtln 
                experiment-id
                (:num-survivors (:game-state output)) 
                (:knowledge-mined (:game-state output))))))

;; (defn log-mega [output config-name]
;;     (with-open [wrtr (io/writer "outputs/tryagain" :append true)]
;;         (.write
;;             wrtr
;;             (fmtln config-name (:num-survivors (:game-state output))))))

(defn log-players [output config-name experiment-id]
    (with-open [wrtr (io/writer "outputs/half_die/log_players" :append true)]
        (doseq [p (:player-state output)]
            (.write wrtr 
                (fmtln
                    experiment-id
                    (:id p) 
                    (:init-will-to-live p)
                    (:final-will-to-live p) 
                    (:init-cooperation p)
                    (:final-cooperation p)
                    (:init-aggression p)
                    (:final-aggression p)
                    (:status p)
                    (:tick p))))))