(ns invu.database
    (:require [clojure.set :as set]
              [clojure.data :as data]
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
        (/ (count (:common-knowledge final-state)) (:num-steps config))))

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