(ns invu.database
    (:require [clojure.set :as set]
              [clojure.data :as data]
              [clojure.edn :as edn])
    (:gen-class))

(defrecord GameStateRow
    [])

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