(ns invu.core
  (:import (javax.swing JPanel))
  (:require [invu.util :as util]
            [invu.player :as players]
            [invu.domain :as domain])
  (:gen-class))

(defonce tempered-steps [0 1 0 1 0 0])

(defonce players-state
  (atom { :platform-players {}
          :bridge (into (sorted-map) (domain/init-bridge 6))
          :dead-players {}
          :survivors {}
          :common-knowledge []}))

(defn spawn-players [state num-players]
  (let [names (take num-players (repeatedly #(gensym "Player")))
        players (into [] (map (partial players/init-player) names))
        active-players (zipmap names players)]
    ;; NOTE: maybe a bit dodge? idk if it's guaranteed that names and players are 
    ;;       correctly ordered at this point.
    (swap! state assoc :platform-players active-players)))

(defn maybe-jump [state]
  "I volunteer as tribute!"
  (let [candidates (:platform-players @state)
        tributes (remove nil? (into [] (map players/decide-jump (vals candidates))))]
    (when (not (empty? tributes))
      (let [chosen-one-name (rand-nth tributes)
            chosen-one (get candidates chosen-one-name)
            player-move (if (zero? (players/jump chosen-one (:common-knowledge @state))) :right :left)]
            (swap! state dissoc :platform-players chosen-one-name)
            (swap! (get-in @state [:bridge 1 player-move]) assoc chosen-one-name chosen-one)))))

(defn can-jump [position state]
  "Check if the next step of the bridge is occupied or not."
  (let [next-step (get-in @state [:bridge (inc position)])]
    (if (and (empty? @(:left next-step)) (empty? @(:right next-step)))
      true
      false)))

(defn find-players [step]
  "If the step is occupied return the map of occupants, if none return nil"
  (cond 
    (not (empty? @(:left step))) (:left step)
    (not (empty? @(:right step))) (:right step)
    :default nil))

; TODO: clean this mess up.
(defn maybe-move [state]
  (doseq [[num-step step] (reverse (:bridge @state))
          :let [location (find-players step)]
          :when (not (nil? location))
          ;; This only works because max no. of players on a step is 1.
          :let [[player-id player] (first @location)]]
    (when (can-jump num-step state)
      (let [player-move (players/move player (:common-knowledge @state))]
        (when (not (nil? player-move))
          (let [player-dir (if (zero? player-move) :right :left)]
            (swap! location dissoc player-id)
            (swap! (get-in @state [:bridge (inc num-step) player-dir]) assoc player-id player)))))))

(defn eliminate [player location state]
  (swap! location dissoc (:id player))
  (swap! state assoc-in [:dead-players (:id player)] player))

(defn end-of-tick [state yellowbrick-rd]
  (doseq [[num-step step] (:bridge @state)
          :let [location (find-players step)]
          :when (not (nil? location))
          ;; This only works because max no. of players on a location is 1.
          :let [[_ player] (first @location)]
          :let [path (:path-travelled player)]]
    (when (not= (nth yellowbrick-rd (dec (count @path))) (last @path))
      (eliminate player location state))))


(defn -main []
  (spawn-players players-state 10)
  ; Do this part under dosync
  (maybe-jump players-state)
  (maybe-move players-state)
  (end-of-tick players-state tempered-steps)
  (shutdown-agents))
