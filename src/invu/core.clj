(ns invu.core
  (:require [clojure.set :as set]
            [invu.util :as util]
            [invu.player :as players]
            [invu.logger :as log]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:gen-class))

(defonce new-state
  (atom { :platform #{}
          :bridge {}
          :dead-players #{} 
          :survivors #{}
          :common-knowledge {}
          :timer nil
          :tick 0 
          :moves-made 0
          :chance-of-death 0
          :jump-misfortune 0
          :common-cooperation 0.75
          :tempered-steps {} 
        }))

(defrecord State
  [
    platform 
    bridge
    dead-players
    survivors
    common-knowledge 
    tick 
    moves-made 
    chance-of-death 
    jump-misfortune 
    common-cooperation
  ])

(defn spawn-players [num-players]
  (repeatedly num-players 
    #(players/->Random 
        (gensym "Player") 
        (atom 0) 
        (atom (rand)) 
        (atom (rand))
        (atom (rand)) 
        (atom nil))))

(defn id-to-players [num-players]
  (let [players (spawn-players num-players)]
    (zipmap (map #(:id %) players) players)))

(defn init-state [state num-steps num-ticks id-to-players]
  (let [num-entries (inc num-steps)
        bridge (zipmap 
                  (range 1 num-entries)
                  (take num-steps (repeat nil)))
        tempered-steps (into (sorted-map)
                          (zipmap
                            (range 1 num-entries)
                            (take num-steps (repeatedly #(rand-int 2)))))
        players (keys id-to-players)]
    (->State players bridge #{} #{} {} 0 0 0 0 0.7)))

(defn player-to-will-to-move [active-ids id-to-players common-knowledge common-cooperation]
  "Returns a map of ids and desires of players who are willing to move"
  (let [active-players (vals (select-keys id-to-players active-ids))]
    (into 
      {}
      (map 
        #(players/will-move % common-knowledge common-cooperation) 
        active-players))))


(defn shift-players [players-on-bridge moving-players last-player]
  "Given a flat list of players on the bridge (empty tiles represented by nil)
  shifts willing players up by one step (player at the end always moves).
  The last element if non-nil represents survivors"
  (reverse
    (reduce
      (fn [players player]
        (if (and 
              (some? player) 
              (nil? (last players)) 
              (or (contains? moving-players player) (= player last-player)))
          (conj (pop players) player nil)
          (conj players player)))
      [nil]
      players-on-bridge)))

(defn most-willing-player [platform moving-players]
  "If exists returns the most willing player to jump off platform 
  else returns nil"
  (let [willing-players 
          (into 
            {} 
            (filter (comp some? val) (select-keys moving-players platform)))]
    (when (seq willing-players)
      (key (apply max-key willing-players)))))

(defn update-platform [platform jumping-player]
  "Given the player that will jump returns the updated platform."
  (disj platform jumping-player))

(defn update-state)

(defn find-leading-step [bridge]
  (if (empty? (remove nil? (vals bridge)))
    0
    (first 
      (first 
        (filter #(some? (second %)) (reverse bridge))))))

(defn kill [state step player]
  (swap! state assoc-in [:bridge step] nil)
  (swap! state update-in [:dead-players] conj player))

(defn survive [state player]
  (swap! state assoc-in [:bridge (first (last (:tempered-steps @state)))] nil)
  (swap! state update-in [:survivors] conj player))

;; update-threshold = moves-made / ticks
;; if update-threshold > 0.5:
;;    common-cooperation = common-cooperation + 0.2 * (1 - common-cooperation)
;; else:
;;    common-cooperation = common-cooperation - 0.2 * common-cooperation
(defn delta-common-cooperation [state]
  (let [old-common-coop (:common-cooperation @state)
        update-threshold (util/one-div (:moves-made @state) (:tick @state))
        new-common-coop (util/reinforce-value old-common-coop update-threshold 0.2 0.5)]
    (swap! state assoc :common-cooperation new-common-coop)
    (- old-common-coop new-common-coop)))

;; Chance of certain death [0, 1]
;; ==> chance-of-death = (active-players - ticks-left) / active-players
;; if (active-players - ticks-left) = 0:
;; ==> chance-of-death = 0
;; Implement a panic that kicks in at consecutive number of delta increasing.
(defn delta-chance-of-death [state]
  (let [old-chance-of-death (:chance-of-death @state)
        time-left (- (:timer @state) (:tick @state))
        num-active-players (count (util/get-active-players state))
        players-to-die (- num-active-players time-left)
        new-chance-of-death 
          (if (neg? players-to-die) 
            0 
            (util/one-div players-to-die num-active-players))]
    (swap! state assoc :chance-of-death new-chance-of-death)
    (- old-chance-of-death new-chance-of-death)))

(defn delta-jump-misfortune [state]
  (let [old-jump-misfortune (:jump-misfortune @state)
        new-jump-misfortune (util/zero-div (count (:dead-players @state)) (:moves-made @state))]
    (swap! state assoc :jump-misfortune new-jump-misfortune)
    (- old-jump-misfortune new-jump-misfortune)))

(defn end-of-bridge [state player]
  (let [[last-step correct-last-step] (last (:tempered-steps @state))]
    (if (= @(:decision player) correct-last-step)
      (survive state player)
      (kill state last-step player))))

(defn no-new-knowledge [state leading-step]
  (or (contains? (:common-knowledge @state) leading-step) 
      (= 0 leading-step)))

(defn new-knowledge [state]
  (if (empty? (remove nil? (vals (:bridge @state))))
    ;; Nobody on the bridge
    nil 
    (let [leading-step (find-leading-step (into (sorted-map) (:bridge @state)))
          leading-player (get-in @state [:bridge leading-step])
          correct-step (get-in @state [:tempered-steps leading-step])]
      (if (contains? (:common-knowledge @state) leading-step)
        ;; Leading player has not moved
        nil
        (if (= leading-step (first (last (:tempered-steps @state))))
          ;; Someone has reached the end of the bridge
          (end-of-bridge state leading-player)
          (do
            (when (not= correct-step @(:decision leading-player))
              (kill state leading-step leading-player))
            [leading-step correct-step]))))))

(defn end-of-tick [state]
  (when-let [[step knowledge] (new-knowledge state)]
    (swap! state assoc-in [:common-knowledge step] knowledge)
    (swap! state update :common-knowledge #(into (sorted-map) %)))
  (let [active-players (util/get-active-players state)
        delta-common-cooperation (delta-common-cooperation state)
        delta-chance-of-death (delta-chance-of-death state)
        delta-jump-misfortune (delta-jump-misfortune state)]
    (doseq [player active-players]
      (players/update-cooperation player delta-common-cooperation)
      (players/update-will-to-live player delta-chance-of-death)
      (players/update-aggression player delta-jump-misfortune))
      ))

(defn tick [state]
  (swap! state update :tick inc)
  (swap! state maybe-move)
  (swap! state maybe-jump)
  (end-of-tick state)
  (log/log :log-state state)
  (log/log :log-players state))

(defn start-simulation [state]
  (while (and (< (:tick @state) (:timer @state))
              (not (empty? (util/get-active-players state))))
    (tick new-state)))

(defn -main [& args]
  (let [config (edn/read-string (slurp "config.edn"))]
    (init-state new-state (:num-steps config) (:num-ticks config))
    (spawn-players new-state (:num-players config))
    (log/log :log-state new-state)
    (start-simulation new-state)
    (shutdown-agents))
)