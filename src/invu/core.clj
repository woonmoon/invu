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

; You jump when
; 1) You are the last person - survival guaranteed.
; 2) You are already willing to jump, the next step is free and.
(defn tile-available? [step final-step next-player]
  "Checks if the next tile is unoccupied or there are no more tiles."
  (or (= step final-step) (nil? next-player)))

(defn move-bridge [bridge moving-players tempered-tiles]
  "Returns a new bridge."
  (let [final-step (last (keys bridge))]
    (reduce-kv
      (fn [acc-bridge step player]
        (let [last-player 
                (second (last acc-bridge))
              tile-available 
                (tile-available? step final-step last-player)
              is-willing 
                (contains? moving-players player)
              has-survived 
                (= (rand-int 2) (get tempered-tiles step))]
          (cond
              (and is-willing tile-available has-survived)
                  (assoc acc-bridge (inc step) player step nil)
              (and is-willing tile-available (not has-survived))
                  (assoc acc-bridge (inc step) nil step nil)
              :else
                  (assoc acc-bridge step player))))
      (into (sorted-map-by >) {(inc final-step) nil})
      (into (sorted-map-by >) bridge))))

;; Platform -> Bridge
;; Platform -> Death
;; Bridge -> Death
;; Bridge -> Bridge
;; Bridge -> Survivor
;; Delta Bridge + Delta Death + Delta Survivor = Moves Made
(defn delta-bridge-moves [new-bridge old-bridge]
;; Delta (num of nils) -> died and survived
  )

(defn update-state [state moving-players tempered-tiles]
  (let [new-bridge (move-bridge (:bridge state) moving-players tempered-tiles)]
    ;; Check for survivors
    (when (val (last new-bridge))
      (let))
    ;; Check for jumpers (off the platform)
    ;; Check for who died
    ;; Check how many moves were made 
    ;; moves-made = delta deaths + delta survivors + delta changed positions
    ;; update ticks
    ;; update common cooperation
    ;; update chance of death
    ;; update jump misfortune
    ;; update common cooperation
))

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
      (players/update-aggression player delta-jump-misfortune))))

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