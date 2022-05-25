(ns invu.core
  (:require [clojure.set :as set]
            [clojure.data :as data]
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

;; BREAKING
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

(defn move-bridge [bridge moving-players tempered-tiles]
  "Returns a new bridge."
  (let [final-step (last (keys bridge))]
    (reduce-kv
      (fn [acc-bridge step player]
        (let [last-player 
                (second (last acc-bridge))
              tile-available 
                (or (= step final-step) (nil? last-player))
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

(defn moves-made [old-bridge new-bridge]
  "Returns the number of moves made in the tick."
;; NOTE(woonmoon): This works because each player maps onto a step and if
;; no moves or made that player-tile mapping stays the same. The only
;; reason the mapping would have changed is if the player moved.
  (count 
    (into #{}
      (flatten
        (map 
          keys
          (drop-last
            (data/diff 
              (dissoc (set/map-invert new-bridge) nil) 
              (dissoc (set/map-invert old-bridge) nil))))))))

(defn deceased-players [old-bridge new-bridge]
  (remove nil?
    (set/difference 
      (into #{} (vals new-bridge)) 
      (into #{} (vals old-bridge)))))

;; update-threshold = moves-made / ticks
;; if update-threshold > 0.5:
;;    common-cooperation = common-cooperation + 0.2 * (1 - common-cooperation)
;; else:
;;    common-cooperation = common-cooperation - 0.2 * common-cooperation
(defn new-common-cooperation [old-common-cooperation moves-made current-tick]
  (let [update-threshold (util/one-div moves-made current-tick)]
    (util/reinforce-value old-common-cooperation update-threshold 0.2 0.5)))

;; Chance of certain death [0, 1]
;; time-left = time-total - current-tick
;; ==> chance-of-death = (active-players - ticks-left) / active-players
;; if (active-players - ticks-left) = 0:
;; ==> chance-of-death = 0
;; Implement a panic that kicks in at consecutive number of delta increasing.
(defn new-chance-of-death [old-chance-of-death time-left num-active-players]
  (let [players-to-die (- num-active-players time-left)]
    (if (neg? players-to-die) 
      0 
      (util/one-div players-to-die num-active-players))))

(defn new-jump-misfortune [num-deaths moves-made]
  (util/zero-div num-deaths moves-made))

(defn update-state [state moving-players tempered-tiles total-time]
  ;; Check how many moves were made 
  (let [platform (:platform state)
        bridge (:bridge state)
        current-tick (:tick state)
        old-common-cooperation (:common-cooperation state)
        old-chance-of-death (:chance-of-death state)
        new-bridge (into (sorted-map) (move-bridge bridge moving-players tempered-tiles))
        moves-made (+ (moves-made bridge new-bridge) (:moves-made state))
        time-left (- total-time current-tick)
        ;; who died this tick
        dead-players (deceased-players bridge new-bridge)
        num-active-players (+ (count platform) (remove nil? (vals new-bridge)))
        ;; update common cooperation
        new-common-cooperation (new-common-cooperation old-common-cooperation moves-made current-tick)
        ;; update chance of death
        new-chance-of-death (new-chance-of-death old-chance-of-death time-left num-active-players)
        ;; update jump misfortune
        new-jump-misfortune (new-jump-misfortune (count dead-players) moves-made)
        ;; survivors 
        survivors (val (last new-bridge))
        ;; jumped off the platform
        jumpers (most-willing-player platform moving-players)]
    ))

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