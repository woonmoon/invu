(ns invu.core
  (:require [clojure.set :as set]
            [invu.util :as util]
            [invu.player :as players]
            [invu.logger :as log]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:gen-class))

(defonce new-state
  (atom { :active-players {}
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

(defn init-state [state num-steps num-ticks]
  (let [num-entries (inc num-steps)
        bridge (zipmap 
                  (range num-entries)
                  (take num-entries (repeat #{})))
        tempered-steps (into (sorted-map)
                          (zipmap
                            (range 1 num-entries)
                            (take num-steps (repeatedly #(rand-int 2)))))]
    (swap! state assoc :active-players bridge)
    (swap! state assoc :tempered-steps tempered-steps)
    (swap! state assoc :timer num-ticks)))

(defn spawn-players [state num-players]
  (let [players 
          (into #{}
            (repeatedly num-players 
              #(players/->Random 
                  (gensym "Player") 
                  (atom 0) 
                  (atom (rand)) 
                  (atom (rand))
                  (atom (rand)) 
                  (atom nil))))]
    (swap! state assoc-in [:active-players 0] players)))

(defn maybe-jump [state]
  "I volunteer as tribute!"
  (let [candidates (get-in state [:active-players 0])
        tributes (into (sorted-map) 
                    (remove nil?
                      (map 
                        #(players/will-move % (:common-knowledge state) (:common-cooperation state)) 
                        candidates)))]
    (if (or (empty? tributes) (seq (get-in state [:active-players 1])))
      state
      (let [chosen-one (second (first tributes))
            disjoint-state (update-in state [:active-players 0] disj chosen-one)
            moved-state (assoc-in disjoint-state [:active-players 1] #{chosen-one})]
        (players/move chosen-one (:common-knowledge state))
        (update moved-state :moves-made inc)))))

(defn next-step-available [location bridge]
  "Check if the next step of the bridge is occupied or not."
  (let [next-step (inc location)]
    (when (and (empty? (get @bridge next-step)) (<= next-step (count @bridge)))
      next-step)))

(defn maybe-move [state]
  (let [platform (get-in state [:active-players 0])
        bridge (atom (into (sorted-map-by >) (dissoc (:active-players state) 0)))
        common-knowledge (:common-knowledge state)
        common-cooperation (:common-cooperation state)]
    (doseq [[step players] @bridge]
      (when-let [player (first players)]
        (when-let [next-step (next-step-available step bridge)]
          (when-let [player-move (players/will-move player common-knowledge common-cooperation)]
            (players/move player common-knowledge)
            (swap! bridge update step disj player)
            (swap! bridge update next-step conj player)))))
    (assoc state :active-players (merge {0 platform} (into (sorted-map) @bridge)))))

(defn find-leading-step [active-players]
  (first 
    (first 
      (filter #(not (empty? (second %))) (reverse active-players)))))

(defn kill [state step player]
  (swap! state update-in [:active-players step] disj player)
  (swap! state update-in [:dead-players] conj player))

(defn survive [state player]
  (swap! state update-in [:active-players (first (last (:tempered-steps @state)))] disj player)
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
        num-active-players (util/count-active-players (:active-players @state))
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
  (let [leading-step (find-leading-step (into (sorted-map) (:active-players @state)))
        leading-player (first (get-in @state [:active-players leading-step]))
        correct-step (get-in @state [:tempered-steps leading-step])
        common-knowledge (:common-knowledge @state)]
    (when (not (no-new-knowledge state leading-step))
      (if (= leading-step (first (last (:tempered-steps @state))))
        (end-of-bridge state leading-player)
        (when (not= correct-step @(:decision leading-player))
          (kill state leading-step leading-player)))
      [leading-step correct-step])))

(defn end-of-tick [state]
  (when-let [[step knowledge] (new-knowledge state)]
    (swap! state assoc-in [:common-knowledge step] knowledge)
    (swap! state update :common-knowledge #(into (sorted-map) %)))
  (let [active-players (apply set/union (vals (:active-players @state)))
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
              (not (empty? (apply set/union (vals (:active-players @state))))))
    (tick new-state)))

(defn -main [& args]
  (let [config (edn/read-string (slurp "config.edn"))]
    (init-state new-state (:num-steps config) (:num-ticks config))
    (spawn-players new-state (:num-players config))
    (log/log :log-state new-state)
    (start-simulation new-state)
    (shutdown-agents))
)