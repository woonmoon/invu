(ns invu.core
  (:require [clojure.set :as set]
            [clojure.data :as data]
            [invu.util :as util]
            [invu.player :as players]
            [invu.logger :as log]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:gen-class))

(def foo {
  1 1
  2 0
  3 0
  4 0
  5 1
})

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
    #(players/->Random (gensym "Player") (rand) (rand) (rand))))

(defn id-to-players [num-players]
  (->> (spawn-players num-players)
       (map (fn [p] [(.id p) p]))
       (into {})))

(defn init-state [num-steps id-to-players]
  (let [num-entries (inc num-steps)
        bridge (zipmap 
                  (range 1 num-entries)
                  (take num-steps (repeat nil)))
        tempered-steps (into (sorted-map)
                          (zipmap
                            (range 1 num-entries)
                            (take num-steps (repeatedly #(rand-int 2)))))
        players (set (keys id-to-players))]
    (->State players bridge #{} #{} {} 0 0 0 0 0.7)))

(defn active-id->location [state]
  (merge
    (zipmap (:platform state) (repeat (count (:platform state)) 0))
    (dissoc (set/map-invert (:bridge state)) nil)))

(defn moving-players [active-id->location id->player common-knowledge common-cooperation final-step]
  "Returns a map of ids and desires of players who are willing to move"
  (let [active-ids (keys active-id->location)
        location #(get active-id->location (:id %))
        next-step-known? #(or (contains? common-knowledge (inc %)) (= final-step %))]
    (->> (select-keys id->player active-ids)
       vals
       (map 
          (fn [p] 
            (players/will-move p (next-step-known? (location p)) common-cooperation)))
       (filter (comp some? second))
       (into {}))))

(defn most-willing-player [platform moving-players]
  "Returns the most willing player to jump off platform if nobody, returns nil."
  (when-let [willing-players (seq (select-keys moving-players platform))]
    (->> willing-players
         (apply max-key val)
         (key))))

(defn move-on-bridge [bridge moving-players tempered-tiles common-knowledge]
  "Returns a tuple of [new-bridge, survivors] where new-bridge is sorted (ascending)"
  (let [final-step (last (keys bridge))
        survivor-step (inc final-step)
        tile-available? 
          #(or (= %1 final-step) (nil? %2))
        willing? 
          #(contains? moving-players %)
        survives? 
          #(or (= (rand-int 2) (get tempered-tiles %)) (contains? common-knowledge %))
        moves-forward-survives
          #(assoc %1 %2 %3 %4 nil)
        moves-forward-dies
          #(assoc %1 %2 nil %3 nil)
        does-not-move 
          #(assoc %1 %2 %3)]
    (into (sorted-map)
      (reduce-kv
        (fn [acc-bridge step player]
          (let [[last-step last-player] (last acc-bridge)]
            (cond
                (and  (willing? player) 
                      (tile-available? step last-player) 
                      (survives? step))
                  (moves-forward-survives acc-bridge last-step player step)
                (and  (willing? player) 
                      (tile-available? step last-player) 
                      (not (survives? step)))
                  (moves-forward-dies acc-bridge last-step step)
                :else
                  (does-not-move acc-bridge step player))))
        (into (sorted-map-by >) {survivor-step nil})
        (into (sorted-map-by >) bridge)))))

(defn moves-made [old-bridge new-bridge]
  "Returns the number of moves made in the tick."
;; NOTE(woonmoon): This works because each player maps onto a step and 
;; the only reason a mapping would have changed is if the player moved.
  (let [old-players (dissoc (set/map-invert old-bridge) nil)
        new-players (dissoc (set/map-invert new-bridge) nil)
        difference (data/diff old-players new-players)]
    (-> (map keys (drop-last difference))
        flatten
        count)))

(defn deceased-players [old-bridge new-bridge already-dead]
  (let [players #(-> % vals set (disj nil))]
    (if-let [dead (seq (first (data/diff (players old-bridge) (players new-bridge))))]
      (set (concat (seq already-dead) dead))
      already-dead)))

;; update-threshold = moves-made / ticks
;; if update-threshold > 0.5:
;;    common-cooperation = common-cooperation + 0.2 * (1 - common-cooperation)
;; else:
;;    common-cooperation = common-cooperation - 0.2 * common-cooperation
(defn new-common-cooperation [old-common-cooperation moves-made current-tick]
  (let [threshold (util/one-div moves-made current-tick)]
    (util/reinforce-value old-common-cooperation threshold 0.2 0.5)))

;; time-left = time-total - current-tick
;; chance-of-death = (active-players - ticks-left) / active-players
;; if (active-players - ticks-left) = 0:
;; ==> chance-of-death = 0
;; TODO: Implement a panic that kicks in at consecutive number of delta increasing.
(defn new-chance-of-death [old-chance-of-death time-left num-active-players]
  (let [players-to-die (- num-active-players time-left)]
    (if (neg? players-to-die) 
      0 
      (util/one-div players-to-die num-active-players))))

(defn jump-off-platform [platform bridge moving-players]
  (let [brave-player (most-willing-player platform moving-players)]
    (if (and brave-player (-> bridge first val nil?))
      [(disj platform brave-player) (assoc bridge 1 brave-player)]
      [platform bridge])))

(defn new-common-knowledge [common-knowledge tempered-tiles old-bridge new-bridge delta-deaths]
  ;; Common knowledge is mined if somebody pioneers a new step.
  ;; Somebody must have mined a new step if there are new deaths or change in leading step
  (if (= common-knowledge tempered-tiles)
    common-knowledge
    (let [leading-tile 
          (fn [tiles] 
            (if-let 
                [max-tile
                    (->> tiles
                        (filter (comp some? val))
                        (into (sorted-map))
                        (last))] 
                (first max-tile)
                0))
          delta-leading-step (< (leading-tile old-bridge) (leading-tile new-bridge))
          next-step (inc (count common-knowledge))]
      (if (or delta-leading-step (pos? delta-deaths))
        (assoc 
          common-knowledge
          next-step
          (get tempered-tiles next-step))
        common-knowledge))))

(defn update-state [state moving-players tempered-tiles total-time]
  (let [bridge 
          (move-on-bridge (:bridge state) moving-players tempered-tiles (:common-knowledge state))
        [new-platform new-bridge] 
          (jump-off-platform (:platform state) bridge moving-players)
        new-dead-players 
          (deceased-players (:bridge state) new-bridge (:dead-players state))
        new-survivors 
          (cond-> (:survivors state) 
            (val (last new-bridge)) (conj (val (last new-bridge))))
        new-common-knowledge 
          (new-common-knowledge
            (:common-knowledge state)
            tempered-tiles
            (:bridge state) 
            new-bridge 
            (- (count new-dead-players) (count (:dead-players state))))
        new-tick (inc (:tick state))
        new-moves-made 
          (+ (moves-made (:bridge state) new-bridge) (:moves-made state))
        time-left (- total-time (:tick state))
        num-active-players
          (+ (count (:platform state)) (count (remove nil? (vals new-bridge))))
        new-chance-of-death
          (new-chance-of-death (:chance-of-death state) time-left num-active-players)
        new-jump-misfortune
          (util/zero-div (count new-dead-players) new-moves-made)
        new-common-cooperation
          (new-common-cooperation (:common-cooperation state) new-moves-made new-tick)]
    (->State 
      new-platform 
      new-bridge 
      new-dead-players 
      new-survivors 
      new-common-knowledge 
      new-tick 
      new-moves-made
      new-chance-of-death
      new-jump-misfortune
      new-common-cooperation)))

(defn update-players [active-ids id->players old-state new-state]
  (let [delta-cc (- (:common-cooperation new-state) (:common-cooperation old-state))
        delta-cod (- (:chance-of-death new-state) (:chance-of-death old-state))
        delta-jm (- (:jump-misfortune new-state) (:jump-misfortune old-state))
        new-player #(players/->Random 
                      (:id %)
                      (players/update-will-to-live % delta-cod)
                      (players/update-aggression % delta-jm)
                      (players/update-cooperation % delta-cc))]
    (reduce-kv
      (fn [m id player] 
        (assoc m id (new-player player))) 
      {}
      (select-keys id->players active-ids))))

(defn tick [state id->player tempered-tiles total-time]
  (let [active-id->location 
          (active-id->location state)
        moving-players 
          (moving-players 
            active-id->location 
            id->player 
            (:common-knowledge state) 
            (:common-cooperation state)
            (count tempered-tiles))
        new-state 
          (update-state state moving-players tempered-tiles total-time)
        new-id->player
          (update-players (keys active-id->location) id->player state new-state)]
    [new-state new-id->player]))

(defn start-simulation [initial-state all-id->all-player tempered-tiles total-time]
  (loop [state initial-state
         id->player all-id->all-player]
    (if (or (= (:tick state) total-time) (empty? id->player))
      [state id->player]
      (let [[new-state new-id->player] 
              (tick state id->player tempered-tiles total-time)]
        (recur new-state new-id->player)))))

(defn -main [& args]
  ;; (let [config (edn/read-string (slurp "config.edn"))]
  ;;   (init-state new-state (:num-steps config) (:num-ticks config))
  ;;   (spawn-players new-state (:num-players config))
  ;;   (log/log :log-state new-state)
  ;;   (start-simulation new-state)
  ;;   (shutdown-agents))
  (let [all-players (id-to-players 7)
        state (init-state 5 all-players)
        fin-state (start-simulation state all-players foo 5)]
      (println state)
      (println "***************")
      (println (first fin-state)))
)