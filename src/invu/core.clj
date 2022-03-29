(ns invu.core
  (:import (javax.swing JPanel))
  (:require [invu.util :as util]
            [invu.player :as players]
            [invu.domain :as domain])
  (:gen-class))

(defonce tempered-steps [0 1 0 1 0 0])

(defonce players-state
  (atom { :platform-players #{}
          :bridge-players #{}
          :dead-players #{}
          :survivors #{}
          :common-knowledge []
          :bridge (into (sorted-map) (domain/init-bridge 6))}))

(defonce id-to-player (atom {}))

(defn spawn-players [state player-lookup num-players]
  (let [ids (take num-players (repeatedly #(gensym "Player")))
        players (into [] (map (partial players/init-player) ids))
        ids-to-players (zipmap ids players)]
    ;; NOTE: maybe a bit dodge? idk if it's guaranteed that names and players are 
    ;;       correctly ordered at this point.
    (swap! state util/state-union :platform-players (into #{} ids))
    (swap! player-lookup merge ids-to-players)))

; This can def be done better without abusing let-bindings. Read about post-walk
(defn maybe-jump [state player-lookup]
  "I volunteer as tribute!"
  (let [candidates (vec (:platform-players state))
        tributes (remove nil? (into [] (map players/decide-jump (map #(get @player-lookup %) candidates))))]
    (when (not (empty? tributes))
      (let [chosen-one-name (rand-nth tributes)
            chosen-one (get @player-lookup chosen-one-name)
            player-move (if (zero? (players/jump chosen-one (:common-knowledge state))) :right :left)
            disjoint-state (util/state-disj state :platform-players chosen-one-name)
            unioned-state (util/state-union disjoint-state :bridge-players #{chosen-one-name})]
              (swap! (get-in unioned-state [:bridge 1 player-move]) conj chosen-one-name)
              unioned-state))))

(defn can-jump [position state]
  "Check if the next step of the bridge is occupied or not."
  (let [next-step (get-in state [:bridge (inc position)])]
    (if (and (empty? @(:left next-step)) (empty? @(:right next-step)))
      true 
      false)))

(defn find-players [step]
  "If the step is occupied return the map of occupants, if none return nil"
  (cond 
    (not (empty? @(:left step))) (:left step)
    (not (empty? @(:right step))) (:right step)
    :default nil))

; TODO: Clean this up. For now you're kind of stuck in this mess because
;       you specifically want to traverse front of bridge to back.
(defn maybe-move [state player-lookup]
  (doseq [[num-step step] (reverse (state :bridge))
          :let [location (find-players step)]
          :when (not (nil? location))
          ;; This only works because max no. of players on a step is 1.
          :let [player-id (first @location)]]
    (when (can-jump num-step state)
      (let [player-move (players/move (get @player-lookup player-id) (:common-knowledge state))]
        (when (not (nil? player-move))
          (let [player-dir (if (zero? player-move) :right :left)]
            (swap! location disj player-id)
            (swap! (get-in state [:bridge (inc num-step) player-dir]) conj player-id))))))
    state)

; TODO: You can definitely write this without doseq.
(defn eliminate [state player-lookup yellowbrick-rd]
  (doseq [player (map #(get @player-lookup %) (:bridge-players @state))
          :let [id (:id player)]
          :let [path @(:path-travelled player)]
          :let [last-move (if (zero? (last path)) :right :left)]
          :when (not= (nth yellowbrick-rd (dec (count path))) (last path))
          ]
    (swap! (get-in @state [:bridge (count path) last-move]) disj id)
    (swap! state util/state-disj :bridge-players id)
    (swap! state util/state-union :dead-players #{id})))

; DO SOMETHING ABOUT THE DEAD AGENTS
(defn join-states [jumped-state moved-state]
  (let [new-bridge (assoc (:bridge moved-state) 1 (get-in jumped-state [:bridge 1]))]
    {
      :platform-players (:platform-players jumped-state)
      :bridge-players (:bridge-players jumped-state)
      :common-knowledge (:common-knowledge moved-state)
      :bridge new-bridge
    }
  ))

(defn update-common-knowledge [state player-lookup]
  (let [players (map #(get @player-lookup %) (:bridge-players @state))
        longest-path (first (sort-by count (map #(deref (:path-travelled %)) players)))]
      (when (< (count (:common-knowledge @state)) (count longest-path))
        (swap! state util/state-replace :common-knowledge longest-path))))

(defn tick [state player-lookup tempered-steps]
  (let [jumped-state (future (swap! state maybe-jump player-lookup))]
    (swap! state maybe-move player-lookup)
    (swap! state join-states @jumped-state))
  (eliminate state player-lookup tempered-steps)
  (update-common-knowledge state player-lookup)
  (println @state))

(defn -main []
  (spawn-players players-state id-to-player 10)
  (println "INITIAL STATE")
  (println @players-state)
  (println "*************")
  (dotimes [_ 5] 
    (tick players-state id-to-player tempered-steps))
  ; (end-of-tick players-state tempered-steps)
  (shutdown-agents))
