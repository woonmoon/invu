(ns invu.core
  (:import (javax.swing JPanel))
  (:require [invu.util :as util]
            [invu.player :as players]
            [invu.domain :as domain])
  (:gen-class))

(defonce tempered-steps [0 1 0 1 0 0])

(defonce new-state
  (atom { :active-players {}
          :dead-players {}
          :survivors {}
          :common-knowledge []}))

(defn init-state [state num-steps]
  (let [num-entries (inc num-steps)
        bridge (zipmap 
                  (map #(keyword (str %)) (range num-entries)) 
                  (take num-entries (repeat #{})))]
    (swap! state assoc :active-players bridge)))

(defn spawn-players [state num-players]
  (let [ids (take num-players (repeatedly #(gensym "Player")))
        players (into #{} (map (partial players/init-player) ids))]
    (swap! state assoc-in [:active-players :0] players)))

(defn maybe-jump [state]
  "I volunteer as tribute!"
  (let [candidates (get-in state [:active-players :0])
        tributes (remove nil? 
                    (map #(players/decide-jump % (:common-knowledge state)) candidates))]
    (if (empty? tributes)
      state
      (let [chosen-one (rand-nth tributes)
            player-move (players/jump chosen-one (:common-knowledge state))
            disjoint-state (update-in state [:active-players :0] disj chosen-one)]
            (swap! (:location chosen-one) inc)
            (assoc-in disjoint-state [:active-players :1] #{chosen-one})))))

(defn can-jump [location state]
  "Check if the next step of the bridge is occupied or not."
  (empty? (get-in state [:active-players (inc location)])))

(defn find-players [step]
  "If the step is occupied return the map of occupants, if none return nil"
  (cond 
    (not (empty? @(:left step))) (:left step)
    (not (empty? @(:right step))) (:right step)
    :default nil))

(defn maybe-move [state]
  (let [platform (get-in state [:active-players :0])
        bridge (atom (dissoc (:active-players state) :0))
        common-knowledge (:common-knowledge state)]
    ;; (println "INIT BRIDGE")
    ;; (println @bridge)
    (doseq [[step players] @bridge
            :when (not (empty? players))
            :let [player (first players)]
            :let [location @(:location player)]
            :when (can-jump location state)
            :let [player-move (players/move player common-knowledge)]
            :when (not (nil? player-move))]
      ;; (println "SOMEBODY CHOSE TO MOVE")
      ;; (println @bridge)
      (swap! bridge #(update-in % [step] disj player))
      ;; (println "NEW INDEX")
      ;; (println (keyword (str (inc location))))
      (swap! bridge #(update-in % [(keyword (str (inc location)))] conj player)))
    ;; (println "PLATFORM")
    ;; (println platform)
    ;; (println "BRIDGE")
    ;; (println @bridge)
    ;; (println "*******")
    (assoc state :active-players (merge {:0 platform} @bridge))))

; TODO: You can definitely write this without doseq.
(defn eliminate [state player-lookup yellowbrick-rd]
  (doseq [player (map #(get @player-lookup %) (:bridge-players @state))
          :let [id (:id player)]
          :let [path @(:path-travelled player)]
          :let [last-move (if (zero? (last path)) :right :left)]
          :let [correct-step (nth yellowbrick-rd (dec (count path)))]
          :when (not= correct-step (last path))]
    (swap! (get-in @state [:bridge (count path) last-move]) disj id)
    (swap! state util/state-disj :bridge-players id)
    (swap! state util/state-union :dead-players #{id})
    correct-step))

; DO SOMETHING ABOUT THE DEAD AGENTS
(defn join-states [jumped-state moved-state]
  (let [new-bridge (assoc (:bridge moved-state) 1 (get-in jumped-state [:bridge 1]))]
    {
      :platform-players (:platform-players jumped-state)
      :bridge-players (:bridge-players jumped-state)
      :dead-players (:dead-players jumped-state)
      :survivors (:survivors jumped-state)
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
  (let [jumped-state (future (swap! new-state maybe-jump))]
    (swap! new-state maybe-move)
    (swap! state join-states @jumped-state))
  (eliminate state player-lookup tempered-steps)
  (update-common-knowledge state player-lookup)
  (println @state))

(defn -main []
  ;; (spawn-players players-state id-to-player 10)
  ;; (println "INITIAL STATE")
  ;; (println @players-state)
  ;; (println "*************")
  ;; (dotimes [_ 5] 
  ;;   (tick players-state id-to-player tempered-steps))
  ;; ; (end-of-tick players-state tempered-steps)
  ;; (shutdown-agents)

  (println @new-state)
  (println "********")
  (init-state new-state 6)
  (spawn-players new-state 5)
  (swap! new-state maybe-jump)
  (println @new-state)
  (println "********")
  (swap! new-state maybe-move)
  (println @new-state)
  (shutdown-agents)
)
