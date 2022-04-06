(ns invu.core
  (:import (javax.swing JPanel))
  (:require [invu.util :as util]
            [invu.player :as players]
            [invu.domain :as domain])
  (:gen-class))

(defonce tempered-steps {1 0, 2 1, 3 0, 4 1, 5 0, 6 0})

(defonce new-state
  (atom { :active-players {}
          :dead-players #{}
          :survivors #{}
          :common-knowledge {}}))

(defn init-state [state num-steps]
  (let [num-entries (inc num-steps)
        bridge (zipmap 
                  (range num-entries)
                  (take num-entries (repeat #{})))]
    (swap! state assoc :active-players bridge)))

(defn spawn-players [state num-players]
  (let [ids (take num-players (repeatedly #(gensym "Player")))
        players (into #{} (map (partial players/init-player) ids))]
    (swap! state assoc-in [:active-players 0] players)))

(defn maybe-jump [state]
  "I volunteer as tribute!"
  (let [candidates (get-in state [:active-players 0])
        tributes (remove nil? 
                    (map #(players/decide-jump % (:common-knowledge state)) candidates))]
    (if (empty? tributes)
      state
      (let [chosen-one (rand-nth tributes)
            player-move (players/jump chosen-one (:common-knowledge state))
            disjoint-state (update-in state [:active-players 0] disj chosen-one)]
        (when (empty? (get-in state [:active-players 1]))
          (println "SOMEBODY IS JUMPING! PLAYER: " (:id chosen-one))
          (swap! (:location chosen-one) inc)
          (assoc-in disjoint-state [:active-players 1] #{chosen-one})
        )))))

(defn next-step [location state]
  "Check if the next step of the bridge is occupied or not."
  (let [next-step (inc location)]
    (if (empty? (get-in state [:active-players next-step]))
      next-step
      nil)))

(defn find-players [step]
  "If the step is occupied return the map of occupants, if none return nil"
  (cond 
    (not (empty? @(:left step))) (:left step)
    (not (empty? @(:right step))) (:right step)
    :default nil))

(defn maybe-move [state]
  (let [platform (get-in state [:active-players 0])
        bridge (atom (into (sorted-map-by >) (dissoc (:active-players state) 0)))
        common-knowledge (:common-knowledge state)]
    ;; (doseq [[step players] @bridge
    ;;         :when (not (empty? players))
    ;;         :let [player (first players)]
    ;;         :let [location @(:location player)]
    ;;         :when (can-jump location state)
    ;;         :let [player-move (players/move player common-knowledge)]
    ;;         :when (not (nil? player-move))]
    ;;   (println "Moving player: " (:id player))
    ;;   (swap! bridge #(update % step disj player))
    ;;   (swap! bridge #(update % (inc location) conj player)))
    (doseq [[step players] @bridge]
      (println "STEP: " step " PLAYER: " players)
      (when-let [player (first players)]
        (when-let [next-step (next-step step state)]
          (when-let [player-move (players/move player common-knowledge)]
            (println "NOT EMPTY AT STEP " step " WITH PLAYER " (:id player) " PLAYER MOVE: " player-move)
            (swap! bridge update step disj player)
            (swap! bridge update next-step conj player)
            ))))
    (println "BRIDGE: " (into (sorted-map) @bridge))
    (assoc state :active-players (merge {0 platform} (into (sorted-map) @bridge)))))

(defn find-leading-step [active-players]
  (first 
    (first 
      (filter #(not (empty? (second %))) (reverse active-players)))))

(defn kill [state step player]
  (swap! state #(update-in % [:active-players step] disj player))
  (swap! state #(update-in % [:dead-players] conj player))
)

; Nobody stepped forward
; Somebody stepped forward and died
; Somebody stepped forward and survived
(defn find-correct-step [state tempered-steps]
  (let [leading-step (find-leading-step (get @state :active-players))
        leading-player (first (get-in @state [:active-players leading-step]))
        correct-step (get tempered-steps leading-step)
        common-knowledge (:common-knowledge @state)]
    ;; (println "LEADING STEP: " leading-step)
    ;; (println "LEADING PLAYER: " leading-player)
    ;; (println "CORRECT STEP: " correct-step)
    ;; (println "LEADING DECISION: " @(:decision leading-player))
    (if (or (= 0 leading-step) (contains? common-knowledge leading-step))
      nil
      (if (= correct-step @(:decision leading-player))
        [leading-step correct-step]
        (do
          (kill state leading-step leading-player)
          [leading-step (util/other-direction @(:decision leading-player))])))))

(defn end-of-tick [state tempered-steps]
  (when-let [[step knowledge] (find-correct-step state tempered-steps)]
    ;; (println "STEP: " step " KNOWLEDGE: " knowledge)
    (swap! state #(assoc-in % [:common-knowledge step] knowledge))))

;jumped-state and state
(defn tick [state tempered-steps]
  (swap! state maybe-move)
  (swap! state maybe-jump)
  (end-of-tick state tempered-steps))

(defn -main []
  (init-state new-state 6)
  (spawn-players new-state 5)
  (println "********INITIAL STATE********")
  (println @new-state)
  (newline)
  (println "********START TICK 1********")
  (tick new-state tempered-steps)
  (println "********FINAL TICK 1  STATE********")
  (println @new-state)
  (newline)
  (println "********START TICK 2********")
  (tick new-state tempered-steps)
  (println "********FINAL TICK 2 STATE********")
  (println @new-state)
  (newline)
  (println "********START TICK 3********")
  (tick new-state tempered-steps)
  (println "********FINAL TICK 3 STATE********")
  (println @new-state)
  (newline)
  (println "********START TICK 4********")
  (tick new-state tempered-steps)
  (println "********FINAL TICK 4 STATE********")
  (println @new-state)
  (newline)
  (shutdown-agents)
)
