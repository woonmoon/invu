(ns invu.core
  (:import (javax.swing JPanel))
  (:require [invu.util :as util]
            [invu.player :as players]
            [invu.domain :as domain])
  (:gen-class))

(defonce tempered-steps {:1 0, :2 1, :3 0, :4 1, :5 0, :6 0})

(defonce new-state
  (atom { :active-players {}
          :dead-players #{}
          :survivors #{}
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
    (doseq [[step players] @bridge
            :when (not (empty? players))
            :let [player (first players)]
            :let [location @(:location player)]
            :when (can-jump location state)
            :let [player-move (players/move player common-knowledge)]
            :when (not (nil? player-move))]
      (swap! bridge #(update-in % [step] disj player))
      (swap! bridge #(update-in % [(keyword (str (inc location)))] conj player)))
    (assoc state :active-players (merge {:0 platform} @bridge))))

(defn find-leading-step [num-players active-players]
  (first 
    (first 
      (filter #(not (empty? (second %))) (reverse active-players)))))

(defn kill [state step player]
  (swap! state #(update-in % [:active-players step] disj player))
  (swap! state #(update-in % [:dead-players] conj player))
  (util/other-direction (:decision player)))

; TODO: You can definitely write this without doseq.
(defn eliminate [state tempered-steps]
  (let [leading-step (find-leading-step 5 (:active-players @state))
        leading-player (first (get-in @state [:active-players leading-step]))]
    (if (or (= :0 leading-step) (= (leading-step tempered-steps) @(:decision leading-player)))
        nil
        (kill state leading-step leading-player))))

;; (defn end-of-tick [state tempered-steps]
;;   (let [new-wisdom (eliminate state tempered-steps)]
    
;;     (swap! state #(update % :common-knowledge new-wisdom))
;;     )
;;   )

(defn join-states [jumped-state moved-state]
  (let [platform (get-in jumped-state [:active-players :0])
        bridge (dissoc (:active-players moved-state) :0)]
    {
      :active-players (merge {:0 platform} bridge)
      :dead-players (:dead-players jumped-state)
      :survivors (:survivors jumped-state)
      :common-knowledge (:common-knowledge moved-state)
    }))

(defn tick [state tempered-steps]
  (let [jumped-state (future (swap! state maybe-jump))]
    (swap! state maybe-move)
    (swap! state join-states @jumped-state)))

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
  (tick new-state tempered-steps)
  (eliminate new-state tempered-steps)
  (println @new-state)
  (shutdown-agents)
)
