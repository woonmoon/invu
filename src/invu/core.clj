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
          :common-knowledge []}))

(defonce id-to-player (atom {}))

(defonce bridge (atom (into (sorted-map) (domain/init-bridge 6))))

(defn spawn-players [state player-lookup num-players]
  (let [ids (take num-players (repeatedly #(gensym "Player")))
        players (into [] (map (partial players/init-player) ids))
        ids-to-players (zipmap ids players)]
    ;; NOTE: maybe a bit dodge? idk if it's guaranteed that names and players are 
    ;;       correctly ordered at this point.
    (swap! state util/state-union :platform-players (into #{} ids))
    (swap! player-lookup merge ids-to-players)))

(defn maybe-jump [state bridge player-lookup]
  "I volunteer as tribute!"
  (let [candidates (vec (:platform-players @state))
        tributes (remove nil? (into [] (map players/decide-jump (map #(get @player-lookup %) candidates))))]
    (when (not (empty? tributes))
      (let [chosen-one-name (rand-nth tributes)
            chosen-one (get @player-lookup chosen-one-name)
            player-move (if (zero? (players/jump chosen-one (:common-knowledge @state))) :right :left)]
            (swap! state util/state-disj :platform-players chosen-one-name)
            (swap! state util/state-union :bridge-players #{chosen-one-name})
            (swap! (get-in @bridge [1 player-move]) conj chosen-one-name)))))

(defn can-jump [position bridge]
  "Check if the next step of the bridge is occupied or not."
  (let [next-step (get @bridge (inc position))]
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
(defn maybe-move [bridge player-lookup common-knowledge]
  (doseq [[num-step step] (reverse @bridge)
          :let [location (find-players step)]
          :when (not (nil? location))
          ;; This only works because max no. of players on a step is 1.
          :let [player-id (first @location)]]
    (when (can-jump num-step bridge)
      (let [player-move (players/move (get @player-lookup player-id) common-knowledge)]
        (when (not (nil? player-move))
          (let [player-dir (if (zero? player-move) :right :left)]
            (swap! location disj player-id)
            (swap! (get-in @bridge [(inc num-step) player-dir]) conj player-id)))))))

; TODO: You can definitely write this without doseq.
(defn eliminate [state bridge player-lookup yellowbrick-rd]
  (doseq [player (map #(get @player-lookup %) (:bridge-players @state))
          :let [id (:id player)]
          :let [path @(:path-travelled player)]
          :let [last-move (if (zero? (last path)) :right :left)]
          :when (not= (nth yellowbrick-rd (dec (count path))) last-move)]
    (swap! (get-in @bridge [(count path) last-move]) disj id)
    (swap! state util/state-disj :bridge-players #{id})
    (swap! state util/state-union :dead-players #{id})))

(defn update-common-knowledge [state player-lookup]
  (println (map #(get @player-lookup %) (:bridge-players @state)))
  (println (first (sort-by count (map #(deref (:path-travelled %)) (map #(get @player-lookup %) (:bridge-players @state))))))
  (let [players (map #(get @player-lookup %) (:bridge-players @state))
        longest-path (first (sort-by count (map #(deref (:path-travelled %)) players)))]
      (when (< (count (:common-knowledge @state)) (count longest-path))
        (println longest-path)
        (swap! state util/state-replace :common-knowledge longest-path)
      )
    )
  )

;; (defn end-of-tick [state yellowbrick-rd]
;;   (doseq [[num-step step] (:bridge @state)
;;           :let [location (find-players step)]
;;           :when (not (nil? location))
;;           ;; This only works because max no. of players on a location is 1.
;;           :let [[_ player] (first @location)]
;;           :let [path (:path-travelled player)]]
;;     (when (not= (nth yellowbrick-rd (dec (count @path))) (last @path))
;;       (eliminate player location state))))


(defn -main []
  (spawn-players players-state id-to-player 10)
  ; Do this part under dosync
  (maybe-jump players-state bridge id-to-player)
  (maybe-move bridge id-to-player (:common-knowledge @players-state))
  (eliminate players-state bridge id-to-player tempered-steps)
  (update-common-knowledge players-state id-to-player)
  ; (end-of-tick players-state tempered-steps)
  (shutdown-agents))
