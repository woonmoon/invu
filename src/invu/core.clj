(ns invu.core
  (:import (javax.swing JPanel))
  (:require [invu.util :as util]
            [invu.player :as players]
            [invu.domain :as domain])
  (:gen-class))

(defonce animator (agent nil))

(defonce tempered-steps [0 1 0 1 0 0])
 
(defonce app-state
  (atom { :active-players {:lower-platform {}
                            :bridge {}}
          :inactive-players {:dead {} 
                              :survived {}}}))

(defn spawn-players [state num-players]
  (let [names (take num-players (repeatedly #(gensym "Player")))
        players (into [] (map (partial players/init-player) names))
        active-players (zipmap names players)]
    ;; NOTE: maybe a bit dodge? idk if it's guaranteed that names and players are 
    ;;       correctly ordered at this point.
    (swap! state assoc-in [:active-players :lower-platform] active-players)))

(defn maybe-jump [state]
  ; I volunteer as tribute!
  (let [candidates (get-in @state [:active-players :lower-platform])
        tributes (remove nil? (into [] (map players/decide-jump (vals candidates))))]
    (when (not (empty? tributes))
      (let [chosen-one-name (rand-nth tributes)
            chosen-one (get candidates chosen-one-name)]
            (println (count (get-in @state [:active-players :lower-platform])))
            (players/jump chosen-one)
            (swap! state util/dissoc-in [:active-players :lower-platform] chosen-one-name)
            (println (count (get-in @state [:active-players :lower-platform]))))))
    ;; (swap! (get-in @state [:active-players :lower-platform]) dissoc chosen-one-name)
    ;; (swap! (get-in @state [:active-players :bridge] assoc chosen-one-name chosen-one))
  ;; (println (remove nil? (into [] (map players/decide-jump (vals (get-in @state [:active-players :lower-platform]))))))
  ;; (println (rand-nth (remove nil? (into [] (map players/decide-jump (vals (get-in @state [:active-players :lower-platform])))))))
)

(defn -main []
  (spawn-players app-state 10)
  (maybe-jump app-state))
