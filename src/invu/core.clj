(ns invu.core
  (:import (javax.swing JPanel))
  (:require [invu.util :as util]
            [invu.player :as players]
            [invu.domain :as domain])
  (:gen-class))

(defonce animator (agent nil))

(defonce tempered-steps [0 1 0 1 0 0])
 
(defonce app-state
  (atom { :active-players {:platform {}
                            :bridge (sorted-map)}
          :inactive-players {:dead {} 
                              :survived {}}}))

(defn spawn-players [state num-players]
  (let [names (take num-players (repeatedly #(gensym "Player")))
        players (into [] (map (partial players/init-player) names))
        active-players (zipmap names players)]
    ;; NOTE: maybe a bit dodge? idk if it's guaranteed that names and players are 
    ;;       correctly ordered at this point.
    (swap! state assoc-in [:active-players :platform] active-players)))

(defn maybe-jump [state]
  ; I volunteer as tribute!
  (let [candidates (get-in @state [:active-players :platform])
        tributes (remove nil? (into [] (map players/decide-jump (vals candidates))))]
    (when (not (empty? tributes))
      (let [chosen-one-name (rand-nth tributes)
            chosen-one (get candidates chosen-one-name)]
            (players/jump chosen-one)
            (swap! state util/dissoc-in [:active-players :platform] chosen-one-name)
            (swap! state assoc-in [:active-players :bridge 0] chosen-one)))))

;; (defn maybe-move [state]
;;   (doseq [[step player] (reverse (get-in @state [:active-players :bridge]))]))

(defn -main []
  (spawn-players app-state 10)
  (maybe-jump app-state))
