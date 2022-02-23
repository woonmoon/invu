(ns invu.domain)

;; A bridge is a map {StepNo: Step}
;; Step has a left and right field which are both mutable lists of players
;; StepNo starts with 1 and ends with the number of steps
;; e.g. A bridge of steps has StepNo 1, 2, ..., 6
(defrecord Step [left right])

(defn init-bridge [num-steps]
  (zipmap 
    (range 1 (inc num-steps)) 
    (take num-steps 
      (repeatedly #(map->Step {:left (atom {}) :right (atom {})})))))