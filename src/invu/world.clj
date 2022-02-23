(ns invu.world
    (:require [invu.util :as util]))

(defn next-step [current-step dimensions direction]
    "Returns random coordinates somewhere in the next step"
    ; Pick random x-coordinate within next x-range depending on direction
    (let [x-offset (cond->> (rand-int (:step-width dimensions))
                    (zero? direction) (util/rand-range (:step-width dimensions) (:platform-width dimensions)))]
        ; Pick random y-coordinate within the next step y-range
        [x-offset (util/rand-offset (first (nth (:steps-data dimensions) current-step)) (:step-height dimensions))]))
    