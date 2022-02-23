(ns invu.domain)

(defrecord Cell [player platform location tempered])

(defn build-cell [args]
  (map->Cell (merge {:platform false :tempered true :location [0 0]} args)))
