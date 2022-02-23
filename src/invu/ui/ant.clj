(ns invu.ui.ant
    (:require [invu.ui.core :as ui]
              [invu.util :as util]))
  
  (def directions
    {0 [2 0 2 4]
     1 [4 0 0 4]
     2 [4 2 0 2]
     3 [4 4 0 0]
     4 [2 4 2 0]
     5 [0 4 4 0]
     6 [0 2 4 2]
     7 [0 0 4 4]})
  
  (defn ant-color [_] :red)
  
  (defn render-ant [img x y]
    (ui/make-rect img {:color :red
                       :fill [x y 5 5]}))
  