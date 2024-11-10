(ns mini.quilout
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def size 50)

(defrecord Entity [type speed x y])

(defn random-entity [x y]
  (let [types ["predator" "prey" "grass" "empty"]
        type (rand-nth types)]
    (cond
      (= type "empty") nil
      :else (->Entity type (rand-int 5) x y))))

(defn random-world []
  (vec (for [x (range size)]
         (vec (for [y (range size)]
                (random-entity x y))))))

(defn setup []
  (q/frame-rate 10)
  (q/background 255)
  (random-world))

(defn distance [entity1 entity2]
  (Math/sqrt (+ (Math/pow (- (:x entity1) (:x entity2)) 2)
                (Math/pow (- (:y entity1) (:y entity2)) 2))))

(defn nearest-entity [world entity target-type]
  (let [targets (for [x (range size)
                      y (range size)
                      :let [e (nth (nth world x) y)]
                      :when (and e (= (:type e) target-type))]
                  e)]
    (when (seq targets)
      (apply min-key #(distance entity %) targets))))

(defn move-entity [world entity]
  (cond
    (= (:type entity) "predator")
    (let [prey (nearest-entity world entity "prey")]
      (if prey
        (let [dx (cond
                   (< (:x entity) (:x prey)) 1
                   (> (:x entity) (:x prey)) -1
                   :else 0)
              dy (cond
                   (< (:y entity) (:y prey)) 1
                   (> (:y entity) (:y prey)) -1
                   :else 0)
              new-x (mod (+ (:x entity) dx) size)
              new-y (mod (+ (:y entity) dy) size)]
          (assoc entity :x new-x :y new-y))
        entity))

    (= (:type entity) "prey")
    (let [grass (nearest-entity world entity "grass")]
      (if grass
        (let [dx (cond
                   (< (:x entity) (:x grass)) 1
                   (> (:x entity) (:x grass)) -1
                   :else 0)
              dy (cond
                   (< (:y entity) (:y grass)) 1
                   (> (:y entity) (:y grass)) -1
                   :else 0)
              new-x (mod (+ (:x entity) dx) size)
              new-y (mod (+ (:y entity) dy) size)]
          (assoc entity :x new-x :y new-y))
        entity))

    :else
    (let [dx (- (rand-int 3) 1)
          dy (- (rand-int 3) 1)
          new-x (mod (+ (:x entity) dx) size)
          new-y (mod (+ (:y entity) dy) size)]
      (assoc entity :x new-x :y new-y))))

(defn step-forward [world]
  (vec (for [x (range size)]
         (vec (for [y (range size)]
                (let [entity (nth (nth world x) y)]
                  (if entity
                    (let [moved-entity (move-entity world entity)]
                      (cond
                        (and (= (:type moved-entity) "predator")
                             (= (:type (nth (nth world (:x moved-entity)) (:y moved-entity))) "prey"))
                        nil

                        (and (= (:type moved-entity) "prey")
                             (= (:type (nth (nth world (:x moved-entity)) (:y moved-entity))) "grass"))
                        nil

                        :else moved-entity))
                    nil)))))))

(defn draw-world [world]
  (q/background 255)
  (doseq [x (range size)
          y (range size)]
    (let [entity (nth (nth world x) y)]
      (when entity
        (q/fill (cond
                  (= (:type entity) "predator") (q/color 255 0 0) ; Red for predators
                  (= (:type entity) "prey") (q/color 0 255 0) ; Green for prey
                  (= (:type entity) "grass") (q/color 0 255 255) ; Cyan for grass
                  :else (q/color 255))) ; Default color
        (q/rect (* (:x entity) 10) (* (:y entity) 10) 10 10)))))

(q/defsketch life
  :host "host"
  :size [(* size 10) (* size 10)]
  :setup setup
  :update (fn [state] (step-forward state))
  :draw draw-world
  :middleware [m/fun-mode])