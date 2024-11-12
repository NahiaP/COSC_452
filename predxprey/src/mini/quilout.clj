(ns mini.quilout
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; Define the size of the world and other parameters
(def size 30)
(def predator-count 1)
(def prey-count 10)
(def grass-spawn-interval 20)

;; Define a record to represent an entity with type, speed, x, and y coordinates
(defrecord Entity [type speed x y])

;; Function to generate a random entity at a given position (x, y)
(defn random-entity [x y]
  (let [types ["predator" "prey" "grass" "empty"]
        type (rand-nth types)]
    (cond
      (= type "empty") nil
      :else (->Entity type (rand-int 5) x y))))

;; Function to generate a random world of size x size filled with random entities
(defn random-world []
  (vec (for [x (range size)]
         (vec (for [y (range size)]
                (random-entity x y))))))

;; Setup function to initialize the Quil sketch
(defn setup []
  (q/frame-rate 10)
  (q/background 255)
  {:world (random-world)
   :generation 0})

;; Function to calculate the Euclidean distance between two entities
(defn distance [entity1 entity2]
  (Math/sqrt (+ (Math/pow (- (:x entity1) (:x entity2)) 2)
                (Math/pow (- (:y entity1) (:y entity2)) 2))))

;; Function to find the nearest entity of a given type to a given entity
(defn nearest-entity [world entity target-type]
  (let [targets (for [x (range size)
                      y (range size)
                      :let [e (nth (nth world x) y)]
                      :when (and e (= (:type e) target-type))]
                  e)]
    (when (seq targets)
      (apply min-key #(distance entity %) targets))))

;; Function to move an entity based on its type and the nearest target
(defn move-entity [world entity]
  (cond
    ;; If the entity is a predator, move towards the nearest prey
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

    ;; If the entity is a prey, move towards the nearest grass
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
          (assoc entity :x new-x :y new-y :reproduce true))
        entity))

    ;; If the entity is grass or empty, move randomly
    :else
    (let [dx (- (rand-int 3) 1)
          dy (- (rand-int 3) 1)
          new-x (mod (+ (:x entity) dx) size)
          new-y (mod (+ (:y entity) dy) size)]
      (assoc entity :x new-x :y new-y))))

;; Function to update the state of the world for each generation
(defn step-forward [state]
  (let [world (:world state)
        generation (:generation state)
        new-world (vec (for [x (range size)]
                         (vec (for [y (range size)]
                                (let [entity (nth (nth world x) y)]
                                  (if entity
                                    (let [moved-entity (move-entity world entity)]
                                      (cond
                                        ;; If a predator moves to a cell with prey, it eats the prey
                                        (and (= (:type moved-entity) "predator")
                                             (= (:type (nth (nth world (:x moved-entity)) (:y moved-entity))) "prey"))
                                        (assoc moved-entity :x (:x moved-entity) :y (:y moved-entity))

                                        ;; If a prey moves to a cell with grass, it eats the grass and reproduces
                                        (and (= (:type moved-entity) "prey")
                                             (= (:type (nth (nth world (:x moved-entity)) (:y moved-entity))) "grass"))
                                        (do
                                          (let [new-world (assoc-in world [(:x moved-entity) (:y moved-entity)] nil)]
                                            (assoc-in new-world [(mod (inc (:x moved-entity)) size) (mod (inc (:y moved-entity)) size)]
                                                      (->Entity "prey" (rand-int 5) (mod (inc (:x moved-entity)) size) (mod (inc (:y moved-entity)) size))))
                                          (assoc moved-entity :x (:x moved-entity) :y (:y moved-entity)))

                                        ;; Otherwise, just move the entity
                                        :else moved-entity))
                                    nil))))))]
    ;; Spawn new grass at intervals
    (if (zero? (mod generation grass-spawn-interval))
      (let [new-world (assoc-in new-world [(rand-int size) (rand-int size)] (->Entity "grass" 0 (rand-int size) (rand-int size)))]
        {:world new-world :generation (inc generation)})
      {:world new-world :generation (inc generation)})))

;; Function to draw the world
(defn draw-world [world]
  (q/background 255)
  (doseq [x (range size)
          y (range size)]
    (let [entity (nth (nth world x) y)]
      (when entity
        (q/fill (cond
                  (= (:type entity) "predator") (q/color 200 0 0) ; Red for predators
                  (= (:type entity) "prey") (q/color 0 0 200) ; Blue for prey
                  (= (:type entity) "grass") (q/color 0 200 0) ; Green for grass
                  :else (q/color 255))) ; Default color
        (q/rect (* (:x entity) 10) (* (:y entity) 10) 10 10)))))

;; Define the Quil sketch
(q/defsketch life
  :host "host"
  :size [(* size 10) (* size 10)]
  :setup setup
  :update (fn [state] (step-forward state))
  :draw (fn [state] (draw-world (:world state)))
  :middleware [m/fun-mode])