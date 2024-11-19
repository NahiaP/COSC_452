(ns mini.quilout
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def frame-rate 5)
(def size 30)
(def predator-count 1)
(def prey-count 2) ; Reduced initial prey count
(def grass-count 20) ; Added grass count
(def prey-lifespan (* 100 frame-rate)) ; Number of generations prey can live without eating
(def predator-lifespan (* 3 frame-rate)) ; Number of generations predators can live without eating

(defrecord Entity [type speed x y last-fed])

(defn random-entity [x y type]
  (->Entity type (rand-int 5) x y 0))

(defn random-world []
  (let [empty-world (vec (for [x (range size)]
                           (vec (for [y (range size)]
                                  nil))))
        predator-positions (take predator-count (shuffle (for [x (range size) y (range size)] [x y])))
        prey-positions (take prey-count (shuffle (for [x (range size) y (range size)] [x y])))
        grass-positions (take grass-count (shuffle (for [x (range size) y (range size)] [x y])))]
    (reduce (fn [world [x y]]
              (assoc-in world [x y] (random-entity x y "predator")))
            (reduce (fn [world [x y]]
                      (assoc-in world [x y] (random-entity x y "prey")))
                    (reduce (fn [world [x y]]
                              (assoc-in world [x y] (random-entity x y "grass")))
                            empty-world
                            grass-positions)
                    prey-positions)
            predator-positions)))

(defn setup []
  (q/frame-rate frame-rate)
  (q/background 255)
  {:world (random-world)
   :generation 0})

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
              new-x (max 0 (min (dec size) (+ (:x entity) dx)))
              new-y (max 0 (min (dec size) (+ (:y entity) dy)))]
          (assoc entity :x new-x :y new-y :last-fed 0))
        (let [dx (- (rand-int 3) 1)
              dy (- (rand-int 3) 1)
              new-x (max 0 (min (dec size) (+ (:x entity) dx)))
              new-y (max 0 (min (dec size) (+ (:y entity) dy)))]
          (assoc entity :x new-x :y new-y :last-fed (inc (:last-fed entity))))))

    (= (:type entity) "prey")
    (let [dx (- (rand-int 3) 1)
          dy (- (rand-int 3) 1)
          new-x (max 0 (min (dec size) (+ (:x entity) dx)))
          new-y (max 0 (min (dec size) (+ (:y entity) dy)))]
      (assoc entity :x new-x :y new-y :last-fed (inc (:last-fed entity))))

    (= (:type entity) "grass")
    entity ; Grass entities do not move

    ))

(defn step-forward [state]
  (let [world (:world state)
        generation (:generation state)
        new-world (vec (for [x (range size)]
                         (vec (for [y (range size)]
                                (let [entity (nth (nth world x) y)]
                                  (if entity
                                    (let [moved-entity (move-entity world entity)]
                                      (cond
                                        (and (= (:type moved-entity) "predator")
                                             (= (:type (nth (nth world (:x moved-entity)) (:y moved-entity))) "prey"))
                                        (assoc moved-entity :x (:x moved-entity) :y (:y moved-entity))

                                        (and (= (:type moved-entity) "prey")
                                             (>= (:last-fed moved-entity) prey-lifespan)) ; Prey dies if it hasn't eaten within prey-lifespan generations
                                        nil

                                        (and (= (:type moved-entity) "predator")
                                             (>= (:last-fed moved-entity) predator-lifespan)) ; Predator dies if it hasn't eaten within predator-lifespan generations
                                        nil

                                        :else moved-entity))
                                    nil))))))]
    {:world new-world :generation (inc generation)}))

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

(q/defsketch life
  :host "host"
  :size [(* size 10) (* size 10)]
  :setup setup
  :update (fn [state] (step-forward state))
  :draw (fn [state] (draw-world (:world state)))
  :middleware [m/fun-mode])