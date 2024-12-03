(ns archive.gh-quilout
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;BUGS
;1. Predators and prey should never collide with eachother. When spawning, prey should spawn on the nearest free tile and predators should move away when they're going to collide.
;2. Grass should spawn every x generations and be controlled by another global var
;3. Predators should eat prey/not dance around them for generations (maybe has smth to do with movement)
;4. Prey should consume grass and move towards nearest grass tile
;5. Predators and prey should not be stuck on grass tiles
;6. Prey should reproduce after eating grass           

; Define the frame rate for the simulation
(def frame-rate 10)

; Define the size of the world (size x size grid) multiplied in setup function at the bottom
(def size 50)

; Define the initial count of predators
(def predator-count 1)

; Define the initial count of prey
(def prey-count 1)

; Define the initial count of grass
(def grass-count 0)

; Define the lifespan of prey in terms of generations
(def prey-lifespan (* 100 frame-rate)) 

; Define the lifespan of predators in terms of generations (multiply by 100 so they don't die immediately) at higher frame rates)
(def predator-lifespan (* 100 frame-rate)) 

; Define a record to represent an entity in the world
(defrecord Entity [type x y])

; Function to create a random entity of a given type at position (x, y)
(defn random-entity [x y type]

;create a constructor for entities
(->Entity type x y))

; Function to create a random world with predators, prey, and grass
(defn random-world []
  ; Create an empty world represented as a 2D vector
  (let [empty-world (vec (for [x (range size)]
                           (vec (for [y (range size)]
                                  nil))))
        ; Generate random positions for predators
        predator-positions (take predator-count (shuffle (for [x (range size) y (range size)] [x y])))
        ; Generate random positions for prey
        prey-positions (take prey-count (shuffle (for [x (range size) y (range size)] [x y])))
        ; Generate random positions for grass
        grass-positions (take grass-count (shuffle (for [x (range size) y (range size)] [x y])))]
    ; Place grass entities in the world at the generated positions
    (reduce (fn [world [x y]]
              (assoc-in world [x y] (random-entity x y "grass")))
            ; Place prey entities in the world at the generated positions
            (reduce (fn [world [x y]]
                      (assoc-in world [x y] (random-entity x y "prey")))
                    ; Place predator entities in the world at the generated positions
                    (reduce (fn [world [x y]]
                              (assoc-in world [x y] (random-entity x y "predator")))
                            empty-world
                            predator-positions)
                    prey-positions)
            grass-positions)))

; Function to set up the initial state of the simulation
(defn setup []
  ; Set the frame rate for the simulation
  (q/frame-rate frame-rate)
  ; Set the background color to white
  (q/background 255)
  ; Return the initial state with a random world and start at generation 1
  {:world (random-world)
   :generation 1})

; Function to calculate the distance between two entities
(defn distance [entity1 entity2]
  ; Use the Euclidean distance formula
  (Math/sqrt (+ (Math/pow (- (:x entity1) (:x entity2)) 2)
                (Math/pow (- (:y entity1) (:y entity2)) 2))))

; Function to find the nearest entity of a given type to a specified entity
(defn nearest-entity [world entity target-type]
  ; Collect all entities of the target type in the world
  (let [targets (for [x (range size)
                      y (range size)
                      :let [e (nth (nth world x) y)]
                      :when (and e (= (:type e) target-type))]
                  e)]
    ; Return the nearest target entity, if any
    (when (seq targets)
      (apply min-key #(distance entity %) targets))))

; Function to move an entity in the world
(defn move-entity [world entity]
  (cond
    ; If the entity is a predator
    (= (:type entity) "predator")
    (let [prey (nearest-entity world entity "prey")]
      (if prey
        ; Move towards the nearest prey
        (let [dx (cond
                   (< (:x entity) (:x prey)) 1 ;if x of entity is greater move up
                   (> (:x entity) (:x prey)) -1 ;if x of entity is lesser move down
                   :else 0)
              dy (cond
                   (< (:y entity) (:y prey)) 1 ;if y of entity is greater move up
                   (> (:y entity) (:y prey)) -1 ;if y of entity is lesser move down
                   :else 0)
              new-x (max 0 (min (dec size) (+ (:x entity) dx))) ;increment x if it is within bounds
              new-y (max 0 (min (dec size) (+ (:y entity) dy)))] ;increment y if it is within bounds
          (if (and (= new-x (:x prey)) (= new-y (:y prey)))
            ; If the predator reaches the prey, remove the prey
            (do
              (assoc-in world [new-x new-y] nil) ; Remove prey
              (assoc entity :x new-x :y new-y)) ; Update predator position
            ; Otherwise, move the predator
            (assoc entity :x new-x :y new-y)))
        ; If no prey is found, move randomly
        (let [dx (- (rand-int 3) 1)
              dy (- (rand-int 3) 1)
              new-x (max 0 (min (dec size) (+ (:x entity) dx)))
              new-y (max 0 (min (dec size) (+ (:y entity) dy)))]
          (assoc entity :x new-x :y new-y))))

    ; If the entity is prey
    (= (:type entity) "prey")
    (let [grass (nearest-entity world entity "grass")]
      (if grass
        ; Move towards the nearest grass (same function as above just different entity)
        (let [dx (cond
                   (< (:x entity) (:x grass)) 1
                   (> (:x entity) (:x grass)) -1
                   :else 0)
              dy (cond
                   (< (:y entity) (:y grass)) 1
                   (> (:y entity) (:y grass)) -1
                   :else 0)
              new-x (max 0 (min (dec size) (+ (:x entity) dx)))
              new-y (max 0 (min (dec size) (+ (:y entity) dy)))]
          (if (and (= new-x (:x grass)) (= new-y (:y grass)))
            ; If the prey reaches the grass, remove the grass
            (do
              (assoc-in world [new-x new-y] nil) ; Remove grass
              (assoc entity :x new-x :y new-y))
            ; Otherwise, move the prey
            (assoc entity :x new-x :y new-y)))
        ; If no grass is found, move randomly
        (let [dx (- (rand-int 3) 1)
              dy (- (rand-int 3) 1)
              new-x (max 0 (min (dec size) (+ (:x entity) dx)))
              new-y (max 0 (min (dec size) (+ (:y entity) dy)))]
          (assoc entity :x new-x :y new-y))))

    ; If the entity is grass, it does not move
    (= (:type entity) "grass")
    entity))

; Function to advance the simulation by one step
(defn step-forward [state]
  ; Get the current world and generation from the state
  (let [world (:world state)
        generation (:generation state)
        ; Create a new world by moving each entity
        new-world (vec (for [x (range size)]
                         (vec (for [y (range size)]
                                (let [entity (nth (nth world x) y)]
                                  (if entity
                                    (let [moved-entity (move-entity world entity)]
                                      (cond
                                        ; If a predator reaches a prey, remove the prey
                                        (and (= (:type moved-entity) "predator")
                                             (= (:type (nth (nth world (:x moved-entity)) (:y moved-entity))) "prey"))
                                        (assoc moved-entity :x (:x moved-entity) :y (:y moved-entity))

                                        ; If prey exceeds its lifespan, remove it
                                        (and (= (:type moved-entity) "prey")
                                             (>= (:generation state) prey-lifespan))
                                        nil

                                        ; If predator exceeds its lifespan, remove it
                                        (and (= (:type moved-entity) "predator")
                                             (>= (:generation state) predator-lifespan))
                                        nil

                                        ; Otherwise, keep the moved entity
                                        :else moved-entity))
                                    ; If no entity is present, return nil
                                    nil))))))]
    ; Return the new state with the updated world and incremented generation
    {:world new-world :generation (inc generation)}))

; Function to draw the world on the screen
(defn draw-world [world]
  ; Set the background color to white
  (q/background 255)
  ; Iterate over each cell in the world
  (doseq [x (range size)
          y (range size)]
    (let [entity (nth (nth world x) y)]
      ; If an entity is present, draw it with the appropriate color
      (when entity
        (q/fill (cond
                  (= (:type entity) "predator") (q/color 200 0 0) ; Red for predators
                  (= (:type entity) "prey") (q/color 0 0 200) ; Blue for prey
                  (= (:type entity) "grass") (q/color 0 200 0) ; Green for grass
                  :else (q/color 255))) ; Default color
        ; Draw the entity as a rectangle
        (q/rect (* (:x entity) 10) (* (:y entity) 10) 10 10)))))

; Define the Quil sketch
(q/defsketch life
  :host "host"
  :size [(* size 10) (* size 10)]
  ; Set up the initial state
  :setup setup
  ; Update the state by stepping forward
  :update (fn [state] (step-forward state))
  ; Draw the world based on the current state
  :draw (fn [state] (draw-world (:world state)))
  ; Use the fun-mode middleware
  :middleware [m/fun-mode])