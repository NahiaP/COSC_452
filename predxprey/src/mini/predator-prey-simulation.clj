(ns mini.predator-prey-simulation
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; Constants for grid size and initial counts
(def grid-size 100)
(def predator-count 5)
(def prey-count 15)
(def max-age {:predator 120 :prey 180})  ; ages in seconds

;; State to hold entities
(defn initial-state []
  {:predators (vec (for [_ (range predator-count)] {:type :predator, :position [(rand-int grid-size) (rand-int grid-size)], :age 0}))
   :prey      (vec (for [_ (range prey-count)] {:type :prey, :position [(rand-int grid-size) (rand-int grid-size)], :age 0}))})

;; Function to calculate distance
(defn distance [pos1 pos2]
  (Math/sqrt (reduce + (map #(Math/pow (- %1 %2) 2) pos1 pos2))))

;; Find nearest friend or enemy
(defn nearest-entity [entity entities type]
  (->> entities
       (filter #(= (:type %) type))
       (reduce (fn [nearest other]
                 (let [d (distance (:position entity) (:position other))]
                   (if (< d (:dist nearest))
                     {:entity other, :dist d}
                     nearest)))
               {:entity nil, :dist Double/POSITIVE_INFINITY})
       :entity))

;; Random movement logic with wrap-around at grid edges
(defn move [entity]
  (let [angle (rand (* 2 Math/PI))
        dx (Math/cos angle)
        dy (Math/sin angle)
        [x y] (:position entity)
        new-pos [(mod (+ x dx) grid-size)
                 (mod (+ y dy) grid-size)]]
    (assoc entity :position (mapv int new-pos))))

;; Update entity positions and ages
(defn update-entities [state]
  (let [update-fn (fn [entity]
                    (let [moved (move entity)
                          age (:age entity)]
                      (assoc moved :age (inc age))))]
    (-> state
        (update :predators (partial mapv update-fn))
        (update :prey (partial mapv update-fn)))))

;; Collision detection and interactions
(defn handle-collisions [state]
  (let [prey (atom (:prey state))
        predator (fn [p]
                   (if-let [nearest-prey (nearest-entity p @prey :prey)]
                     (do (swap! prey #(vec (remove #(= % nearest-prey) %)))
                         p)
                     p))]
    (-> state
        (assoc :predators (mapv predator (:predators state)))
        (assoc :prey @prey))))

;; Main update function
(defn update-state [state]
  (-> state
      update-entities
      handle-collisions))

;; Drawing function
(defn draw-state [state]
  (q/background 255)
  (doseq [{:keys [position]} (:predators state)]
    (q/fill 255 0 0)
    (apply q/ellipse (conj position 10 10)))
  (doseq [{:keys [position]} (:prey state)]
    (q/fill 0 255 0)
    (apply q/ellipse (conj position 10 10))))

;; Setting up Quil sketch
(q/defsketch predator-prey
  :title "Predator-Prey Simulation"
  :size [500 500]
  :setup (fn [] (q/frame-rate 10) (initial-state))
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode])