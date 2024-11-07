

;; (ns mini.quilout
;;   (:require [quil.core :as q :include-macros true]
;;             [quil.middleware :as m]))

;; (def size 100)

;; (defn random-world []
;;   (repeatedly size
;;               (fn []
;;                 (repeatedly size #(rand-nth [" " "*"])))))

;; (defn setup []
;;   (q/frame-rate 500)
;;   (random-world))

;; (defn live-neighbors [world x y]
;;   (reduce + (for [i [-1 0 1]
;;                   j [-1 0 1]]
;;               (if (= i j 0)
;;                 0
;;                 (if (= "*" (nth (nth world (mod (+ x i) size))
;;                                 (mod (+ y j) size)))
;;                   1
;;                   0)))))

;; (defn step-forward [world]
;;   (let [result (for [x (range size)]
;;                  (for [y (range size)]
;;                    (let [neigh (live-neighbors world x y)]
;;                      (if (= " " (nth (nth world x) y))
;;                        (if (= neigh 3) "*" " ")
;;                        (if (<= 2 neigh 3) "*" " ")))))] result))


;; (defn draw-world [world]
;;   (dotimes [r size]
;;     (dotimes [c size]
;;       (q/fill (if (= (nth (nth world r) c) "*") 0 255))
;;       (q/rect (* r 10) (* c 10) 10 10))))

;;   (q/defsketch life
;;     :host "host"
;;     :size [500 500]
;;     :setup setup
;;     :update step-forward
;;     :draw draw-world
;;     :middleware [m/fun-mode])


(ns mini.quilout
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def size 50)

(defrecord Entity [type speed strength moves])

(defn random-entity []
  (let [types ["predator" "prey" "grass" "empty"]
        type (rand-nth types)]
    (cond
      (= type "empty") nil
      (= type "grass") (->Entity type 0 0 0)
      :else (->Entity type (rand-int 5) (rand-int 10) 0))))

(defn random-world []
  (repeatedly size
              (fn []
                (repeatedly size random-entity))))

(defn setup []
  (q/frame-rate 500)
  (random-world))

(defn mean-location [world entity-type]
  (let [locations (for [x (range size)
                        y (range size)
                        :let [entity (nth (nth world x) y)]
                        :when (and entity (= (:type entity) entity-type))]
                    [x y])]
    (if (empty? locations)
      [0 0]
      (let [mean-x (/ (reduce + (map first locations)) (count locations))
            mean-y (/ (reduce + (map second locations)) (count locations))]
        [(int mean-x) (int mean-y)]))))

(defn nearest-grass [world x y]
  (let [distances (for [i (range size)
                        j (range size)
                        :let [entity (nth (nth world i) j)]
                        :when (and entity (= (:type entity) "grass"))]
                    [(Math/abs (- x i)) (Math/abs (- y j)) [i j]])]
    (if (empty? distances)
      [x y]
      (let [[_ _ [gx gy]] (apply min-key (fn [[dx dy _]] (+ dx dy)) distances)]
        [gx gy]))))

(defn move-towards [world x y target-x target-y]
  (let [dx (cond
             (< x target-x) 1
             (> x target-x) -1
             :else 0)
        dy (cond
             (< y target-y) 1
             (> y target-y) -1
             :else 0)
        new-x (mod (+ x dx) size)
        new-y (mod (+ y dy) size)]
    (if (nil? (nth (nth world new-x) new-y))
      (assoc-in (assoc-in world [x y] nil) [new-x new-y] (nth (nth world x) y))
      world)))

(defn step-forward [world]
  (let [mean-prey (mean-location world "prey")]
    (reduce (fn [w [x y]]
              (if-let [entity (nth (nth w x) y)]
                (cond
                  (= (:type entity) "predator")
                  (move-towards w x y (first mean-prey) (second mean-prey))

                  (= (:type entity) "prey")
                  (let [new-entity (update entity :moves inc)
                        [gx gy] (nearest-grass w x y)]
                    (if (>= (:moves new-entity) 3)
                      (if (and (= x gx) (= y gy))
                        (assoc-in (assoc-in w [x y] nil) [gx gy] (assoc new-entity :moves 0))
                        (assoc-in w [x y] nil))
                      (move-towards w x y gx gy)))

                  :else w)
                w))
            world
            (for [x (range size) y (range size)] [x y]))))

(defn draw-world [world]
  (dotimes [r size]
    (dotimes [c size]
      (let [entity (nth (nth world r) c)]
        (q/fill (cond
                  (nil? entity) 255
                  (= (:type entity) "predator") 0
                  (= (:type entity) "prey") 100
                  (= (:type entity) "grass") 50))
        (q/rect (* r 10) (* c 10) 10 10)))))

(q/defsketch life
  :host "host"
  :size [500 500]
  :setup setup
  :update step-forward
  :draw draw-world
  :middleware [m/fun-mode])