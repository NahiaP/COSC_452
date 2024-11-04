;; A version of Conway's Game of Life that prints the world to the REPL.
;; Represents cell states using the numbers 0 and 1.

;; Lee Spector (lspector@amherst.edu), 2023

(ns mini.numeric)

(def size 10)

(defn random-world []
    (repeatedly size
                (fn []
                  (repeatedly size #(rand-int 2)))))

(defn print-world [world]
  (println "-----")
  (doseq [row world]
    (println row)))

(defn live-neighbors [world x y]
    (reduce + (for [i [-1 0 1]
                    j [-1 0 1]]
                (if (= i j 0)
                  0
                  (nth (nth world (mod (+ x i) size))
                       (mod (+ y j) size))))))

(defn step-forward [world]
    (for [x (range size)]
      (for [y (range size)]
        (let [neigh (live-neighbors world x y)]
          (if (zero? (nth (nth world x) y))
            (if (= neigh 3) 1 0)
            (if (<= 2 neigh 3) 1 0))))))

(defn life [opts]
  (loop [world (random-world)
         step 0]
    (print-world world)
    (if (>= step (:steps opts))
      :done
      (recur (step-forward world)
             (inc step)))))

;; Evaluate the following to run the game, starting with a random world,
;; for 50 steps:

#_(life {:steps 50})