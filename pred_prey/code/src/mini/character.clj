;; A version of Conway's Game of Life that prints the world to the REPL.
;; Represents cell states as spaces and * characters.

;; Lee Spector (lspector@amherst.edu), 2023

(ns mini.character)

(def size 20)

(defn random-world []
  (repeatedly size
              (fn []
                (repeatedly size #(rand-nth [" " "*"])))))

(defn print-world [world]
  (println "-----")
  (doseq [row world]
    (println row)))

(defn live-neighbors [world x y]
  (reduce + (for [i [-1 0 1]
                  j [-1 0 1]]
              (if (= i j 0)
                0
                (if (= "*" (nth (nth world (mod (+ x i) size))
                                (mod (+ y j) size)))
                  1
                  0)))))

(defn step-forward [world]
  (for [x (range size)]
    (for [y (range size)]
      (let [neigh (live-neighbors world x y)]
        (if (= " " (nth (nth world x) y))
          (if (= neigh 3) "*" " ")
          (if (<= 2 neigh 3) "*" " "))))))

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
