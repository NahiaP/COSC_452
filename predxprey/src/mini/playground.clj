(ns mini.playground
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TO DO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Step happens for 100 steps
; After step, return list
; Loop 3 times, breed inbetween

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Initializing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Global variables and creation of species
; 
; types: "predator" "prey" "grass" "empty"
; next steps: N S E W F(freeze) -> 0 1 2 3 4 (i dont feel like fixing it yet)

;; Define the size of the world and other parameters
(def size 30)
(def predator-count 3) ; initial
(def prey-count 16) ; initial
(def frame-rate 5)

;; defining a species type
(defrecord Entity [type x y nextstep grass])

; blue-print for typical entity (used for testing)
(def miscpred (Entity. "predator" 4 20 (list rand-int 5) true))
(print miscpred)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SPAWN WORLD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; to set up the matrix that is the world
; 
; first, create a matrix full of empty cells
; second, pick out a list of indexes which will grow grass
;       then, recurse through the spots in the list and set grass to true on those
; third, randomly change some empty spots into predators
; fourth, randomly change some empty spots into prey

;;;;;;;;;; create empty matrix ;;;;;;;;;;;;;;

;; spawn empty cells
(defn fill_blanks []
  (vec (for [x (range size)]
         (vec (for [y (range size)]
                (Entity. "empty" x y nil false))))))

;;;;;;;;;;; helpful fxns ;;;;;;;;;;;

; replace an item at certain index
(defn replace_at [mat x y insert]
  (assoc mat x (assoc (get mat x) y insert)))

; testing
;(replace_at fill_blanks 2 1 (Entity. "empty" 400 500 nil))

;; replace an item at certain index with entity
(defn replace_ent [mat x y newtype newnextstep grass]
  (assoc mat x (assoc (get mat x) y (Entity. newtype x y newnextstep grass))))

; testing
;(replace_ent (fill_blanks) 2 1 "predator" "HEHEHEHE" false)
;(def whatdidibreak (replace_ent fill_blanks 2 1 "predator" "HEHEHEHE"))
;(replace_ent whatdidibreak 2 2 "predator" "HEHEHEHE")

;; get entity at index x y
(defn get_ent [matrix x y]
  ((matrix x) y))

; testing
;(get_ent (fill_blanks) 0 0)

;;;;;;;;;;; unused but i didnt want to delete it yet
;
; get type at x y
(defn type_xy [mat x y]
  (:type ((mat x) y)))
;
; replace type
(defn changetype [ent newtype]
  (Entity. newtype (:x ent) (:y ent) (:nextstep ent) (:grass ent)))
; testing
;(changetype miscpred "testing")
;
; replace nextstep
(defn changenextstep [ent newstep]
  (Entity. (:type ent) (:x ent) (:y ent) newstep (:grass ent)))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;; end of helpful fxns ;;;;;;;;;;;

;; pick out some random instances to become grass
; in a diamond shape around 1 center
; yes i did it manualy, yes that was a bad idea
(defn diamond [x y]
  [[x y] [(+ 1 x) y] [x (+ 1 y)] [(+ 1 x) (+ 1 y)] [(+ 2 x) y] [x (+ 2 y)] [(- x 1) (- y 1)] [x (- y 1)] [x (- y 2)] [(+ 1 x) (- y 1)] [(- x 1) (+ y 1)] [(- x 1) y] [(- x 2) y]])

; testing
;(diamond 1 1)

;; random chunk of grass that fits in size-constraints
; the constraints acutally suck, so they are reinforced later
(defn grs_spts [s]
  (diamond (rand-int (+ s 2)) (rand-int (+ s 2))))

; testing
;(grs_spts 20)

;; specify how many chunks to add in world
(defn m_grs_spts [limit amount]
  (distinct (apply concat (for [i (range amount)] (grs_spts limit)))))

; testing
;(m_grs_spts 10 8)

;; fill one block with grass
(defn onegrass [matrix xy]
  (replace_ent matrix (xy 0) (xy 1) "empty" nil true))

;; fill up matrix with the diamond of grass
(defn recurgrass [matrix list]
  (if (empty? list)
    matrix
    (let [newmat (onegrass matrix (first list))
          newsp (rest list)]
      (if newmat
        (recurgrass newmat newsp) ; dealing with bounds is getting annoying
        (recurgrass matrix newsp)))))

; testing
;(recurgrass (fill_blanks) [[3 13] [4 13] [3 14]])
;(recurgrass (fill_blanks) (grs_spts size))

;;;;;;;;;; randomly replace with some preds ;;;;;;;;;;;;;;

;; get what is in the cell and if it has grass
; only allowed to place a new predator if the spot is empty
; its way easier later to store them both in one item to reference
(defn type&grass_xy [mat x y]
  [(:type ((mat x) y)) (:grass ((mat x) y))])

; testing
;((type&grass_xy (fill_blanks) 0 1) 0)

;; add one new predator
(defn onepred [matrix]
  (let [newx (rand-int size)
        newy (rand-int size)
        check (type&grass_xy matrix newx newy)] ; here we make sure its empty
    (if (= (check 0) "empty")
      (replace_ent matrix newx newy "predator" (list rand-int 5) (check 1))
      (onepred matrix))))

; testing
;(onepred (fill_blanks))

;; fill up matrix with random predators in random spots
; limited to the pre-set pred count
(defn recurpred [matrix count]
  (if (= count 0)
    matrix
    (let [newmat (onepred matrix)
          newcount (dec count)]
      (recurpred newmat newcount))))

; testing
;(recurpred (fill_blanks) predator-count)
;(recurpred (recurgrass (fill_blanks) (grs_spts size)) predator-count)

;;;;;;;;;; randomly replace with some prey ;;;;;;;;;;;;;;
; works exactly the same as pred

;; add one new prey
(defn oneprey [matrix]
  (let [newx (rand-int size)
        newy (rand-int size)
        check (type&grass_xy matrix newx newy)]
    (if (= (check 0) "empty")
      (replace_ent matrix newx newy "prey" (list rand-int 5) (check 1))
      (oneprey matrix))))

;; fill up matrix with random prey in random spots
; limited to the pre-set prey count
(defn recurprey [matrix count]
  (if (= count 0)
    matrix
    (let [newmat (oneprey matrix)
          newcount (dec count)]
      (recurprey newmat newcount))))

;;;;;;;;;; generate random world ;;;;;;;;;;;;;;

;; stack each step of preparing an empty world
; the let/if is because sometimes the world wouldn't work (i think problems with bounds I was too lazy to fix)
(defn random_world []
  (let [world (recurprey (recurpred (recurgrass (fill_blanks) (m_grs_spts size 2)) predator-count) prey-count)]
    (if world
      world
      (random_world))))

; testing
;(random_world)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; QUIL SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; I actually don't know how any of this works

;; Setup function to initialize the Quil sketch
(defn setup []
  (q/frame-rate frame-rate)
  (q/background 255)
  {:world (random_world)
   :generation 0})

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
                  (:grass entity) (q/color 0 200 0) ; else, Green for grass
                  :else (q/color 255))) ; else, default color
        (q/rect (* (:x entity) 10) (* (:y entity) 10) 10 10)))))

; testing that the setup works (no movement yet)
;(q/defsketch life
;  :host "host"
;  :size [(* size 10) (* size 10)]
;  :setup setup
;  :draw (fn [state] (draw-world (:world state)))
;  :middleware [m/fun-mode])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NEXT STEP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; take a world and move every creature
; 
; first, get a list of the indexes of the creatures that currently exist
; 

;;;;;;;;;; get list of creatures ;;;;;;;;;;

;; given a world matrix, return a list of the creatures in it
(defn creats_in_w [mat]
  (remove #(= (:type %) "empty") (apply concat mat)))

; testing
;(creats_in_w (random_world))

;; given this list, get the xs and ys
(defn creats_xys [list]
  (map vector (map :x list) (map :y list)))

; testing
;(creats_xys (list miscpred))

;; given a world matrix, return the list of creature xs and ys
(defn w_creat_xys [matrix]
  (let [li (creats_in_w matrix)]
    (creats_xys li)))

; testing
;(w_creat_xys (random_world))

;;;;;;;;;; movement decisions ;;;;;;;;;;

; if 0, y+1
; if 1, x+1
; if 2, y-1
; if 3, x-1
; if 4, same

;; given a direction, return the new x and y it wants to go to
(defn wantstogo_helper [direction x y]
  (cond
    (= 0 direction)
    [x (+ y 1)]
    (= 1 direction)
    [(+ x 1) y]
    (= 2 direction)
    [x (- y 1)]
    (= 3 direction)
    [(- x 1) y]
    (= 4 direction)
    [x y]
    ))

; testing
;(wantstogo_helper 4 20 20)

;; checking if the next step is in bounds 
; bounds are 0 - (size-1)
(defn in_bounds [xy]
  (if (< -1 (xy 0))
    (if (< (xy 0) size)
      (if (< -1 (xy 1))
        (if (< (xy 1) size)
          true
          false)
        false)
      false)
    false))

;; given an entity, evaluate it's nextstep function to determin the direction it wants to go it
; then return what the idex is in that direction
(defn wantstogo [ent]
  (let [dir (eval (:nextstep ent))
        here (wantstogo_helper dir (:x ent) (:y ent))]
    ; cheking in bounds, reroll else wise
    (if (in_bounds here)
      here
      (wantstogo ent))
    ))

; testing
;(wantstogo miscpred)

;;;;;;;;;; movement helper fxns ;;;;;;;;;;

;; a pred or prey wants to move into an available spot
(defn movecreat [world ent new] 
  (let [oldgrass (:grass ent)] ; don't want to acidentaly move grass around
    (replace_ent (replace_ent world (:x new) (:y new) (:type ent) (:nextstep ent) (if (= (:type ent) "predator") ; later add eating mechanic
                                                                                    (:grass new) ; if a pred, grass same | if prey, grass gone
                                                                                    false)) (:x ent) (:y ent) "empty" nil oldgrass)))

;; a prey walks into a spot with a predator and gets eaten
(defn slayed [world idiot] ; later, add mechanism to extend pred life
  (replace_ent world (:x idiot) (:y idiot) "empty" nil false)) ; a prey would have eaten the grass

;; a pred walks into a prey and eats it
(defn slay [world pred prey]
  (let [oldgrass (:grass pred)]
    (replace_ent (replace_ent world (:x prey) (:y prey) "predator" (:nextstep pred) false) (:x prey) (:y prey) "empty" nil oldgrass))) 

; testing
;(def itsasmallworld [[(Entity. "pred" 0 0 40 false) (Entity. "empty" 0 1 nil true)]])
;(movecreat itsasmallworld (Entity. "pred" 0 0 40 false) (Entity. "empty" 0 1 nil true))

;;;;;;;;;; grass-respawning ;;;;;;;;;;
; randomly spread in one direction
; subject to change, this seems like the best idea for now

;; given a world matrix, return a list of the grass in it
(defn grass_in_w [mat]
  (remove #(false? (:grass %)) (apply concat mat)))

; testing
;(grass_in_w (random_world))

;; given a world matrix, return the list of grass xs and ys
; using creat xys works
(defn w_grass_xys [matrix]
  (let [li (grass_in_w matrix)]
    (creats_xys li)))

;; given the list, get xs and ys offset by 1
; these will be the new xs and ys
; can actually reuse another helper
; added a shuffle and take 2 to limit the amount of growth
(defn w_newgrass [list]
  (take 2 (shuffle (filter in_bounds (mapv #(wantstogo_helper (rand-int 4) (% 0) (% 1)) list)))))

; testing
;(w_newgrass [[1 2] [3 4] [0 0]])

;; fill one block with grass
(defn newgrass [world ent]
  (replace_ent world (:x ent) (:y ent) (:type ent) (:nextstep ent) true))

;; fill up matrix with the diamond of grass
(defn recurnewgrass [matrix list]
  (if (empty? list)
    matrix
    (let [newxy (first list)
          newmat (newgrass matrix (get_ent matrix (newxy 0) (newxy 1)))
          newsp (rest list)]
      (if newmat
        (recurnewgrass newmat newsp) ; dealing with bounds is getting annoying
        (recurnewgrass matrix newsp)))))

(def newgrass)

;;;;;;;;;; given a world, move everyone ;;;;;;;;;;

; get list of creatures
; shuffle it (equal chance to step first)
; for each creature, eval which direction it wants to go in (0 1 2 3 4)
; check what is in that spot
  ; if empty, step forward (reroll if out of bounds)
  ; if prey
    ; if creature is predator, replace it it is now dead
    ; if creature is prey, reroll
  ; if pred
    ; if creature is prey, it dies
    ; if creature is pred, reroll

;; this function executes the movement with a list of the creatures
(defn move_with_list [world list]
  (if (empty? list) ; (recurgrass (fill_blanks) (m_grs_spts size 2))
    (recurnewgrass world (w_newgrass (w_grass_xys world)))
    (let [curent (get_ent world ((first list)0) ((first list)1)) ; getting entity info
          spotxy (wantstogo curent) ; getting desired direction
          spot (get_ent world (spotxy 0) (spotxy 1))] ; getting what is there
      (cond
        (= (:type spot) "empty") 
        (move_with_list (movecreat world curent spot) (rest list)) ; [world ent new] 
        (= (:type spot) "prey")
        (cond 
          (= (:type curent) "prey") ; pred or prey
          (move_with_list world (rest list))
          (= (:type curent) "predator")
          (move_with_list (slay world curent spot) (rest list)))
        (= (:type spot) "predator")
        (if (= (:type curent) "predator") ; pred or prey
          (move_with_list world (rest list))
          (move_with_list (slayed world curent) (rest list))
          )))))

;; given a world, get all the creatures, shuffle the order of the list, and call move_with_list
(defn move_in_world [world]
  (move_with_list world (shuffle (w_creat_xys world)))
  )

;; applying the steps forward on the state item
(defn steps-forward [state]
  (let [world (:world state)
        generation (:generation state)
        new-world (move_in_world world)]
    {:world new-world :generation (inc generation)}))

; testing
(q/defsketch life
  :host "host"
  :size [(* size 10) (* size 10)]
  :setup setup
  :update (fn [state] (steps-forward state))
  :draw (fn [state] (draw-world (:world state)))
  :middleware [m/fun-mode])











