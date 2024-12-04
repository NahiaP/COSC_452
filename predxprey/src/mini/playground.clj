(ns mini.playground
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TO DO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Breeding fxn (after certain amount of frames, call this to breed them)
; Update the quil step-forward function to call breeding when applicable
;       (after calling breeding, it should call the regular movement again)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INDEX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
; A. Initializing
; B. Movement Genome
;    B.1. genome creation
;    B.2. determine closest creature
;    B.3. get lists of creature xys
;    B.4. block you want to step towards
;    B.5. helpful fxns for determining direction
;    B.6. determining direction
; C. Spawn World
;    C.1. create empty matrix
;    C.2. helpful fxns
;    C.3. adding grass
;    C.4. randomly replace with some preds
;    C.5. randomly replace with some prey
;    C.6. generate random world
; D. Quil Setup
; E. Breeding                        -> to-do
; F. Next Step
;    F.1. movement decisions
;    F.2. movement helper fxns
;    F.3. grass-respawning
;    F.4. next generation            -> to-do                           
;    F.5. given a world, move everyone
; G. Testing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; A. INITIALIZING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Global variables and creation of species
; 
; types: "predator" "prey" "grass" "empty"
; next steps: N E S W F(freeze) -> 0 1 2 3 4 

;; Define the size of the world and other parameters
; adjustable:
(def size 40)
(def predator-count 3) ; initial
(def prey-count 16) ; initial
(def frame-rate 5)
(def grass_regrowth 2) ; rate at which grass re-grows
; automatic
(def middlepoints (vector (int (/ size 2)) (int (/ size 2)))) ; rounding sucks so just go to floor

;; defining a species type
(defrecord Entity [type x y nextstep grass ate])

; blue-print for typical entity (used for testing)
(def miscpred (Entity. "predator" 4 20 (list rand-int 5) true true))
(print miscpred)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; B. MOVEMENT GENOME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; figuring out how the fuck angles work
;
; A genome should be:
; [[p1 d1] [p2 d2] [p3 d3] [p4 d4] [p5 d5]]
;
; Here, p can be 0-5
;       d can be 0 or 1
; 
; Evaluated as:
; Direction ~   p1 * ([center] + (d1 * pi))
;             + p2 * ([nearest same species] + (d2 * pi))
;             + p3 * ([nearest diff species] + (d3 * pi))
;             + p4 * ([nearest grass] + (d4 * pi))
;             + p5 * ([random] + (d5 * pi))
; 
; fist, find the thing being considered 
; then, get a vector of the object to that thing
; then, normalize all the vectors to 0-1
; then, if applicable flip the vector to go away instead of towards that consiedration
; then, magnify the vectors by the ps (increase weight as applicable)
; then, add all the vectors and get resulting vector
; then, get the angle of that final vector and translate that into the final direction
;
; also, need a fxn to generate the genome
; also, need a fxn that given an ent, calculate direction

;;;;;;;;;; B.1. genome creation ;;;;;;;;;;;;;;

;; generate a random movement genome
; reminder: 
; [[p1 d1] [p2 d2] [p3 d3] [p4 d4] [p5 d5]]
; p can be 0-5
; d can be 0 or 1
(defn random_move_gene []
  [[(rand-int 6) (rand-int 2)] [(rand-int 6) (rand-int 2)] [(rand-int 6) (rand-int 2)] [(rand-int 6) (rand-int 2)] [(rand-int 6) (rand-int 2)]])

;;;;;;;;;; B.2. determine closest creature ;;;;;;;;;;;;;;

;; Fxn to calculate the Euclidean distance between two entities
(defn distance [xy1 xy2]
  (Math/sqrt (+ (Math/pow (- (xy1 0) (xy2 0)) 2)
                (Math/pow (- (xy1 1) (xy2 1)) 2))))

;; given a ent xy and a list of other xys, return the closest xy
(defn get_closest [center others]
  ((first (sort-by first (map vector (map #(distance center %) others) others))) 1))

;;;;;;;;;; B.3. get lists of creature xys ;;;;;;;;;;

;; given a world matrix, return a list of the creatures in it
(defn creats_in_w [mat]
  (remove #(= (:type %) "empty") (apply concat mat)))

;; given a world matrix, return a list of one type of creatures in it
(defn spec_creats_in_w [mat type]
  (filter #(= (:type %) type) (apply concat mat)))

;; given a list of creatures, get the xs and ys
(defn creats_xys [list]
  (map vector (map :x list) (map :y list)))

;; given a world matrix, return the list of creature xs and ys
(defn w_creat_xys [matrix]
  (let [li (creats_in_w matrix)]
    (creats_xys li)))

;; given a world matrix, return the list of a type of speceis xs and ys
(defn w_spect_creat_xys [matrix type]
  (let [li (spec_creats_in_w matrix type)]
    (creats_xys li)))

;; given a world matrix, return a list of the grass in it
(defn grass_in_w [mat]
  (remove #(false? (:grass %)) (apply concat mat)))

;; given a world matrix, return the list of grass xs and ys
; using creat xys works
(defn w_grass_xys [matrix]
  (let [li (grass_in_w matrix)]
    (creats_xys li)))

;;;;;;;;;; B.4. block you want to step towards ;;;;;;;;;;

;; Find nearest same species
; given a species, get a list of current species/location on the map
; remove yourself from list
; calculate all the distances
; shuffle (more fairness if 2 are same direction)
; sort list
; get smallest
; return the point of the closest same 
(defn nearest_same [world ent]
  (let [my_xy [(:x ent) (:y ent)]
        others (remove #{my_xy} (w_spect_creat_xys world (:type ent)))]
    (get_closest my_xy others)
    ))

;; Find nearest different species
(defn nearest_diff [world ent]
  (let [my_xy [(:x ent) (:y ent)]
        my_spec (:type ent)
        others (w_spect_creat_xys world (if (= my_spec "prey")
                                                           "predator"
                                                           "prey"))]
    (get_closest my_xy others)))

;; Find the nearest grass block
; deal with case where im on the grass later
(defn nearest_grass [world ent]
  (let [my_xy [(:x ent) (:y ent)]
        others (w_grass_xys world)]
    (get_closest my_xy others)))

;; give a random index to step to 
(defn rand_dir [x y]
  (let [dir (rand-int 5)]
  (cond
  (= 0 dir)
  [x (+ y 1)]
  (= 1 dir)
  [(+ x 1) y]
  (= 2 dir)
  [x (- y 1)]
  (= 3 dir)
  [(- x 1) y]
  (= 4 dir)
  [x y]
  )))

;;;;;;;;;; B.5. helpful fxns for determining direction ;;;;;;;;;;

;; Given 2 points, give me the vector from one to the other
; from central to outside
(defn give_vector [center other]
  [(- (other 0) (center 0)) (- (other 1) (center 1))])

;; Given ent point and list of points, generate the vectors for every point
(defn get_all_the_vectors [list entxy]
  (map #(give_vector entxy %) list)
  )

;; given a vector, normalize it
(defn normalize_vector [vec]
  (let [x (vec 0)
        y (vec 1)
        mag (Math/sqrt (+ (* x x) (* y y)))]
    (if (= mag 0)
      [0 0]
      [(/ x mag) (/ y mag)])))

;; given a list of vectors, normalize all of them
(defn norm_vec_list [list]
  (map normalize_vector list))

;; given a vector, flip the direction
; determines if you go towards or away from this thing
(defn flip_vector [vec]
  [(- 0 (vec 0)) (- 0 (vec 1))])

;; given a vector and a 0 or 1, flip any 1s
(defn list_flipper [vec flip]
  (if (= flip 0)
    vec
    (flip_vector vec)))

;; given a vector, magnify it
(defn mag_vector [vec factor]
  [(* factor (vec 0)) (* factor (vec 1))])

;; given a list of vectors, get the sum
(defn sum_vec_list [list]
  (apply mapv + list))

(defn round-to-hund [n] ;; credit to chatgpt for coming up with this one, ate that up
  (/ (Math/round (* n 100)) 100.0))

;;;;;;;;;; B.6. determining direction ;;;;;;;;;;

;; given a vector (preferably, the final one) return the direction it indicates
; next steps: N S E W F(freeze) -> 0 1 2 3 4 
; note: it's hard to get 0, so if the magnitude is small enough that becomes 0
; currently, small enough will be 
(defn final_direction [vec]
  (let [x (vec 0)
        y (vec 1)
        mag (Math/sqrt (+ (* x x) (* y y)))]
    (if (< mag 2) ; if the magnitude of the total sum is smaller than 2, stay in the same spot
      4
      (let [rad_angle (round-to-hund (Math/atan2 y x))]
        (cond
          (< 2.35 rad_angle)
          3
          (< rad_angle -2.35)
          3
          (= rad_angle 2.35)
          (* 3 (rand-int 2)) ; randomly 0 or 3
          (= rad_angle -2.35)
          (+ 2 (rand-int 2)) ; randomly 2 or 3
          (< 0.709 rad_angle)
          0
          (= 0.709 rad_angle) ; randomly 0 or 1
          (rand-int 2)
          (< rad_angle -0.709)
          2
          (= rad_angle -0.709)
          (+ 1 (rand-int 2)) ; randomly 1 or 2
          :else
          1
          )))))

;; given an ent, calculate desired direction
(defn eval_dir [world ent] 
  (let [move_genome (:nextstep ent)
        nearest_same (nearest_same world ent)
        nearest_other (nearest_diff world ent)
        nearest_grass (nearest_grass world ent)
        random (rand_dir (:x ent) (:y ent))
        list_of_points [middlepoints nearest_same nearest_other nearest_grass random]
        list_of_flips (mapv second move_genome)
        list_of_mags (mapv first move_genome)]
        (final_direction (sum_vec_list (mapv mag_vector (mapv list_flipper (norm_vec_list (get_all_the_vectors list_of_points [(:x ent) (:y ent)])) list_of_flips) list_of_mags)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; C. SPAWN WORLD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; to set up the matrix that is the world
; 
; first, create a matrix full of empty cells
; second, pick out a list of indexes which will grow grass
;       then, recurse through the spots in the list and set grass to true on those
; third, randomly change some empty spots into predators
; fourth, randomly change some empty spots into prey

;;;;;;;;;; C.1. create empty matrix ;;;;;;;;;;;;;;

;; spawn empty cells
(defn fill_blanks []
  (vec (for [x (range size)]
         (vec (for [y (range size)]
                (Entity. "empty" x y nil false nil))))))

;;;;;;;;;;; C.2. helpful fxns ;;;;;;;;;;;

;; replace an item at certain index with entity
(defn replace_ent [mat x y newtype newnextstep grass ate]
  (assoc mat x (assoc (get mat x) y (Entity. newtype x y newnextstep grass ate))))

;; get entity at index x y
(defn get_ent [matrix x y]
  ((matrix x) y))

;; checking if xy is in bounds 
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

;;;;;;;;;;; C.3. adding grass ;;;;;;;;;;;

;; pick out some random instances to become grass
; in a diamond shape around 1 center
; yes i did it manualy, yes that was a bad idea
(defn diamond [x y]
  [[x y] [(+ 1 x) y] [x (+ 1 y)] [(+ 1 x) (+ 1 y)] [(+ 2 x) y] [x (+ 2 y)] [(- x 1) (- y 1)] [x (- y 1)] [x (- y 2)] [(+ 1 x) (- y 1)] [(- x 1) (+ y 1)] [(- x 1) y] [(- x 2) y]])

;; random chunk of grass that fits in size-constraints
; the constraints acutally suck, so they are reinforced later
(defn grs_spts [s]
  (diamond (rand-int (+ s 2)) (rand-int (+ s 2))))

;; specify how many chunks to add in world
(defn m_grs_spts [limit amount]
  (distinct (apply concat (for [i (range amount)] (grs_spts limit)))))

;; fill one block with grass
(defn onegrass [matrix xy]
  (if (in_bounds xy)
    (replace_ent matrix (xy 0) (xy 1) "empty" nil true nil)
    matrix))

;; fill up matrix with the diamond of grass
(defn recurgrass [matrix list]
  (if (empty? list)
    matrix
    (let [newmat (onegrass matrix (first list))
          newsp (rest list)]
      (recurgrass newmat newsp)))) ; dealing with bounds is getting annoying

;;;;;;;;;; C.4. randomly replace with some preds ;;;;;;;;;;;;;;

;; get what is in the cell and if it has grass
; only allowed to place a new predator if the spot is empty
; its way easier later to store them both in one item to reference
(defn type&grass_xy [mat x y]
  [(:type ((mat x) y)) (:grass ((mat x) y))])

;; add one new predator
(defn onepred [matrix]
  (let [newx (rand-int size)
        newy (rand-int size)
        check (type&grass_xy matrix newx newy)] ; here we make sure its empty
    (if (= (check 0) "empty")
      (replace_ent matrix newx newy "predator" (random_move_gene) (check 1) false)
      (onepred matrix))))

;; fill up matrix with random predators in random spots
; limited to the pre-set pred count
(defn recurpred [matrix count]
  (if (= count 0)
    matrix
    (let [newmat (onepred matrix)
          newcount (dec count)]
      (recurpred newmat newcount))))

;;;;;;;;;; C.5. randomly replace with some prey ;;;;;;;;;;;;;;
; works exactly the same as pred

;; add one new prey
(defn oneprey [matrix]
  (let [newx (rand-int size)
        newy (rand-int size)
        check (type&grass_xy matrix newx newy)]
    (if (= (check 0) "empty")
      (replace_ent matrix newx newy "prey" (random_move_gene) (check 1) false)
      (oneprey matrix))))

;; fill up matrix with random prey in random spots
; limited to the pre-set prey count
(defn recurprey [matrix count]
  (if (= count 0)
    matrix
    (let [newmat (oneprey matrix)
          newcount (dec count)]
      (recurprey newmat newcount))))

;;;;;;;;;; C.6. generate random world ;;;;;;;;;;;;;;

;; stack each step of preparing an empty world
; the let/if is because sometimes the world wouldn't work (i think problems with bounds I was too lazy to fix)
(defn random_world []
  (let [world (recurprey (recurpred (recurgrass (fill_blanks) (m_grs_spts size 2)) predator-count) prey-count)]
    (if world
      world
      (random_world))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; D. QUIL SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; I actually don't know how any of this works

;; Setup fxn to initialize the Quil sketch
(defn setup []
  (q/frame-rate frame-rate)
  (q/background 255)
  {:world (random_world)
   :frame 0
   :generation 0})

;; Fxn to draw the world
(defn draw-world [world frame gen]
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
        (q/rect (* (:x entity) 10) (* (:y entity) 10) 10 10)))
    )
  (q/fill 122 63 0)
  (q/text (str frame) 50 389)
  (q/text "frame:" 15 389)
  (q/text (str gen) 370 389)
  (q/text "gen:" 345 389))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; E. BREEDING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; get 2 lists (all preds and all prey)
; in each list:
;    shuffle
;    filter for only ones that did eat
;    pair them off
;    for each pair, create 1 new animal (this number should be adjustable)
;        for p, the animal should have a random value between each p
;               (chance of mutation + or - rand value)
;        for d, the animal should have a match if so or if diff a random one
;               (chance of mutation to flip it at the end)
;    place that animal in a random new spot




















;; given a world, breed the animals


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; F. NEXT STEP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; take a world and move every creature
; 
; first, get a list of the indexes of the creatures that currently exist
; 

;;;;;;;;;; F.1. movement decisions ;;;;;;;;;;

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

;; given an entity, evaluate it's nextstep fxn to determin the direction it wants to go it
; then return what the idex is in that direction
(defn wantstogo [ent world]
  (let [dir (eval_dir world ent)
        here (wantstogo_helper dir (:x ent) (:y ent))]
    ; cheking in bounds, else stay in spot
    (if (in_bounds here)
      here
      [(:x ent) (:y ent)])
    ))

;;;;;;;;;; F.2. movement helper fxns ;;;;;;;;;;

;; a pred wants to move into an available spot
(defn movepred [world ent new] 
  (let [oldgrass (:grass ent)] ; don't want to acidentaly move grass around
    (replace_ent (replace_ent world (:x new) (:y new) "predator" (:nextstep ent) (:grass new) (:ate ent)) (:x ent) (:y ent) "empty" nil oldgrass nil)))

;; a prey wants to move into an available spot
; splitting them up makes it easier to deal with grass
(defn moveprey [world ent new] 
  (let [oldgrass (:grass ent)] ; don't want to acidentaly move grass around
    (replace_ent (replace_ent world (:x new) (:y new) (:type ent) (:nextstep ent) false (if (true? oldgrass) ; check for grass
                                                                                          true
                                                                                          false)) (:x ent) (:y ent) "empty" nil oldgrass nil)))

;; a prey walks into a spot with a predator and gets eaten
(defn slayed [world idiot pred] ; later, add mechanism to extend pred life
  (replace_ent (replace_ent world (:x pred) (:y pred) "predator" (:nextstep pred) (:grass pred) true) (:x idiot) (:y idiot) "empty" nil false nil)) ; a prey would have eaten the grass

;; a pred walks into a prey and eats it
(defn slay [world pred prey] 
  (let [oldgrass (:grass pred)]
    (replace_ent (replace_ent world (:x prey) (:y prey) "predator" (:nextstep pred) false true) (:x prey) (:y prey) "empty" nil oldgrass nil)
      ))

;;;;;;;;;; F.3. grass-respawning ;;;;;;;;;;
; randomly spread in one direction
; subject to change, this seems like the best idea for now

;; given a list, get xs and ys offset by 1
; these will be the new xs and ys for the grass
; can actually reuse another helper
; added a shuffle and take 2 to limit the amount of growth
(defn w_newgrass [list]
  (filter in_bounds (mapv #(wantstogo_helper (rand-int 4) (% 0) (% 1)) (take grass_regrowth (shuffle list)))))

;; fill one block with grass
(defn newgrass [world ent]
  (replace_ent world (:x ent) (:y ent) (:type ent) (:nextstep ent) true (:ate ent)))

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

;;;;;;;;;; F.5. given a world, move everyone ;;;;;;;;;;
;
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

;; this fxn executes the movement with a list of the creatures
(defn move_with_list [world list]
  (if (empty? list) ; (recurgrass (fill_blanks) (m_grs_spts size 2))
    (recurnewgrass world (w_newgrass (w_grass_xys world)))
    (let [curent (get_ent world ((first list)0) ((first list)1)) ; getting entity info
          spotxy (wantstogo curent world) ; getting desired direction
          spot (get_ent world (spotxy 0) (spotxy 1))] ; getting what is there
      (cond
        (= (:type spot) "empty") 
        (if
          (= (:type curent) "prey") ; pred or prey
          (move_with_list (moveprey world curent spot) (rest list)) ; [world ent new] 
          (move_with_list (movepred world curent spot) (rest list))) ; [world ent new] 
        (= (:type spot) "prey")
        (if 
          (= (:type curent) "prey") ; pred or prey
          (move_with_list world (rest list))
          (move_with_list (slay world curent spot) (rest list)))
        (= (:type spot) "predator")
        (if (= (:type curent) "predator") ; pred or prey
          (move_with_list world (rest list))
          (move_with_list (slayed world curent spot) (rest list))
          )))))

;; given a world, get all the creatures, shuffle the order of the list, and call move_with_list
(defn move_in_world [world]
  (move_with_list world (shuffle (w_creat_xys world)))
  )

;; applying the steps forward on the state item
(defn steps-forward [state]
  (let [world (:world state)
        frame (:frame state)
        generation (:generation state)
        new-world (move_in_world world)]
    {:world new-world :frame (inc frame) :generation generation}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; G. TESTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(q/defsketch life
  :host "host"
  :size [(* size 10) (* size 10)]
  :setup setup
  :update (fn [state] (steps-forward state))
  :draw (fn [state] (draw-world (:world state) (:frame state) (:generation state)))
  :middleware [m/fun-mode])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; E. BREEDING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; get 2 lists (all preds and all prey)
; in each list:
;    shuffle
;    filter for only ones that did eat
;    pair them off
;    for each pair, create 1 new animal (this number should be adjustable)
;        for p, the animal should have a random value between each p
;               (chance of mutation + or - rand value)
;        for d, the animal should have a match if so or if diff a random one
;               (chance of mutation to flip it at the end)
;    place that animal in a random new spot




(defn breed-entities [world]
 ; 1. Extract lists of all predators and prey.
 ; 2. Shuffle each list, filter for only those that ate, and pair them off.
 ; 3. For each pair, create a new animal with randomized properties:
 ;      - For `p`, the property is a random value between the parents' values, with a chance of mutation.
 ;      - For `d`, the property is inherited directly or randomly with a chance of mutation.
 ; 4. Place the new animal in a random empty spot in the world.




; Step 1: Extract all creatures and grid size.
 (let [all-creatures (creats_in_w world) 
; Step 2: Separate creatures into two groups: predators and prey.              
       preds (filter #(= (:type %) "predator") all-creatures)
       prey  (filter #(= (:type %) "prey") all-creatures)
  ; Step 3: Shuffle the lists and filter for those that ate.
       shuffled-preds (shuffle preds)
       shuffled-prey  (shuffle prey)
       eligible-preds (filter :ate shuffled-preds)
       eligible-prey  (filter :ate shuffled-prey)
; Step 4: Pair them up for breeding.
       pairs-preds (partition 2 2 nil eligible-preds) ; Pair off predators
       pairs-prey  (partition 2 2 nil eligible-prey)  ;now prey
 ; Mutation settings to control randomness in breeding.
       mutation-chance 0.1 
       mutation-range 0.05 ; Random mutation adjustment range
;create a new offspring from a pair of creatures.
       create-offspring
       (fn [pair]
         (when (= 2 (count pair))
           (let [p1 (:p (first pair))
                 p2 (:p (second pair))
                 d1 (:d (first pair))
                 d2 (:d (second pair))
                 new-p (+ (rand-nth [p1 p2]) (* mutation-range (rand)))
                 new-d (if (= d1 d2) d1 (rand-nth [true false]))]
; Return the new offspring with its inherited and mutated properties.
             {:p new-p
              :d (if (< (rand) mutation-chance) (not new-d) new-d)}))) 
;place an offspring in a random empty spot in the grid.
       place-offspring
       (fn [offspring]
         (loop []
           (let [rand-x (rand-int size)
                 rand-y (rand-int size)]
             (if (nil? (get-in world [rand-y rand-x]))
               (assoc-in world [rand-y rand-x] offspring)
               (recur)))))]
; Step 5: Create offspring for each pair and place them in the world. 
   (reduce (fn [w pair]
             (if-let [offspring (create-offspring pair)]
               (place-offspring offspring)
               w))
           world
           (concat pairs-preds pairs-prey))))


(def tester_world (random_world))
(print tester_world)
(replace_ent (replace_ent tester_world 4 20 "predator" [[5 1] [4 0] [5 0] [2 1] [0 0]] false true) 20 4 "predator" [[5 1] [4 0] [5 0] [2 1] [0 0]] true true)
(breed-entities (replace_ent (replace_ent tester_world 4 20 "predator" [[5 1] [4 0] [5 0] [2 1] [0 0]] false true) 20 4 "predator" [[5 1] [4 0] [5 0] [2 1] [0 0]] true true))
































;; given a world, breed the animals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; F. NEXT STEP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; transsion the breeding function
;

(random_move_gene)


(defn step-forward [world frame-count]
; First, allow the creatures to breed.
 (let [world-after-breeding (breed-entities world)
; Next, move the creatures (assuming `move-entities` is defined elsewhere).
       world-after-movement (move-entities world-after-breeding)]
 ; Return the updated world after breeding and movement. 
   world-after-movement))
; take a world and move every creature
