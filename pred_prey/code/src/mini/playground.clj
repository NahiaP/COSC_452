; Load Packages
(ns mini.playground)

; Globals

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                           TO-DO                          ;

; create basic predator
; create basic prey
; see if they can evolve in tandem

; Predator has...
  ; speed      (1-10)
; [s]


; Prey has...
  ; speed      (1-10)
; [s]

; note, later integrate x and y

; build initial populations
 ; 100 prey animals
 ; 100 predator animals

; selection
 ; round 1
  ; randomly grab 1 prey and one predator
  ; compare speeds, winner lives
 ; round 2
  ; prey animals must get a plant (if any left)
  ; for every prey alive, one less plant

; breeding
 ; pairs of leftover 

; main loop
 ; randomly pair up entire population and tournament
 ; with prey left, eat one gets to eat a grass
 ; breed leftover pred (crossover), prey(crossover), and grass(+5, *2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                    CREATING POPULATION                   ;

; in this initial very simple population generation, we just want 100 of each value
; in the furture, an individual will likely have more features involved in it 

(defn new-pop []
  (repeatedly 100 #(+ (rand-int 10) 1)))

; test

; (new-pop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                         SELECTION                        ;

;ROUND 1

(defn eat [x]
  (if (< (nth x 0) (nth x 1)) [0 (nth x 1)] [(nth x 0) 0]))

(defn eat2 [zipped]
  (for [x zipped]
    (if (< (nth x 0) (nth x 1)) [0 (nth x 1)] [(nth x 0) 0])
    ))

(defn the_hunt [pred prey]
  (eat2 (map vector pred prey)))

(the_hunt (new-pop) (new-pop))

(defn getpred [all]
  (for [x all]
    (nth x 0)))

(defn getprey [all]
   (for [x all]
          (nth x 1)))

; figuring out how to remove the dead bodies
;(remove #{0} (getprey [[1 0] [0 2]]))

; ROUND 2 (for prey)

; given a set of prey, shuffle and feed

(defn prey_eat [prey grass]
  take grass (prey))

;(prey_eat (new-pop) 50)

(defn grassleft [grass prey_n]
  (if (< grass prey_n) [0] [(- grass prey_n)]))

(grassleft 50 100)
(grassleft 100 50)
(grassleft 50 50)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                         BREEDING                         ;

; given a population, pair them randomly, 
; make 2 babies each with some speed value between the 2
; if there is 1 stragller they do not reproduce

;(map sort (partition 2 [1 2 4 3 5]))
;(nth [1 2] 0)

(defn breed [pop]
  (for [x (map sort(partition 2 pop))]
    (+ (rand-int (+ (- (nth x 1)(nth x 0)) 1)) (nth x 0))))

(breed [1 2 3 4 5])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                         EVO LOOP                         ; 

; evolve for certain amount of generations
(defn evolve
  [gens]
  (println "Starting evolution...")
  (loop [generation 0
         pred_pop (new-pop)
         prey_pop (new-pop)
         grass 200] 
    (let [count_pred (count pred_pop)
          count_prey (count prey_pop)
          ;top_pred (max (pred_pop))
          ;top_prey (max (prey_pop))
          ]
      (println "======================")
      (println "Generation:" generation)
      (println "Count pred:" count_pred)
      ;(println "Top pred speed:" top_pred)
      (println "Count prey:" count_prey)
      ;(println "Top prey speed:" top_prey)
      (println "Grass left:" grass)
     
      (if (> generation gens) ;; the cycle will stop after certain amount of gens
        (println "Done")
        (recur
         (inc generation)
         (shuffle (conj
                   (breed 
                    (getpred 
                     (the_hunt pred_pop prey_pop)))
                   (breed 
                    (getpred 
                     (the_hunt pred_pop prey_pop)))))
         (shuffle (conj 
                   (breed 
                    (prey_eat 
                     (getprey 
                      (the_hunt pred_pop prey_pop)) 
                     grass))
                   (breed
                    (prey_eat
                     (getprey
                      (the_hunt pred_pop prey_pop))
                     grass))
                   ))
         200
         )))))

(evolve 10)