(defn split_entities [world]

  ; get breeding list of predators
  (let [all-preds (filter #(= (:type %) "predator") (creats_in_w world))] ; get list of all predators
    (let [shuffled-preds (shuffle all-preds)] ; shuffle list of all predators
      (let [preds (filter :ate shuffled-preds)] ; get shuffled list of predators who have eaten
        ; pair up predators for breeding
        ; partition in sequences of 2, each starting 2 apart
        (let [pred-pairs (partition 2 2 preds)])
      )
    )
  )

  ; get breeding list of prey
  (let [all-prey (filter #(= (:type %) "prey") (creats_in_w world))] ; get list of all prey
    (let [shuffled-prey (shuffle all-prey)] ; shuffle list of all prey
      (let [prey (filter :ate shuffled-prey)] ; get shuffled list of prey who have eaten
        ; pair up predators for breeding
        ; partition in sequences of 2, each starting 2 apart
        (let [prey-pairs (partition 2 2 prey)])
      )
    )
  )

  ; TODO: create offspring
  (fn [breed])

  (let [pred-offspring ()])
  (let [prey-offspring ()])


  ; place offspring in a random location
  (loop [pred-offspring]
    let [newx (rand-int size)]
    let [newy (rand-int size)]
    ; TODO: change squares of the matrix to reflect frame change
  )
)