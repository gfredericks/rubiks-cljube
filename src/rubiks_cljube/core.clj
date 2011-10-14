(ns rubiks-cljube.core
  "Canonical orientation:
    front  := blue
    back   := green
    top    := yellow
    bottom := white
    left   := orange
    right  := red
  
  Corners and edges are both numbered from 0, by layers starting
  at the top. Within each layer, start either on the front left
  or front center as applicable and move around the layer counter-
  clockwise. Orientations are wrt the front and back (blue/green).
  Edge orientations are 0 for correct and 1 for flipped. Corner
  orientations are 0 for correct, 1 for clockwise twist, 2 for
  counter-clockwise twist.")

(def solved
  {:corner-pos (vec (range 8)),
   :edge-pos   (vec (range 12)),
   :corner-or  (vec (repeat 8 0)),
   :edge-or    (vec (repeat 12 0))})

(let [circular-pair-partition
        (fn [coll] (partition 2 1 (concat coll [(first coll)])))
      rotate-corners
        (fn [cube & corners]
          (reduce
            (fn [cube' [c1 c2]]
              (->
                cube'
                (assoc-in [:corner-pos c2] (get-in cube [:corner-pos c1]))
                (assoc-in [:corner-or  c2] (get-in cube [:corner-or c1]))))
            cube
            (circular-pair-partition corners))),
      rotate-edges
        (fn [cube & edges]
          (reduce
            (fn [cube' [e1 e2]]
              (->
                cube'
                (assoc-in [:edge-pos e2] (get-in cube [:edge-pos e1]))
                (assoc-in [:edge-or  e2] (get-in cube [:edge-or e1]))))
            cube
            (circular-pair-partition edges))),
      twist
        (fn [direction cube & corners]
          (reduce
            (fn [cube corner]
              (update-in cube [:corner-or corner] #(rem (+ % direction 3) 3)))
            cube
            corners)),
      twist-cw (partial twist 1),
      twist-ccw (partial twist -1)
      flip-edges
        (fn [cube & edges]
          (reduce
            (fn [cube edge]
              (update-in cube [:edge-or edge] #(- 1 %)))
            cube
            edges)),
      R #(-> %
           (rotate-corners 1 2 6 5)
           (rotate-edges 1 6 9 5)
           (twist-cw 1 6)
           (twist-ccw 2 5)),
      U #(-> %
           (rotate-corners 0 3 2 1)
           (rotate-edges 0 3 2 1)
           (twist-cw 0 2)
           (twist-ccw 1 3)),
      L #(-> %
           (rotate-corners 3 0 4 7)
           (rotate-edges 3 4 11 7)
           (twist-cw 3 4)
           (twist-ccw 0 7))
      D #(-> %
           (rotate-corners 4 5 6 7)
           (rotate-edges 8 9 10 11)
           (twist-cw 5 7)
           (twist-ccw 4 6))
      F #(-> %
           (rotate-corners 0 1 5 4)
           (rotate-edges 0 5 8 4)
           (flip-edges 0 5 8 4))
      B #(-> %
           (rotate-corners 2 3 7 6)
           (rotate-edges 2 7 10 6)
           (flip-edges 2 7 10 6))]
  (def moves
    {:R R, :R2 (comp R R), :R' (comp R R R),
     :U U, :U2 (comp U U), :U' (comp U U U),
     :L L, :L2 (comp L L), :L' (comp L L L),
     :D D, :D2 (comp D D), :D' (comp D D D),
     :F F, :F2 (comp F F), :F' (comp F F F),
     :B B, :B2 (comp B B), :B' (comp B B B)}))
