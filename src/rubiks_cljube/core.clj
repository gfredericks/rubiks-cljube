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
  counter-clockwise twist."
  (:require [clojure.set :as sets]))

(def solved
  {:corner-pos (vec (range 8)),
   :edge-pos   (vec (range 12)),
   :corner-or  (vec (repeat 8 0)),
   :edge-or    (vec (repeat 12 0))})

; TODO: in order to solve arbitrary pairs of positions in terms of solving to
; the normal solved state, we have to be able to consider the compound effect
; of several moves in an efficient manner. I think. That would also clear up
; the inefficiency of implementing F' as (comp F F F). And it would be more in
; line with the group representation of the rubik's cube.

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

(defn apply-moves
  [cube & move-keywords]
  (if (sequential? (first move-keywords))
    (apply apply-moves cube (first move-keywords))
    (reduce #(%2 %1) cube (map moves move-keywords))))

(let [orientable?
        (fn [{:keys [edge-or corner-or]}]
          (and (zero? (rem (apply + edge-or) 2))
               (zero? (rem (apply + corner-or) 3))))
      reducible-permutation?
        (fn [perm]
          (let [cycles
                  (loop [cycles []]
                    (if (= (count perm) (count (flatten cycles)))
                      cycles
                      (let [next-cycle-start
                              (first (remove (set (flatten cycles)) perm)),
                            next-cycle
                              (cons next-cycle-start
                                    (take-while (complement #{next-cycle-start})
                                                (rest (iterate perm next-cycle-start))))]
                        (recur (conj cycles next-cycle))))),
                has-even-elements? (comp even? count)]
            (has-even-elements? (filter has-even-elements? cycles))))
      positionable?
        (fn [{:keys [edge-pos corner-pos]}]
          (=
            (reducible-permutation? edge-pos)
            (reducible-permutation? corner-pos)))]
  (defn solvable?
    [cube]
    (and (orientable? cube) (positionable? cube))))

(defn random-cube*
  []
  {:corner-pos (vec (shuffle (range 8))),
   :edge-pos   (vec (shuffle (range 12))),
   :corner-or  (vec (take 8 (repeatedly (partial rand-int 3)))),
   :edge-or    (vec (take 12 (repeatedly (partial rand-int 2))))})

(defn random-cube
  []
  (let [c (random-cube*)]
    (if (solvable? c)
      c
      (recur))))

; I'm not sure we need this for anything
(defn reachable
  [cube max-moves]
  (nth
    (iterate
      (fn [reaches]
        (let [all-seen (apply sets/union reaches),
              new-cubes
                (for [cube (last reaches),
                      move (vals moves),
                      cube* [(move cube)],
                      :when (not (all-seen cube*))]
                  cube*)]
          (conj reaches (set new-cubes))))
      [#{cube}])
    max-moves))

(defn subgroup?
  "Returns true if the cube is part of the subgroup that only allows
  U and D quarter turns, and half turns on all other faces."
  [{:keys [edge-or corner-or]}]
  (and (every? zero? edge-or)
       (let [f (frequencies corner-or)]
         (= (f 1) (f 2)))))

(def valid-subgroup-moves #{:U :U' :U2
                            :D :D' :D2
                            :F2 :B2 :L2 :R2})

(defn random-subgroup-cube
  []
  (apply-moves solved (take 100 (repeatedly (partial rand-nth (seq valid-subgroup-moves))))))

(def valid-moves-following
  (memoize
    (fn vmf
      ([move] (vmf (keys moves) move))
      ([allowed-moves move]
        (if (nil? move)
          allowed-moves
          (let [move-class #(subs (name %) 0 1),
                forbidden
                  ({"U" #{"U"},
                    "D" #{"D" "U"},
                    "F" #{"F"},
                    "B" #{"F" "B"},
                    "L" #{"L"},
                    "R" #{"L" "R"}} (move-class move))]
            (remove #(forbidden (move-class %)) allowed-moves)))))))

(defn all-cubes-from
  "Returns an infinite seq of pairs of [moves cube], in order
  of length of the move list."
  ([cube] (all-cubes-from (keys moves) cube))
  ([allowed-moves cube]
    (for [depth (range),
          pair (all-cubes-from allowed-moves cube depth)]
      pair))
  ([allowed-moves cube depth]
    (if (zero? depth)
      [[[] cube]]
      (for [[move-vec cube*] (all-cubes-from allowed-moves cube (dec depth)),
            next-move (valid-moves-following allowed-moves (peek move-vec))]
        [(conj move-vec next-move) (apply-moves cube* next-move)]))))

(defn naive-brute-force
  "Returns a sequence of moves."
  ([cube satisfied?] (naive-brute-force (keys moves) cube satisfied?))
  ([allowed-moves cube satisfied?]
    (->>
      (all-cubes-from allowed-moves cube)
      (filter #(satisfied? (second %)))
      (first)
      (first))))

(defn invert
  "Returns the sequence of moves that undoes the one given."
  [moves]
  (for [move (reverse moves)]
    (or
      ({:R :R' :R' :R,
        :F :F' :F' :F,
        :D :D' :D' :D,
        :B :B' :B' :B,
        :L :L' :L' :L,
        :U :U' :U' :U} move)
      move)))

(let [tw (take-while #(< (count (first %)) 7) (all-cubes-from valid-subgroup-moves solved)),
      sixes (delay (zipmap (map second tw) (map first tw)))]
  (defn solve-subgroup
    "Given a cube in the subgroup, returns a sequence of moves that will solve it."
    [cube]
    {:pre [(subgroup? cube)]}
    (let [mvs (naive-brute-force valid-subgroup-moves cube #(contains? @sixes %))]
      (concat
        mvs
        (invert
          (@sixes (apply-moves cube mvs)))))))

(defn print-cube
  [cube]
  )
