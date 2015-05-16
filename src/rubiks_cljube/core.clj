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

(defprotocol ICube
  (cube->data [_]
    "Returns a map with keys
      :corner-pos
      :edge-pos
      :corner-or
      :edge-or
     Each is a vector of numbers"))

(defrecord DataCube [corner-pos edge-pos corner-or edge-or]
  ICube
  (cube->data [this] this))

(def solved
  (map->DataCube
   {:corner-pos (vec (range 8)),
    :edge-pos   (vec (range 12)),
    :corner-or  (vec (repeat 8 0)),
    :edge-or    (vec (repeat 12 0))}))

(def base-moves
  "The positions resulting from the six base moves performed on a
  solved cube."
  {:B #rubiks_cljube.core.DataCube{:corner-or  [0 0 0 0 0 0 0 0],
                                   :corner-pos [0 1 6 2 4 5 7 3],
                                   :edge-or    [0 0 1 0 0 0 1 1 0 0 1 0],
                                   :edge-pos   [0 1 6 3 4 5 10 2 8 9 7 11]},
   :D #rubiks_cljube.core.DataCube{:corner-or  [0 0 0 0 2 1 2 1],
                                   :corner-pos [0 1 2 3 7 4 5 6],
                                   :edge-or    [0 0 0 0 0 0 0 0 0 0 0 0],
                                   :edge-pos   [0 1 2 3 4 5 6 7 11 8 9 10]},
   :F #rubiks_cljube.core.DataCube{:corner-or  [0 0 0 0 0 0 0 0],
                                   :corner-pos [4 0 2 3 5 1 6 7],
                                   :edge-or    [1 0 0 0 1 1 0 0 1 0 0 0],
                                   :edge-pos   [4 1 2 3 8 0 6 7 5 9 10 11]},
   :L #rubiks_cljube.core.DataCube{:corner-or  [2 0 0 1 1 0 0 2],
                                   :corner-pos [3 1 2 7 0 5 6 4],
                                   :edge-or    [0 0 0 0 0 0 0 0 0 0 0 0],
                                   :edge-pos   [0 1 2 7 3 5 6 11 8 9 10 4]},
   :R #rubiks_cljube.core.DataCube{:corner-or  [0 1 2 0 0 2 1 0],
                                   :corner-pos [0 5 1 3 4 6 2 7],
                                   :edge-or    [0 0 0 0 0 0 0 0 0 0 0 0],
                                   :edge-pos   [0 5 2 3 4 9 1 7 8 6 10 11]},
   :U #rubiks_cljube.core.DataCube{:corner-or  [1 2 1 2 0 0 0 0],
                                   :corner-pos [1 2 3 0 4 5 6 7],
                                   :edge-or    [0 0 0 0 0 0 0 0 0 0 0 0],
                                   :edge-pos   [1 2 3 0 4 5 6 7 8 9 10 11]}})

(defn result-cube->move-function
  "Given the cube that results from applying a sequence of moves to
  the solved cube, returns a function which can efficiently apply the
  same sequence of moves to an arbitrary cube."
  [{:keys [corner-or corner-pos edge-or edge-pos]}]
  (let [edges-moved (->> edge-pos
                         (map-indexed vector)
                         (remove (fn [[i v]] (= i v)))
                         (map (fn [[i v]] {:from v :to i})))
        edges-oriented (->> edge-or
                            (map-indexed vector)
                            (remove (fn [[i v]] (zero? v)))
                            (map (fn [[i v]] {:pos i :inc 1})))
        corners-moved (->> corner-pos
                           (map-indexed vector)
                           (remove (fn [[i v]] (= i v)))
                           (map (fn [[i v]] {:from v :to i})))
        corners-oriented (->> corner-or
                              (map-indexed vector)
                              (remove (fn [[i v]] (zero? v)))
                              (map (fn [[i v]] {:pos i :inc v})))]
    (fn [cube]
      (as-> cube <cube>
            (reduce (fn [<cube> {:keys [from to]}]
                      (-> <cube>
                          (assoc-in [:edge-pos to]
                                    (get-in cube [:edge-pos from]))
                          (assoc-in [:edge-or to]
                                    (get-in cube [:edge-or from]))))
                    <cube>
                    edges-moved)
            (reduce (fn [<cube> {:keys [from to]}]
                      (-> <cube>
                          (assoc-in [:corner-pos to]
                                    (get-in cube [:corner-pos from]))
                          (assoc-in [:corner-or to]
                                    (get-in cube [:corner-or from]))))
                    <cube>
                    corners-moved)
            (reduce (fn [<cube> {:keys [pos inc]}]
                      (update-in <cube> [:edge-or pos] #(-> % (+ inc) (mod 2))))
                    <cube>
                    edges-oriented)
            (reduce (fn [<cube> {:keys [pos inc]}]
                      (update-in <cube> [:corner-or pos] #(-> % (+ inc) (mod 3))))
                    <cube>
                    corners-oriented)))))

(def moves
  (let [rcmfbm #(result-cube->move-function (base-moves %))
        the-six
        {:F (rcmfbm :F)
         :B (rcmfbm :B)
         :L (rcmfbm :L)
         :R (rcmfbm :R)
         :U (rcmfbm :U)
         :D (rcmfbm :D)}
        double #(result-cube->move-function
                 (let [f (the-six %)]
                   (f (f solved))))
        triple #(result-cube->move-function
                 (let [f (the-six %)]
                   (f (f (f solved)))))]
    (assoc the-six
      :F' (triple :F)
      :B' (triple :B)
      :L' (triple :L)
      :R' (triple :R)
      :U' (triple :U)
      :D' (triple :D)
      :F2 (double :F)
      :B2 (double :B)
      :L2 (double :L)
      :R2 (double :R)
      :U2 (double :U)
      :D2 (double :D))))

(defn apply-move
  [cube move-keyword]
  ((moves move-keyword) cube))

(defn apply-moves
  [cube move-keywords]
  (reduce apply-move cube move-keywords))

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
        [(conj move-vec next-move) (apply-move cube* next-move)]))))

(defn naive-brute-force
  "Returns a sequence of moves."
  ([cube satisfied?] (naive-brute-force (keys moves) cube satisfied?))
  ([allowed-moves cube satisfied?]
    (->>
      (all-cubes-from allowed-moves cube)
      (filter #(satisfied? (second %)))
      (first)
      (first))))

(defn invert-move
  [move]
  ({:R :R' :R' :R,
    :F :F' :F' :F,
    :D :D' :D' :D,
    :B :B' :B' :B,
    :L :L' :L' :L,
    :U :U' :U' :U} move move))

(defn invert-moves
  [moves]
  (->> moves
       (map invert-move)
       (reverse)))

(let [tw (take-while #(< (count (first %)) 7) (all-cubes-from valid-subgroup-moves solved)),
      sixes (delay (zipmap (map second tw) (map first tw)))]
  (defn solve-subgroup
    "Given a cube in the subgroup, returns a sequence of moves that will solve it."
    [cube]
    {:pre [(subgroup? cube)]}
    (let [mvs (naive-brute-force valid-subgroup-moves cube #(contains? @sixes %))]
      (concat
        mvs
        (map invert-move (@sixes (apply-moves cube mvs)))))))

(defn print-cube
  [cube]
  )


;;
;; Bit-twiddly impl
;;

;; a 60-bit number: the lowest 48 bits are 12 4-bit numbers
;; representing the edge piece at the given position, and bits 48-59
;; are 12 booleans representing the orientation of the edge.
(def solved-edges 2r000000000000101110101001100001110110010101000011001000010000)

(defmacro get-edge-pos
  [edges i]
  `(-> ~edges (bit-shift-right (* 4 ~i)) (bit-and 15)))

(defmacro get-edge-ori
  [edges i]
  `(-> ~edges (bit-shift-right (+ 48 ~i)) (bit-and 1)))

(defmacro set-edge-pos
  [edges i v]
  `(let [shamt# (* ~i 4)
         v# (bit-shift-left ~v shamt#)
         mask# (-> 15 (bit-shift-left shamt#) (bit-not))]
     (-> ~edges
         (bit-and mask#)
         (bit-or v#))))

(defmacro set-edge-ori
  [edges i v]
  `(let [shamt# (+ ~i 48)
         v# (bit-shift-left ~v shamt#)
         mask# (-> 1 (bit-shift-left shamt#) (bit-not))]
     (-> ~edges
         (bit-and mask#)
         (bit-or v#))))

(defn decode-edges
  [^long edges]
  {:pos (mapv (fn [i] (get-edge-pos edges i)) (range 12))
   :or (mapv (fn [i] (get-edge-ori edges i)) (range 12))})

(def edge-moves
  (eval
   (into {}
         (for [[move-name func] moves
               :let [{:keys [edge-pos edge-or]} (func solved)
                     edges-moved (->> edge-pos
                                      (map-indexed vector)
                                      (remove (fn [[i v]] (= i v))))
                     edges-flipped (->> edge-or
                                        (map-indexed vector)
                                        (remove (fn [[i v]] (zero? v)))
                                        (map first))
                     edges-sym (gensym "edges")]]
           [move-name
            `(fn [~edges-sym]
               (-> ~edges-sym
                   ~@(for [[moved-to moved-from] edges-moved]
                       `(set-edge-pos ~moved-to (get-edge-pos ~edges-sym ~moved-from)))
                   (as-> ~edges-sym
                         (-> ~edges-sym
                             ~@(for [i edges-flipped]
                                 `(set-edge-ori ~i (- 1 (get-edge-ori ~edges-sym ~i))))))))]))))

(defn rotate-move-BG
  "Rotates a move around the blue-green axis."
  [move-kw]
  (case move-kw
    :U :R :U' :R' :U2 :R2
    :R :D :R' :D' :R2 :D2
    :D :L :D' :L' :D2 :L2
    :L :U :L' :U' :L2 :U2
    move-kw))

(defn rotate-move-WY
  "Rotates a move around the white-yellow axis."
  [move-kw]
  (case move-kw
    :F :L :F' :L' :F2 :L2
    :L :B :L' :B' :L2 :B2
    :B :R :B' :R' :B2 :R2
    :R :F :R' :F' :R2 :F2
    move-kw))

(defn rotate-move-OR
  "Rotates a move around the orange-red axis."
  [move-kw]
  (case move-kw
    :F :U :F' :U' :F2 :U2
    :U :B :U' :B' :U2 :B2
    :B :D :B' :D' :B2 :D2
    :D :F :D' :F' :D2 :F2
    move-kw))

(def all-rotations [rotate-move-BG
                    rotate-move-OR
                    rotate-move-WY])

;;
;; Developing a new solution
;;

(def edge-mucker
  [:R' :L :F' :R' :F
   :R :L' :U' :R :U])

(def mirrored-edge-mucker
  [:L :R' :F :L :F'
   :L' :R :U :L' :U'])

;; how much coverage does it have??

(def all-edge-muckers
  (loop [ret #{edge-mucker mirrored-edge-mucker}
         to-check [edge-mucker mirrored-edge-mucker]]
    (if-let [[x & xs] (seq to-check)]
      (let [new-things (->> all-rotations
                            (map #(map % x))
                            (remove ret))]
        (recur (into ret new-things) (into xs new-things)))

      ret)))

(defn edge-mucker-reachable
  [max-moves]
  (loop [ret {solved []}
         last-generation [{:cube solved :moves []}]
         moves-done 0]
    (if (= max-moves moves-done)
      ret
      (let [next-generation
            (->> last-generation
                 (mapcat (fn [{:keys [cube moves]}]
                           (->> all-edge-muckers
                                (map (fn [an-edge-mucker]
                                       {:cube (apply-moves cube an-edge-mucker)
                                        :moves (into moves an-edge-mucker)})))))
                 (remove (comp ret :cube)))]
        (recur (into ret (map (juxt :cube :moves) next-generation))
               next-generation
               (inc moves-done))))))

;; Could we do a hill-climbing algorithm that looks for ways to get
;; edges in the right places?


(comment
  (def random-edge-mucked-cube
    (->> (repeatedly 1000 #(rand-nth (seq all-edge-muckers)))
         (reduce apply-moves solved))
    #_
    #rubiks_cljube.core.DataCube{:corner-or  [0 0 0 0 0 0 0 0],
                                 :corner-pos [0 1 2 3 4 5 6 7],
                                 :edge-or    [0 1 1 1 0 0 1 1 0 0 1 0],
                                 :edge-pos   [5 1 4 6 8 9 2 3 11 10 7 0]})

  (defn solved-edges-count
    [cube]
    (->> (map (fn [idx pos or]
                (and (= idx pos) (zero? or)))
              (range)
              (:edge-pos cube)
              (:edge-or cube))
         (filter identity)
         (count)))

  (def all-two-move-edge-muckers
    (concat all-edge-muckers
            (for [em1 all-edge-muckers em2 all-edge-muckers]
              (concat em1 em2))))

  (def cube-1
    (->> all-two-move-edge-muckers
         (map #(apply-moves random-edge-mucked-cube %))
         (filter #(= 1 (solved-edges-count %)))
         (rand-nth)))

  (def cube-2
    (->> all-two-move-edge-muckers
         (map #(apply-moves cube-1 %))
         (filter #(< 1 (solved-edges-count %)))
         (rand-nth)))

  (def cube-3
    (->> all-two-move-edge-muckers
         (map #(apply-moves cube-2 %))
         (filter #(< 2 (solved-edges-count %)))
         (rand-nth)))

  (def cube-4
    (->> all-two-move-edge-muckers
         (map #(apply-moves cube-3 %))
         (filter #(< 3 (solved-edges-count %)))
         (rand-nth)))

  (def cube-5
    (->> all-two-move-edge-muckers
         (map #(apply-moves cube-4 %))
         (filter #(< 4 (solved-edges-count %)))
         (rand-nth)))

  (def cube-6
    (->> all-two-move-edge-muckers
         (map #(apply-moves cube-5 %))
         (filter #(< 5 (solved-edges-count %)))
         (rand-nth)))

  (def cube-7
    (->> all-two-move-edge-muckers
         (map #(apply-moves cube-6 %))
         (filter #(< 6 (solved-edges-count %)))
         (rand-nth)))

  (def threes (edge-mucker-reachable 3))

  (->> all-two-move-edge-muckers
       (map #(apply-moves cube-6 %))
       (filter threes)
       (count)))
