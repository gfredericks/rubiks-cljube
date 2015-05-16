(ns rubiks-cljube.test.core
  (:use [rubiks-cljube.core])
  (:use [clojure.test])
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(deftest basic-cube-mechanics-test
  (are [move inverse]
       (= solved
          (-> solved ((moves move)) ((moves inverse)))
          (-> solved ((moves inverse)) ((moves move))))
       :R :R'
       :L :L'
       :D :D'
       :U :U'
       :F :F'
       :B :B'
       :R2 :R2
       :L2 :L2
       :D2 :D2
       :U2 :U2
       :F2 :F2
       :B2 :B2)
  (let [move-seq
        ; Wikipedia says this twists two corners
          (map moves [:B :R' :D2 :R :B' :U2 :B :R' :D2 :R :B' :U2]),
        {:keys [corner-pos edge-pos corner-or edge-or]}
          (reduce #(%2 %1) solved move-seq)]
    (is (= (:corner-pos solved) corner-pos))
    (is (= (:edge-pos solved) edge-pos))
    (is (= (:edge-or solved) edge-or))
    (is (not= (:corner-or solved) corner-or))
    (is (= 6 (count (filter #{0} corner-or))))))

(deftest solvable?-test
  (is (solvable? solved))
  (doseq [cube (apply concat (reachable solved 2))]
    ; Any cube we can turn to is solvable
    (is (solvable? cube))
    ; Flipping an edge on a solvable cube is always not solvable
    (is (not (solvable? (update-in cube [:edge-or 7] (partial - 1)))))))

(deftest naive-brute-force-test
  (are [moves] (= (invert-moves moves)
                  (naive-brute-force
                    (apply-moves solved moves)
                    (partial = solved)))
    [:U2 :L' :D]
    [:D' :F' :R']
    [:U  :B' :L2]
    [:R' :B]
    [:L]
    []))


;;
;; Property-based tests
;;

(def gen-move
  (gen/elements [:L :F2 :R :F' :F :L'
                 :D' :D :B :L2 :R2 :B'
                 :B2 :D2 :U2 :U :R' :U']))

(def gen-cube
  (gen/fmap #(apply-moves solved %) (gen/list gen-move)))

(def gen-rotation (gen/elements all-rotations))


(defspec rotation-and-inversion-commute 500
  (prop/for-all [move gen-move
                 rotation gen-rotation]
    (= (-> move invert-move rotation)
       (-> move rotation invert-move))))

(defspec scramble-and-solve 100
  (prop/for-all [moves (gen/list gen-move)]
    (= solved (-> solved
                  (apply-moves moves)
                  (apply-moves (invert-moves moves))))))
