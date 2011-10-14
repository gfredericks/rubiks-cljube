(ns rubiks-cljube.test.core
  (:use [rubiks-cljube.core])
  (:use [clojure.test]))

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
