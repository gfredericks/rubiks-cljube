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
       :B2 :B2))
