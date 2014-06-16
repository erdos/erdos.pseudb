(ns erdos.pseudb.core-test
  (:use clojure.test)
  (:require [erdos.pseudb :as ps])
  )

(deftest creation
  (testing "Empty structure"
    (is (not (nil? (ps/create)))))
  (testing "Simple index"
    (is (not (nil? (ps/create (INDEX :a :b :c))))))
  (testing "unique index")
  ;; ...
  )

(deftest index-simple
  "adasddas"
  (let [db (ps/create (INDEX :a))
        xs '({:a 1 :b 2} {:a 2 :b 3} {:a 3 :b 4})
        db (apply ps/insert db xs)]

    (testing "access with index"
      (are [x] (= 1 (count (ps/ffind db x)))
           {:a 1} {:a 2} {:a 3})
      (is (= (first (ps/ffind db {:a 1}))
             (first (ps/ffind db {:a 1 :b 2}))
             {:a 1 :b 2})))

    (testing "acces without index"
      (is (= 1 (count (ps/ffind db {:b 2}))))
      (is (= (first (ps/ffind db {:b 2}))
             {:a 1 :b 2})))

    (testing "should not find any"
      (is (empty? (ps/ffind db {:c 12})))
      (is (empty? (ps/ffind db {:a 1 :b 3}))))

    (testing "find all items"
      (is (= 3 (count (ps/ffind db {})))))

    (testing "")))


(deftest index-unique
  "UNIQUE index testing"
 (let [db (ps/create (UNIQUE :a))
        xs '({:a 1 :b 2} {:a 2 :b 3} {:a 3 :b 4})
        db (apply ps/insert db xs)]

   (testing "access with index"
      (are [x] (= 1 (count (ps/ffind db x)))
           {:a 1} {:a 2} {:a 3})
      (is (= (first (ps/ffind db {:a 1}))
             (first (ps/ffind db {:a 1 :b 2}))
             {:a 1 :b 2})))

   (testing "acces without index"
      (is (= 1 (count (ps/ffind db {:b 2}))))
      (is (= (first (ps/ffind db {:b 2}))
             {:a 1 :b 2})))

   (testing "should not find any"
      (are [x] (empty? (ps/ffind db x))
           {:c 12} {:a 4}))

   (testing "find all items"
      (is (= 3 (count (ps/ffind db {})))))

   (testing "can not insert twice"
      (is (nil? (ps/insert db {:a 1})))
      (is (nil? (ps/insert db {:a 1 :x :x}))))

   (testing "")))

(deftest index-simple
  "adasddas"
  (let [db (ps/create (INDEX :a))
        xs '({:a 1 :b 2} {:a 2 :b 3} {:a 3 :b 4})
        db (apply ps/insert db xs)]

    (testing "access with index"
      (are [x] (= 1 (count (ps/ffind db x)))
           {:a 1} {:a 2} {:a 3})
      (is (= (first (ps/ffind db {:a 1}))
             (first (ps/ffind db {:a 1 :b 2}))
             {:a 1 :b 2})))

    (testing "acces without index"
      (is (= 1 (count (ps/ffind db {:b 2}))))
      (is (= (first (ps/ffind db {:b 2}))
             {:a 1 :b 2})))

    (testing "should not find any"
      (is (empty? (ps/ffind db {:c 12})))
      (is (empty? (ps/ffind db {:a 1 :b 3}))))

    (testing "find all items"
      (is (= 3 (count (ps/ffind db {})))))

    (testing "")))


(deftest index-unique
  "multiple insert"
  (testing "insertions happen"
    (-> (ps/create (UNIQUE :a))
        (ps/insert {:a 1})
        (ps/insert {:b 2})
        (count)
        (= 2)))
  (testing "compound index"
    (-> (ps/create (UNIQUE :b :a))
        (ps/insert {:a 1 :b 1})
        (ps/insert {:a 1 :b 2})
        (count)
        (= 2)))
  (testing "compound index"
    (-> (ps/create (INDEX :a :b))
        (ps/insert {:a 1 :b 1})
        (ps/insert {:a 1 :b 2})
        (count)
        (= 2))))


(comment
  (let [s (ps/create (INDEX :b))]
    [(seq s) (count s) (.cnt s)])
  (let [s (ps/create (INDEX :b))]
    (ps/insert s {:c 1}))

  (-> (ps/create)
      (ps/insert {:c 1})
      (ps/ffind {:c 1}))

  (-> (ps/create (INDEX :c))
      (ps/insert {:c 1})
      (ps/ffind {:c 1}))

  (-> (ps/create (UNIQUE :c))
      (ps/insert {:c 1})
      (ps/ffind {:c 1}))

  (-> (ps/create (UNIQUE :d))
      (ps/insert {:c 1})
      (ps/ffind {:c 1}))




  ;; should give 2 elems.
  (-> (ps/create (UNIQUE :b))
      (ps/insert {:c 1})
      (ps/insert {:d 1})
      count)
  ;; should give 2 elems.
  (-> (ps/create (INDEX :b))
      (ps/insert {:c 1})
      (ps/insert {:d 1})
      count)

  (let [s (ps/create (UNIQUE :b))]
    (ps/insert s {:c 1}))
  )
