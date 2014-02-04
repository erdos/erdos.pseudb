(ns erdos.pseudb.core-test
  (:use clojure.test)
  (:require [erdos.pseudb :as ps])
  )

(deftest creation
  (testing "Empty structure"
    (is (not (nil? (ps/create)))))
  (testing "Simple index"
    (is (not (nil? (ps/create (INDEX [:a :b :c]))))))
  (testing "unique index")
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


(deftest ins-merg
  (let [db (ps/create (UNIQUE :a))
        db (ps/insert db {:a 1} {:a 2})
        old (comp second list)
        neu (comp first list)
        insert (partial ps/insert-merge db)]
    (testing "null db"
      (is (thrown?
           NullPointerException
           (ps/insert-merge nil {:a 1} old))))
    (testing "no collision"
      (is (= (inc (ps/size db))
             (ps/size (insert {:a 3} old))))
      (is (= (inc (ps/size db))
             (ps/size (insert {:x 5} old)))))
    (testing "first rank collision"
      (is (= (ps/size db)
             (ps/size (insert {:a 2} old))
             (ps/size (insert {:a 2} neu)))))))

(comment

  (ps/insert-merge
   (ps/insert
    (ps/create (UNIQUE :a))
    {:a 1} {:a 2} {:a 3})
   {:a 1 :b 1}
   (comp first list))

    (ps/ffind-collision
     (ps/insert
      (ps/create (UNIQUE :a)
                 (UNIQUE :b))
      {:a 1} {:a 2} {:a 3} {:b 2})
     {:b 2 :a 1})



  )
