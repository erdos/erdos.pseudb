(ns erdos.pseudb)

(defn assocf
  ([m k f]
     (assoc m (f (get m k))))
  ([m k f d]
     (if (contains? m k)
       (assoc m k(f (get m k)))
       (assoc m k d))))


(defprotocol IndexStrategy
  ;; @returns (possibly empty) seq of result indices or
  ;; nil when index not compatible with where map.
  (find- [_ where-map])

  ;; @returns modified index or nil when not compatible.
  (adds- [_ obj-to-insert index-to-insert])

  ;; removes i where old-map matches.
  ;; @returns modified index or nil when not compatible.
  ;; @throws IllegalStateException when compatible but m does not map to i
  (remv- [_ m i]))


(defn +MultiIndex
  "Strategy for not unique, nil allowed index."
  [ks]
  (assert (coll? ks))
  (assert (every? keyword? ks))
  (letfn [(-vls [obj] (vec (map obj ks)))
          (-neue [mp]
            (reify IndexStrategy
              (find- [_ m]
                (if (every? m ks)
                  (get mp (-vls m) [])))
              (adds- [_ m i]
                (if (every? m ks)
                  (-neue (assocf mp (-vls m) #(conj % i) #{i}))))
              (remv- [_ old i]
                (if (every? old ks)
                  (let [v (-vls old)]
                    (-neue (assoc mp v (disj (get mp v) i))))))))]
    (-neue {})))


(defn +UniqueIndex
  "Strategy for not unique index. throws exception on overwrite."
  [ks]
  (assert (coll? ks))
  (assert (every? keyword? ks))
  (letfn [(-vls [obj] (vec (map obj ks)))
          (-neue [mp]
            (reify IndexStrategy
              (find- [_ m]
                (if (every? m ks)
                  (if-let [i (get mp (-vls m))] [i] [])))
              (adds- [_ m i]
                (if (every? mp ks)
                  (let [v (-vls m)]
                    (if-not (contains? mp v)
                      (-neue (assoc mp v i))
                      (-> "index values are in use!"
                          IllegalStateException. throw)))))
              (remv- [_ m i]
                :NOT-IMPLED)))]
    (-neue {})))


                                        ; create

(defn- create-index
  [[k v]]
  (({'INDEX +MultiIndex
     'UNIQUE +UniqueIndex} k) v))

(defn- create* [indices]
  {:data {}, :cnt 0, :indices (map create-index indices)})


(defmacro create [& indices]
  `(create* '~indices))

                                        ; read

(defn- find*
  [db w]
  (let [ks (map #(find- % w) (:indices db))]
    (if-not (every? nil? ks)
      (map (:data db) (set (apply concat ks)))
      (filter (partial clojure.set/subset? w)
              (-> db :data vals)))))


(defmacro ffind
  "Find all occurences in db. When indices do not apply, a linear search is peformed."
  [db & mp]
  (cond
   (and (pos? (count mp)) (even? (count mp)))
   `(find* ~db (hash-map ~@mp))
   (= 1 (count mp))
   `(find* ~db ~(first mp))
   :else
   (-> "No fn" IllegalArgumentException. throw)))


                                        ; update


(defn insert* [db obj]
  (assert (map? obj))
  (let [cnt (-> db :cnt inc)]
    {:cnt     cnt
     :data    (assoc (:data db) cnt obj)
     :indices (doall (map (fn [x] (adds- x obj cnt)) (:indices db)))
     }))


(defn insert [db & objs]
  (reduce insert* db objs))

(comment


  (ffind
   (insert
    (create (INDEX [:a]) (INDEX [:a :b]))
    {:a 1 :b 2} {:a 2 :b 22} {:a 2 :b 33})
   :a 2)

  ;; 1000x speed difference when searching for indexed.
  (do
    (System/gc)
    (time (let [n  50000
                db (create (INDEX [:a]) (INDEX [:a :b]))
                db (reduce (fn [db x] (insert db {:a x :b x})) db (range n))]
            (time (doall (ffind db {:b n}))) ;; slow.
            (time (doall (ffind db {:a n})))
            (time (doall (ffind db {:a n :b n}))))))



  )

:OK
