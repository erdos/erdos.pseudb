(ns ^{:author "Janos Erdos"}
  erdos.pseudb
  "Small clojure schemaless document
   oriented data structure."
  (:require clojure.set))

(defn- submap?
  [sm large]
  (and
   (<= (count sm) (count large))
   (every? (fn [[k v]] (= (large k) v)) sm)))

(defn- assocf
  ([m k f]
     (assoc m (f (get m k))))
  ([m k f d]
     (if (contains? m k)
       (assoc m k(f (get m k)))
       (assoc m k d))))

(def IndexStrategy nil)

(defprotocol IndexStrategy
  ;; @returns (possibly empty) seq of result indices or
  ;; nil when index not compatible with where-map.
  (find- [_ where-map])

  ;; @returns modified index obj
  ;;   or this when not compatible
  ;;   or nil when collision.
  (adds- [_ obj-to-insert index-to-insert])

  ;; removes all i-s where old-map matches.
  ;; @returns modified index when removed.
  ;;          this when not removed or not compatible.
  (remv- [_ m is]))


(defn +MultiIndex
  "Strategy for not unique, nil allowed index."
  [& ks]
  ;(assert (coll? ks))
  (assert (every? keyword? ks))
  (letfn [(-vls [obj] (vec (map obj ks)))
          (-neue [mp]
            (reify IndexStrategy
              (find- [_ m]
                (when (every? m ks)
                  (get mp (-vls m) [])))
              (adds- [_ m i]
                (when (every? m ks)
                  (-neue (assocf mp (-vls m) #(conj % i) #{i}))))
              (remv- [this old i]
                (when (every? old ks)
                  (let [v (-vls old)]
                    (-neue (assoc mp v (disj (get mp v) i))))))))]
    (-neue {})))


(defn +UniqueIndex
  "Strategy for not unique index. throws exception on overwrite."
  [& ks]
;  (assert (coll? ks))
  (assert (every? keyword? ks))
  (letfn [(-vls [obj] (vec (map obj ks)))
          (-neue [mp]
            (reify IndexStrategy
              (find- [_ m]
                (if (every? m ks)
                  (if-let [i (get mp (-vls m))]
                    [i] [])))
              (adds- [this m i]
                (if (every? m ks)
                  (let [v (-vls m)]
                    (if-not (contains? mp v)
                      (-neue (assoc mp v i))))
                  this))
              (remv- [this m i]
                (or (if (every? m ks)
                      (let [v (-vls m)]
                        (if (contains? mp v)
                          (-neue (dissoc mp v i)))))
                    this))))]
    (-neue {})))


                                        ; create

(defn- create-index
  [[k v]]
  (({'INDEX +MultiIndex
     'UNIQUE +UniqueIndex} k) v))

(defn create* [indices]
  {:data {}, :cnt 0, :indices (map create-index indices)})


(defmacro create [& indices]
  `(create* '~indices))

                                        ; read

(defn find*
  [db w]
  (let [ks (map #(find- % w) (:indices db))]
    (if-not (every? nil? ks)
      (filter
       (partial submap? w)
       (map (:data db)
            (distinct (apply concat ks))))
      ;; ez nem jo!!
      (filter (partial submap? w)
              (-> db :data vals)))))


(defmacro ffind
  "Find all occurences in db.
   When indices do not apply, a
   linear search is peformed.
   Usage: (ffind :a 1) or (ffind {:a 1})"
  [db & mp]
  (cond
   (and (pos? (count mp)) (even? (count mp)))
   `(find* ~db (hash-map ~@mp))
   (= 1 (count mp))
   `(find* ~db ~(first mp))
   :else
   (-> "No fn" IllegalArgumentException. throw)))


                                        ; insert


(defn- insert* [db obj]
  (assert (map? obj))
  (let [cnt (-> db :cnt inc)
        ids (map (fn [x] (adds- x obj cnt))
                 (:indices db))]
    (when (not-any? nil? ids)
      {:cnt     cnt
       :data    (assoc (:data db) cnt obj)
       :indices ids})))


(defn insert
  "Returns nil when could not insert."
  [db & objs]
  (if (seq objs)
    (if-let [db (insert* db (first objs))]
      (recur db (rest objs)))
    db))

                                        ; remove

(defn remove* [db obj]
  (let [ids (map #(find- % obj) (:indices db))
        ids (set (apply concat ids))]
    (if (empty? ids)
      (assoc db
        :data (into
               {}
               (clojure.core/remove
                (comp (partial submap? obj) val)
                (:data db))))
      (assoc db
        :data    (apply dissoc (:data db) ids)
        :indices (map #(or (remv- % obj ids) %)
                      (:indices db))))))

(defn rremove [db & objs]
  (reduce remove* db objs))

                                        ; update

(defn updatef
  "Calls f on all items where wmap matches."
  [db wmap f]
  (let [old (ffind db wmap)]
    (apply insert
           (apply rremove db old)
           (map f old))))

(defn ffind-collision
  "Find colliding objects in db."
  [db obj]
  (map (:data db)
       (distinct
        (mapcat
         #(find- % obj)
         (remove  #(adds- % obj -1)
                  (:indices db))))))

;;; insert-merge: problem: may collide with multi vals.

(defn insert-merge
  "Inserts obj or replaces existing entity by
   the value of calling f.
   When f returns nil, returns nil"
  [db obj f]
  (let [cf    #(adds- % obj -1)
        clidx (first (remove cf (:indices db)))]
    (if-not clidx
      (insert db obj)
      (let [fnd-idx (first (find- clidx obj))
            fnd-obj (get (:data db) fnd-idx)
            db0     (rremove db fnd-obj)
            res-obj (f obj fnd-obj)]
        (if res-obj
          (or
           (insert db0 res-obj)
           (recur db0 res-obj f)))))))

(defn to-seq
  [db] (vals (:data db)))

(defn size
  "Size of db obj"
  [db] (-> db :data count))

(comment

  (def a0    (insert
    (create (UNIQUE [:a]))
    {:a 1 :b 2} {:a 2 :b 22} {:a 3 :b 33}))


  (ffind a0 :a 2)
  (ffind a0 {:b 2})
  (rremove a0 {:a 1})
  (insert   (remove* a0 {:a 1}) {:a 5})

  (create)

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
