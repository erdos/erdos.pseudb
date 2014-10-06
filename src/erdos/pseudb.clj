(ns ^{:author "Janos Erdos"}
  erdos.pseudb
  "Small clojure schemaless document
   oriented data structure."
  (:require clojure.set)
  (:gen-class))

(defn- submap?
  [sm large]
  (and
   (<= (count sm) (count large))
   (every? (fn [[k v]] (= (large k) v)) sm)))

(defn- assocf
  ([m k f]
     (assoc m k (f (get m k))))
  ([m k f d]
     (if (contains? m k)
       (assoc m k (f (get m k)))
       (assoc m k d))))

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
          (-appliable [obj] (every? obj ks))
          (-neue [mp]
            (reify IndexStrategy
              (find- [_ m]
                (when (-appliable m)
                  (get mp (-vls m) [])))
              (adds- [t m i]
                (if (-appliable m)
                  (-neue (assocf mp (-vls m)
                                 #(conj % i) #{i}))
                  t))
              (remv- [this old i]
                (when (-appliable old)
                  (let [v (-vls old)]
                    (-neue (assoc mp v (disj (get mp v) i))))))))]
    (-neue {})))


(defn +UniqueIndex
  "Strategy for unique index."
  [& ks]
;  (assert (coll? ks))
  (assert (every? keyword? ks))
  (letfn [(-vls [obj] (mapv obj ks))
          (-neue [mp]
            (reify IndexStrategy
              (find- [_ m]
                (when (every? m ks)
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
  [[k & v]]
  (apply ({'INDEX +MultiIndex
     'UNIQUE +UniqueIndex} k) v))


;; to define once only.
(deftype PStorage
    [data cnt indices]
  clojure.lang.Counted
  (count [_] (count data))
  clojure.lang.Seqable
  (seq [_] (-> data vals seq))
  java.lang.Object
  (toString [t] (str "<storage of " (count t) " items>")))

;; (println "hajdiho")
;; (println (bean erdos.pseudb.PStorage))


(defmacro create [& indices]
  `(->PStorage {} 0 (map ~create-index '~indices)))

                                        ; read

;; TODO: not submap check, but intersections first.
(defn find*
  [^PStorage db w]
  (if-let [s (some #(find- % w) (.indices db))]
    (filter (partial submap? w) (map (.data db) s))
    (filter (partial submap? w)
            (-> db .data vals))))

(defmacro ffind
  "Find all occurences in db.
   When indices do not apply, a
   linear search is peformed.
   Usage: (ffind :a 1) or (ffind {:a 1})"
  [db & mp]
  (when (keyword? db)
    (-> "You forgot to add db obj as first arg."
        IllegalArgumentException. throw))
  (cond
   (and (pos? (count mp)) (even? (count mp)))
   `(find* ~db (hash-map ~@mp))
   (= 1 (count mp))
   `(find* ~db ~(first mp))
   :else
   (-> "No fn" IllegalArgumentException. throw)))


                                        ; insert


(defn- insert-one [db obj]
  (assert (map? obj))
  (assert (instance? PStorage db)
          (str "No PStorage object, got: " (type db)))
  (let [cnt (inc (.cnt db))
        ids (map (fn [x] (adds- x obj cnt))
                 (.indices db))]
    (when (not-any? nil? ids)
      (->PStorage (assoc (.data db) cnt obj) cnt ids))))


(defn insert
  "Returns nil when could not insert."
  [db & objs]
  (if (seq objs)
    (when-let [db (insert-one db (first objs))]
      (recur db (rest objs)))
    db))

                                        ; remove

(defn- remove-one
  "Returns db with obj removed."
  [db obj]
  (let [ids0 (map #(find- % obj) (.indices db))]
    (if
     (every? nil? ids0) ;; hard filtering is needed.
     (->PStorage
      (into {}
            (clojure.core/remove
             (comp (partial submap? obj) val)
             (.data db))) (.cnt db) (.indices db))

     (if (every? empty? ids0)
       db
       (let [ids (set (apply concat ids0))
             ids (filter #(submap? obj (get (.data db) %))
                         ids)]
         (->PStorage
          (apply dissoc (.data db) ids)
          (.cnt db)
          (map #(or (remv- % obj ids) %)
               (.indices db))))))))

(defn rremove [db & objs]
  (reduce remove-one db objs))

                                        ; update

(defn updatef
  "Calls f on all items where wmap matches. returns same object when no item is found. returns nil when f returns nil."
  [db wmap f]
  (assert (instance? PStorage db)
          (str "no PStorate object, got instead: " (type db)))
  (assert (map? wmap))
  (assert (ifn? f))
  (loop [db  db
         old (seq (ffind db wmap))]
    (if-let [[x & xs] old]
      (when-let [fx (f x)]
        (-> db
            (remove-one x)
            (insert-one fx)
            (recur xs)))
      db)))

(defn ffind-collision
  "Find colliding objects in db."
  [db obj]
;;  (assert (instance? PStorage db))
  (map (.data db)
       (distinct
        (mapcat
         #(find- % obj)
         (remove  #(adds- % obj -1)
                  (.indices db))))))

(defn insert-merge
  "Inserts obj or replaces existing entity by
   the value of calling (f new existing).
   When f returns nil, returns nil"
  [db obj f]
;;  (assert (instance? PStorage db))
  (let [colliding
        (some (fn [x] (if-not (adds- x obj -1) x))
              (.indices db))]
    (if (nil? colliding)
      (insert-one db obj)
      (let [fnd-obj (get (.data db)
                         (first (find- colliding obj)))
            db0     (remove-one db fnd-obj)
            res-obj (f obj fnd-obj)]
        (when res-obj
          (or
           (insert db0 res-obj)
           (recur db0 res-obj f)))))))

(defn insert-replace [db obj]
  (insert-merge db obj (fn [n o] n)))


:OK
