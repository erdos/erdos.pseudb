erdos.pseudb
============

small clojure document oriented schemaless data structure

## introduction


## installation

Place `pseudb.clj` in the project's `/src/erdos/` directory, and then require the package in your `(ns)`:
```clojure
(:require (erdos.pseudb :as ps))
```

## usage

### constructor

Create a pseudb object by calling the `create` functions. You can set up the indices in this constructor call.

**syntax:** `(create & clauses)` <br/>
**clause:** `(UNIQUE k1 k2 .. kn)` or `(INDEX k1 k2 .. kn)`
where clauses are optional and k1, k2, ..., kn are keywords.

**example:**
```clojure
(def db0
  (ps/create (UNIQUE :fname :lname)))
;; may not insert two items with the same :fname :lname values.

(ps/create (INDEX :a :b) (UNIQUE :a :b :c))
(ps/create) ; => always O(n) time find.
```

### ffind
`(ffind db m)`
Returns a list of all supermaps of m in db. Takes O(1) time when indices are set for some keys in m.

**example:**
```clojure
(ffind db {:a 1 :b 2})
```

### rremove

### insert or update
