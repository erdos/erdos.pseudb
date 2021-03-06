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
**clause:** `(UNIQUE k1 k2 .. kn)` or `(INDEX k1 k2 .. kn)` <br/>
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

Returns a list of all supermaps of m in db. Takes O(1) time when indices exist for keys in m.

**example:**
```clojure
(ffind db {:a 1 :b 2})
```

### rremove

`(rremove db m)`

Returns a new version of db with all supermaps of m removed. Takes O(1) time when indices exist for keys in m.

**example:**
```clojure
(rremove db0 {:a 1}) ; => removes {:a 1}, {:a 1, :b 2}, etc..
(rremove db0 {}) ; => removes all elements. slow.
```

### insert or update

`(insert-merge db obj f)`

Inserts `obj` to `db` collisions resolved by fn `f`.
Returns a new version of `db` storage object.
Tries to insert `obj` and on collision, removes colliding item from db, calls `f` with the old item and new item, and repeatedly tries to insert result. Always suceeds.

### insert or replace

`(insert-replace db obj)`

Inserts `obj` to `db` and removes all colliding old items.
