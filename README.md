erdos.pseudb
============

small clojure document oriented schemaless data structure

*installation*

Place `pseudb.clj` in the project's `/src/erdos/` directory, and then require the package in your `(ns)`:
```clojure
(:require (erdos.pseudb :as ps))
```



*functions*

**constructor**

Create a storage object by calling the `create` functions. Setting up the index types takes place in this constructor call. You can change the indices later only by rebuilding the entire storage object.

_syntax_
```clojure
(create & args)
;; where args is a sequence of clauses
;; a clauses:
;; - (UNIQUE key1 key2 ... keyn) for unique key
;; - (INDEX k1 k2 ... kn) for general key
```
Any number of clauses is accepted.

_example_
```clojure
(def db0 (ps/create (UNIQUE :fname :lname)))
;; may not insert two items with the same :fname :lname values.
```




**find**

**remove**

**insert or update**
