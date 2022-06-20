
;; # Chapter 1, part II 🧪

;; We continue our exploration of Lisp in small pieces by finishing what
;; we started in part I - our evaluator.
;;
;; In this section, we begin with a look at _environments_, which are a
;; key and somewhat subtle part of Lisp evaluation. We then turn our
;; attention to how _functions_ or _closures_ can be evaluated in a
;; given context or _environment_. Once we've got a way of handling
;; functions, we'll have everything we need to create a full Scheme-style
;; evaluator.

(ns ch-01-2
  "Chapter 1 (cont): a basic evaluator, inc. functions."
  (:require [ch-01-1 :as e]))

;; ## Section 1.5 - the environment

;; A Lisp _environment_ associates variables with values - this is what's known
;; as a _binding_. An environment is a sort of abstract data type which
;; can be described via the following interface;
;;
;; - It should be possible to insert new items.
;; - It should be possible to lookup existing items by id/key.
;; - It should be possible to overwrite existing values with new values.
;;
;; Note that the first and last points are effectively synonymous if we
;; implement our environment in such a way that inserting a value associated
;; with a preexisting key simply overwrites the value that the key already
;; points to.
;;
;; Historically an [_association-list_](https://en.wikipedia.org/wiki/Association_list) was the data structure chosen for
;; this purpose; in Clojure  however we can get greater performance and utility with
;; a [_hashmap_](https://clojure.org/guides/learn/hashed_colls).
;;
;; The initial environment is thus simply an empty hashmap;
(def env-init {})

;; We'll tackle the first part of the interface defined above
;; with `extend` - a function that allows us to insert new items into
;; the environment;
(defn extend
  "Enriches an environment by associating variables -> values.
  Note that this shadows `clojure.core/extend`."
  [env variables values]
  (if (= (count variables) (count values))
    (let [env-extension (zipmap variables values)]
      (merge env env-extension))
    (e/wrong "Mismatch in counts of values and variables" env {:env env
                                                             :variables variables
                                                             :values values})))

;; We're now able to put some bindings into our env;
(extend env-init ['a 'b 'c] [1 2 3])

;; Note that this hits the final point too - values in the environment are automatically
;; overwritten with those that we _extend into_ it:
(extend {'a 1 'b 2 'c 3} ['a 'b 'c 'd] [4 5 6 7])

;; For completeness however we'll define the function `update!` which
;; _updates_ the value associated with an id in the environment.
(defn update!
  "Updates the value associated with id in env."
  [id env value]
  (if (map? env)
    (assoc env id value)
    (e/wrong "Badly structured env" env {:id id
                                       :env env
                                       :value value})))

(update! 'a {'a 1} 2)

;; Finally, to complete the interface for our environment, we'll define
;; a `lookup` function which takes the environment and an id (a _key_) as
;; parameters and returns the value associated with that id;
(defn lookup
  "Returns the value associated with an id."
  [id env]
  (if-let [[_k v] (find env id)]
    v
    (e/wrong "No such binding" id)))

(lookup 'foo {'foo "foo"})
(lookup 'my+ {'my+ +})

;; Note however that because of the immutability of datastructures in Clojure,
;; we don't yet get a _persistent_ environment. That is, the following won't work;
(extend env-init ['a] [1])
(comment
  (lookup 'a env-init))

;; ...even though we define a suitable binding for `'a`.

;; It's also worth noting, whilst we're here, that `lookup` marks an implicit
;; conversion between _symbols_ and _variables_: an environment associates
;; the latter with semantically meaningful values; and variables just so happen
;; to be represented syntactically by symbols.
;;
;; But we could choose something
;; else to represent variables. Accordingly, a more precise definition of the
;; `lookup` function given above might be `,,, (lookup (symbol->variable exp) env) ,,,`, where
;; we call the `symbol->variable` helper to make this conversion explicit.
;;
;; As it stands, we leave `symbol->variable` as an exercise for the reader
;; 😉.
