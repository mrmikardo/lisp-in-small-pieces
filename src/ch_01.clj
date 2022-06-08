(ns ch-01
  "Chapter 1: a basic evaluator.")

(defn atom?
  "Returns `true` if an expression is atomic, `false` otherwise."
  [e]
  (not (list? e)))

(assert (= true (atom? 'foo)))
(assert (= true (atom? 1)))
(assert (= false (atom? '(1 2 3))))

(defn wrong
  "Custom exception function."
  [msg exp & extra-info]
  (throw (ex-info msg {:expression exp :extra-info extra-info})))

;; Section 1.3 - evaluating atoms.

(defn evaluate [e env]
  (if (atom? e)
    (cond
      ;; lookup symbols in the environment
      (symbol? e) (lookup e env)
      ;; unclear if we should permit Clojure vectors here
      ((some-fn number? string? char? boolean? vector?) e) e
      :else (wrong "Unable to evaluate atom" e))
    ;; use (first e) instead of car!
    (case (first e)
      ,,,)))

(assert (= 3 (evaluate 3 {})))
(assert (= "foo" (evaluate "foo" {})))
(assert (= \c (evaluate \c {})))
(assert (= true (evaluate true {})))
(assert (= [] (evaluate [] {})))

;; Section 1.4 extends the initial evaluator to handle special
;; forms and function invocation.

;; Declare some required helper functions (defined below).
(declare lookup eprogn update! invoke make-function evlis)

;; Note that this definition assumes that programs (expressions)
;; are syntactically well-formed; it doesn't do any checking.
(defn evaluate [e env]
  (if (atom? e)
    (cond
      ;; lookup symbols in the environment
      (symbol? e) (lookup e env)
      ;; unclear if we should permit Clojure vectors here
      ((some-fn number? string? char? boolean? vector?) e) e
      :else (wrong "Unable to evaluate atom" e))
    ;; use (first e) instead of car!
    (case (first e)
      ;; use `second` instead of `car` (which is used in book)
      quote (second e)
      if (if (evaluate (second e) env)
           (evaluate (nth e 2) env)
           (evaluate (nth e 3) env))
      begin (eprogn (rest e) env)
      set! (update! (second e) env (evaluate (nth e 2) env))
      lambda (make-function (second e) (nnext e) env)
      ;; not a special form, just an ordinary function => call it!
      (invoke (evaluate (first e) env) (evlis (rest e) env)))))

;; 1.4.1 - discussion of quoting

;; 1.4.2 - discussion of if forms / boolean values
;; We are relying in our `evaluate` implementation on the boolean
;; semantics of the host language (Clojure). A more precise
;; definition is below - where we make explicit boolean values
;; for the language under definition.
(comment
  (def false-value false)
  (if-not (false? ,,,)))

;; 1.4.3 - `eprogn` / sequences
;; `eprogn` is used for handling something similar to Clojure's
;; (do ,,,) blocks i.e. sequences of expressions that must be
;; evaluated in order.
(defn eprogn [exps env]
  (if (list? exps)
    (if (list? (last exps))
        '(begin (evaluate (first exps) env)
               (eprogn (last exps) env))
        (evaluate (first exps) env)))
  '())

;; Should return an empty list after all the sequences are evaluated.
(assert (= '() (evaluate '(begin (1 2) (3 4)) {})))

;; 1.4.6 - `evlis` (required for evaluating function arguments)
(defn evlis [exps env]
  (if (list? exps)
    (map #(evaluate % env) exps)
    '()))

(assert (= (nil nil "hello") (evlis '(a (println b) "hello") {})))

;; Section 1.5 - environments, `invoke`, `update!`, `extend` and `lookup`.
;; An environment associates variables with values (this is a "binding").
;; Historically an association-list was the data structure chosen for
;; this purpose; in Clojure we can get more performance and utility from
;; a hashmap.

(defn lookup
  "Returns the value associated with an id."
  [id env]
  (if-let [[_k v] (find env id)]
    v
    (wrong "No such binding" id)))

(assert (= "foo" (lookup 'foo {'foo "foo"})))

(defn update!
  "Updates the value associated with id in env."
  [id env value]
  (if (map? env)
    (assoc env id value)
    (wrong "Badly structured env" env {:id id
                                       :env env
                                       :value value})))

;; Initial environment - simply an empty hashmap.
(def env-init {})

;; When a function is applied
