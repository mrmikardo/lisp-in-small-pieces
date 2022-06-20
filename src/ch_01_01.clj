
;; # Chapter 1, part I ⚗️
;;
;; The first chapter handles building a basic [Scheme-style](https://en.wikipedia.org/wiki/Scheme_(programming_language)) evaluator.
;; It starts with a discussion of evaluating _atomic expressions_ and
;; later moves on to a discussion of more complex composite expressions
;; involving _function abstractions_ and _closures_.
;;
;; By the end of this chapter we'll have a fully functional evaluator
;; which is able to carry out the full spectrum of computations. That
;; is, our evaluator will be [Turing complete](https://en.wikipedia.org/wiki/Turing_completeness).
;;
;; In the first section, we'll see the outlines of a barebones evaluator
;; which can handle basic _atomic_ expresssions and some more complicated
;; _special forms_. In part II, we'll extend the evaluator from this
;; section to additionally handle _function evaluation_.

(ns ch-01-1
  "Chapter 1: a basic evaluator (no functions)."
  (:require
   [ch-01-2 :as e]))

;; ## Preliminaries

;; We first define some helper functions, beginning with `atom?`, a boolean
;; predicate which tells us whether or not an expression is _atomic_.
;; An _atomic_ expression is one that represents a single atomic value,
;; either a symbol or one of the basic data types.

(defn atom?
  "Returns `true` if an expression is atomic, `false` otherwise."
  [e]
  (not (list? e)))

;; Symbols are atomic...

(atom? 'foo)

;; ...as are basic data types...

(atom? 1)

;; ...but lists are not.

(atom? '(1 2 3))

;; We also define a basic exception handler `wrong` - really just a function
;; we can call when something goes awry. `wrong` takes a message parameter
;; `msg`, the expression `exp` that couldn't be evaluated, and optionally
;; some extra debug information.

(defn wrong
  "Custom exception function."
  [msg exp & extra-info]
  (throw (ex-info msg {:expression exp :extra-info extra-info})))

;; ## Section 1.3
;;
;; A program can contain _free variables_ and _bound variables_. A variable
;; is said to be _free_ if it is not qualified by a binding form such as
;; `let` or `lambda`; it is _free_ because its value could be anything.
;;
;; In order to determine the values associated with such variables, we need
;; some structure which keeps track of name -> value associations. In LISP,
;; this structure is known as the **environment**.
;;
;; The signature for `evaluate` thus requires two parameters: the _expression_
;; or program to be evaluated, and the _environment_ in which that evluation
;; should take place;

(defn evaluate [exp env] ,,,)

;; The easiest types of expressions to evaluate are **atomic expressions**,
;; so we'll start with those.
;;
;; We first need to declare `lookup`, a helper function for looking up the
;; values of _symbols_ in the environment (we'll define this later!);

(declare lookup)

;; Like any compiler, `evaluate` begins its work with a syntactic analysis
;; of the expression to be evaluated. At the top level we distinguish between
;; the two main types of expression: _atomic_ and _non-atomic_.
;;
;; Atomic expressions can be symbols, in which case we look them up in the
;; environment, or else one of the basic data types, in which case we simply
;; return the expression, as there is no need for any further evaluation.

(defn evaluate [e env]
  (if (atom? e)
    (cond
      ;; lookup symbols in the environment
      (symbol? e) (e/lookup e env)
      ;; unclear if we should permit Clojure vectors here
      ((some-fn number? string? char? boolean? vector?) e) e
      :else (wrong "Unable to evaluate atom" e))
    ;; use (first e) instead of car!
    (case (first e)
      ,,,)))

;; We are now ready to test our evaluator on some basic atomic expressions;

(evaluate 3 {})
(evaluate "foo" {})
(evaluate \c {})
(evaluate true {})
(evaluate [] {})

;; For the time being we pass an empty map `{}` as the `env` environment
;; argument, as we're not currently doing any evaluation that would require
;; an environment.

;; ## Section 1.4
;;
;; We're now in a position to extends our evaluator from Section 1.3 to handle special
;; forms and function invocation.

;; First we declare some required helper functions (which are defined below).
(declare eprogn update! invoke make-function evlis lookup)

;; Note that our definition of `evaluate` assumes that the programs (expressions)
;; we ask it to evaluate are syntactically well-formed; it doesn't do any checking
;; before it tries to evaluate.
(defn evaluate [e env]
  (if (atom? e)
    (cond
      ;; lookup symbols in the environment
      (symbol? e) (e/lookup e env)
      ;; unclear if we should permit Clojure vectors here
      ((some-fn number? string? char? boolean? vector?) e) e
      :else (wrong "Unable to evaluate atom" e))
    ;; use (first e) instead of car!
    (case (first e)
      ;; CHANGE: handle special forms
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

;; The evaluator can now handle several _special forms_ - `quote`, `if`,
;; `begin`, `set!` and `lambda`. Special forms are those syntactic
;; elements which require special treatment by our evaluator. For instance,
;; `begin` allows us to group a block of expressions to be evaluated in sequence;
;; when the evaluator encounters an expression that starts with `begin`, it
;; knows to handle the expression by passing it to the special helper function
;; `eprogn`, which knows how to evaluate a sequence of expressions one after
;; another (see below for the definition of `eprogn`).
;;
;; Special forms are distinct from functions, as they cannot be defined in terms
;; of the other constructs which our language affords us (yet! We'll see macros in a later chapter 😎).
;; That's why we define them here, in the body of the evaluator.
;;
;; Lastly, it's worth noting that the special forms of a LISP are what gives it
;; its particular characteristics. Scheme has a relatively terse set of special
;; forms - which makes it great for teaching and learning - whereas Common LISP
;; has a significantly higher number. See [here](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node59.html)
;; if you'd like to learn more 🙂.
;;
;; We'll now take a brief look at the special forms that our evaluator defines.

;; ## 1.4.1 - quoting
;;
;; The first of these special forms is `quote`. `quote` simply returns the value of its argument.
;; It's somewhat analogous to the [identity function](https://en.wikipedia.org/wiki/Identity_(mathematics)) in mathematics.
;;
;; It may not be immediately apparent why `quote` is useful. Why would we need
;; a special form that simply returns the value of its argument?
;;
;; One of the properties of LISP is known as _homoiconitiy_. In essence,
;; _homoiconitiy_ means that the representation of a valid LISP program
;; is homologous to the language itself. In other words, the structures used
;; to represent the language internally are the same structures that the language
;; defines.
;;
;; In these circumstances, `quote` can be used to pass around expressions which
;; can validly be evaluated, e.g. `(+ 1 2)`, without having the evaluator
;; inadvertently evaluate them.
;;
;; Some examples can help to motivate this somewhat;
(evaluate '(quote (1 2 3)) {})

(evaluate '(quote (+ 1 2 3)) {})

(comment
  "This won't evaluate as our `evaluate` function doesn't know what `+` is"
  (evaluate '(+ 1 2 3) {}))

;; Paul Graham has written [an excellent essay](http://www.paulgraham.com/rootsoflisp.html) about LISP where he explains
;; the importance of quote in an eloquent fashion.
;;
;; ## 1.4.2 - if
;;
;; The handling of `if`-statements is relatively intuitive, and uses the
;; following algorithm;
;; 1. `evaluate` the first expression.
;; 2. If the first expression evaluate to `true`, return the evaluated value of the second expression in the sequence as the final result of the expression.
;; 3. Otherwise, return the evaluated value of the third expression in the sequence as the final result of the expression.
;;
;; Note that in our `evaluate` implementation we rely on the boolean
;; semantics of the host language (Clojure). We'll see later how we can
;; define some constants for our interpreter that will make the separation
;; between host language and language-under-definition more discreet 🙂.
;;
;; ## 1.4.3 - begin
;;
;; We've already discussed the `begin` sepcial form in section 1.4. Here we'll
;; provide a definition of `eprogn`, the helper function responsible for handling
;; expressions starting with `begin`;

(defn eprogn [exps env]
  ;; if there are no more expressions to evaluate, return the empty list
  (if (list? exps)
    ;; if the rest of the expressions are a list, then we need to recur
    (if (list? (last exps))
        ;; note the implicit recursive call to our evaluator with `begin`
        '(begin (evaluate (first exps) env)
               (eprogn (last exps) env))
        (evaluate (first exps) env)))
  '())

;; `eprogn` should return an empty list after all the sequences have been evaluated:
(= '() (evaluate '(begin (1 2) (3 4)) {}))

;; ## 1.4.4 - set!
;;
;; The special form `set!` is used to assign a value to a variable. We'll look
;; at `set!` in more detail later when we discuss environments.

;; ## 1.4.6 - `evlis` and functional application

;; If an expression is not atmoic, and does not have  a special form
;; in first position, then the assumption is that it must be a function.
;; We thus proceed to evaluate it.
;;
;; The final part of the `evaluate` function we've defined above is responsible
;; for _functional application_ -

;;
;; ```
;; (case (first exp)
;;     ,,,  ;; other special forms
;;     (invoke (evaluate (first e) env) (evlis (rest e) env)))))
;; ```

;; This can be read as _"call the function `invoke` - which we
;; haven't yet defined - on the evaluated first term - which ought to be
;; the name of a function - and the evaluated list of arguments"_.
;;
;; We define the `evlis` helper which is responsible for evaluating
;; the list of arguments;

(defn evlis [exps env]
  (if (list? exps)
    (map #(evaluate % env) exps)
    '()))

(evlis '(a b "hello") {'a "a" 'b "b"})

;; Note that we have to pass some arguments in the `env` parameter here,
;; so that `evaluate` is able to look up the symbols `'a` and `'b`. We've
;; cheated a little bit too by importing the `lookup` function ahead of time from
;; part II.
;;
;; As functions are a little more complicated, and involve some understanding
;; of the _environment_, we'll turn our attention to that prior to seeing a
;; definition of `invoke`.
