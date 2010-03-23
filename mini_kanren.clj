;; Mini-Kanren implemented in Clojure

;; by Jim Duey
;; last updated March 23, 2010

;; Copyright (c) Jim Duey, 2009, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns mini-kanren
	(:refer-clojure)
	(:use [clojure.contrib monads]
          [clojure.contrib.pprint :only [pprint]]
    ))

(do
(comment
  This file is the result of implementing the logic programming system described
  in "The Reasoned Schemer", called mini-kanren, in Clojure.  I was struggling
  to understand the material in the book without a concrete system to work with,
  so I decided to implement the system in the book to aid my understanding.

  "The Reasoned Schemere" is the third book in the "Little Schemer" series of books
  that explain the Scheme programming language and various aspects of programming
  in Scheme.  "The Reasoned Schemer" extends Scheme to include logic programming like
  Prolog and the pattern maching in Erlang.  The first six chapters introduce the 
  syntax and semantics of this extension, which is what this file covers.

  I chose to pretty much lift the logic function names directly from the book to make
  learning the material easier.  Though there are some differences which should be 
  easily discerned by comparing the code to the book.

  There are two areas that required some work to implement this system in Clojure.

  First, in the book's implementation, logic variables are represented as vectors.
  I chose to represent logic variables as keywords, for reasons explained later.  So,
  the following definition creates a logic variable.  Each time an lvar is created,
  it is assigned a unique name using the gensym facility provided by Clojure.)

; a couple of special symbols
  
(def _ '_)
(def | '|)

; working with logic variables

(defn lvar
 "Creating a unique logic variable that has no value assigned"
  []
  (str (gensym (str "lvar__"))))

(defn lvar?
  "Determine if a value is an logic variable or not."
  [x]
  (and (string? x)
       (.startsWith x "lvar")))

(comment
  The second area of work stems from the fact that Clojure does not use cons cells
  to hold elements of lists.  The book made heavy use of this feature of Scheme and
  so a special version of cons had to be defined.  Passing an item and a list caused
  the item to be added at the head of the list.  Both of those cases were easy to
  account for.  The third case is when the second parameter to cons is not a list,
  causing cons to create a dotted pair where the cdr of the cons cell is a value, not
  a pointer to the rest of the list.  This aspect of cons is used heavily in the book
  to represent lists where the cdr may be undefined.  Since this was a required feature,
  I chose to implement an incomplete list by adding a :tail item to the meta data of a
  literal list, if the second parameter to cons is an lvar.  This worked very well.
  
  This also meant that a special version of 'next' had to be defined to handle incomplete lists.)

(defn- is-seq? [l]
  (or (seq? l)
      (set? l)
      (vector? l)))

(defn lcons [a b]
  "cons a value onto a logic seq"
  (cond
    (nil? b) (list a)
    (is-seq? b) (with-meta (cons a b) (meta b))
    (lvar? b) (with-meta (list a) {:tail b})
    :else (cons a b)))

(defn- lseq? [l]
"test if a value is logic sequence or not
an empty sequence is not a valid logic sequence"
  (and (is-seq? l)
       (not (empty? l))))

(defn- incomplete [l t]
"make a logic sequence that is incomplete, the rest of the
sequence is in the tail"
	  (with-meta l {:tail t}))
;  (concat l [| t]))

(defn- lnext [l]
  "get everything but the first element of a logic sequence"
  (let [nl (rest l)]
    (cond
      (= (first nl) |) (second nl)
      (and (empty? nl) (:tail (meta l))) (:tail (meta l))
      (empty? nl) []
      :else (with-meta nl (meta l)))))

(comment
  Values are assigned to logic variables through a mechanism called substitution.  In
  the book's implementation, this was accomplished through the use of an associative
  list, which is basically a list of cons cells where the car of the cell is the key
  and the cdr of the cell is the value.  Clojure has the hash-map data type which is
  a natural replacement for an associative list.  Using a hash-map to hold a substitution,
  the use of keywords for logic variables means that retrieval of a variable's value is
  simply a get operation on the map with that variable.)

; operations on substitutions

; this is what a substitution looks like, 'e is a value, :x and :y are
; logic variables:
;
; the keys in a substitution may only be keywords
; {:y 'e :x :y}
;
; the values in a substitution may be:
; 	a symbol
; 	a keyword (may or may not be a key in the substitution)
; 	a literal list

(defn lget [s v]
  "Retrieve the value of a logic variable from a substitution"
  (cond
    (= _ v) (lvar)
    (contains? s v) (recur s (get s v))
    (lvar? v) v
    (and (is-seq? v) (:tail (meta v))) (let [tail (lget s (:tail (meta v)))]
                                   (if (is-seq? tail)
                                     (with-meta (seq (concat v tail)) (meta tail))
                                     (with-meta v {:tail tail})))
    :else v))

(defn deep-lget [s v]
  "Walk a logic variable through a substitution.  If the value
  of the variable is a list, get each item in the list.
  Repeat recursively until all atoms are either variables
  with no values or values."
  (let [v (lget s v)]
    (cond
      (lvar? v) v
      (lseq? v) (if (:tail (meta v))
                  (concat (map (partial deep-lget s) v) [| (deep-lget s (:tail (meta v)))])
                  (map (partial deep-lget s) v))
      :else v)))

; unit tests for implementation

(comment
(def x (lvar x))
(def y (lvar y))
(def z (lvar z))
(def l (lvar l))

(def s {y 'e, x y})

(assert (= 'e (lget s x)))
(assert (= z (lget s z)))

(def sl {l (lcons 'a x) x (lcons y z) y 'b z '(1 2 3)})

; end unit tests
)

(defn deep-reify [s v]
  "Associate an indeterminate value with 'v' in 's' if
  it does not already have a value assigned to it.
  If the value of 'v' is a list, recursively associate
  values with each logic variable in the list."
  (let [v (lget s v)]
    (cond
      (lvar? v) (assoc s v (symbol (str "_." (count s))))
      (lseq? v) (with-meta (reduce deep-reify s v)
                           {:tail (:tail (meta v))})
      :else s)))

(defn reify [v]
 "Assign an indeterminate value to 'v'"
	  (deep-lget (deep-reify {} v) v))

(defn- circular?  [s x v]
  "Tests if associating x with v in s will generate a
  circular association"
  (let [v (lget s v)]
    (cond
      (lvar? v) (identical? v x)
      (lseq? v) (some (partial circular? s x) v)
      :else nil)))

(defn- _-to-lvar
  "Replace all the _'s in a value with lvars."
  [v]
  (cond
    (= _ v) (lvar)
    (is-seq? v) (if (contains? (meta v) :tail)
                  (let [new-v (with-meta (map _-to-lvar v) (meta v))]
                    new-v)
                  (let [[sq tail] (split-with (partial not= |) v)
                        new-v (with-meta (map _-to-lvar sq) {:tail (_-to-lvar (second tail))})]
                    new-v))
    :else v))

(defn- safe-assoc [x v s]
  "Associate x with v in s if it will not create
  a circular association"
  (cond
    (circular? s x v) nil
    :else (assoc s x (_-to-lvar v))))

(defn unify  [v w s]
  "Add an association to a substitution if it is not already there,
  if it does not violate any associations already in the substitution
  and if it won't create a circular association"
  (let [v (lget s v)
        w (lget s w)]
    (cond
      (identical? v w) s
      (lvar? v) (safe-assoc v w s)
      (lvar? w) (safe-assoc w v s)
      (and (lseq? v) (lseq? w)) (when-let [new-s (unify (first v) (first w) s)]
                                  (recur (lnext v) (lnext w) new-s))
      (= v w) s
      :else nil)))

; To unify means to add associations to a substitution according to the following rules:
(comment
  (let [x (lvar)
        y (lvar)
        w (lvar)
        q (lvar)]
(assert (= {x 'a} 				(unify x 'a {})))
(assert (= {x y} 				(unify x y {})))
(assert (= {x 'b y 'b} 		    (unify x y {y 'b})))
(assert (= {x 'c y 'c} 		    (unify x y {x 'c})))
(assert (= {x 'a y 'a} 		    (unify x y {x 'a y 'a})))
(assert (= {x 'a y x} 		    (unify x y {x 'a y x})))
(assert (= {q 1 w 9} 			(unify x x {q 1 w 9})))
(assert (= {q 1 w 9} 			(unify '(1 2 3) '(1 2 3) {q 1 w 9})))
(assert (= {x '(a b)} 			(unify x '(a b) {})))
(assert (= {x (list 'a y)} 	    (unify x (list 'a y) {})))
(assert (= {x 'a y 'b} 		    (unify (list x 'b) (list 'a y) {})))
(assert (= {x 'a y '(b c)}	    (unify (lcons x y) (list 'a 'b 'c) {})))
(assert (= {x 'b}				(unify (list 'a x 'c) (list 'a 'b 'c) {})))
(assert (= nil					(unify (lcons x y) () {q 1 w 9})))
))

; The monad foundation for the implementation.  It's basically a lazier 
; variant of the sequence-m monad.

(defmonad logic-m
   [m-result (fn [v]
				 (list v))
    m-bind   (fn m-bind-sequence [mv f]
				 (lazy-seq
				   (when-let [vs (seq mv)]
							 (lazy-cat (f (first vs))
									 (m-bind-sequence (rest vs) f)))))
    m-zero   (list)
    m-plus   (fn [mvs]
				 (lazy-seq
				   (apply concat mvs)))
    ])

; A similar monad except that it's m-plus function interleaves the values
; from each of the monadic values.

(defmonad logic-interleave-m
   [m-result (fn [v]
				 (list v))
    m-bind   (fn m-bind-sequence [mv f]
				 (lazy-seq
				   (when-let [vs (seq mv)]
							 (concat (f (first vs))
									 (m-bind-sequence (rest vs) f)))))
    m-zero   (list)
    m-plus   (fn m-plus-logic [mvs]
				 (let [mvs (drop-while empty? mvs)]
				   (when-not (empty? mvs)
							 (lazy-seq
							   (cons 
								 (ffirst mvs)
								 (m-plus-logic (concat (rest mvs)
													   (list (rest (first mvs))))))))))
    ])

; Clojure implementation of 'Reasoned Schemer' goals
; a goal is a function that accepts a substitution and 
; returns a list of substitutions.  They are monadic functions
; under the logic-m monad.

; The two foundational goals.  A goal takes a stream of
; substitutions and returns a stream of substitutions.
(defn fail [s]
	  (list))

(defn succeed [s]
	  (list s))

; generates a goal that associates a logic variable
; with a value
(defn & [v w]
  (fn [s]
    (if-let [result (unify v w s)]
      (list result)
      (list))))

; &-expr generates a goal that associates a logic variable
; with the value produced by an expression.  value-of is
; a function that can be used in expressions to get the
; current value of a logic variable.
(def curr-subst)
(defn value-of [x]
	  (deep-lget curr-subst x))

(defmacro &-expr [v expr]
  "Associates a free variable with the result of evaluating 'expr'."
  `(fn [s#]
	   (binding [curr-subst s#]
				(when-let [result# (unify ~v ~expr curr-subst)]
						  (list result#)))))

; some utility functions to build the mini-kanren operators

(defn- remove-else [clause-list]
	   (map #(if (= 'else (first %))
					   (next %)
					   %)
					clause-list))

(with-monad logic-m
			(defn do-question [& clause]
				  (m-chain clause))

			(defn test-question [& clause]
				  (let [answer (m-chain (rest clause))]
					(fn [s]
						(let [tested ((first clause) s)]
						  (when-not (= m-zero tested)
									(m-bind tested answer)))))))

(defn- build-clauses [c-list]
	   (map #(cons 'do-question %)
			(remove-else c-list)))

(defn- build-questions [c-list]
	   (map #(cons 'test-question %)
			(remove-else c-list)))

; the mini-kanren operators

(defmacro cond-e [& c-list]
  (let [clauses (build-clauses c-list)]
	`(with-monad logic-m
				 (fn [s#]
					 (~'m-plus (map (fn [c#] (c# s#))
									(lazy-seq (list ~@clauses))))))))

(defmacro cond-i [& c-list]
  (let [clauses (build-clauses c-list)]
	`(with-monad logic-interleave-m
				 (fn [s#]
					 (~'m-plus (map (fn [c#] (c# s#))
									(lazy-seq (list ~@clauses))))))))

(defmacro cond-a [& c-list]
  (let [questions (build-questions c-list)]
	`(with-monad logic-m
				 (fn [s#]
					 (first
					   (drop-while nil? (map (fn [c#] (c# s#))
											   (lazy-seq (list ~@questions)))))))))

(defmacro cond-u [& c-list]
  (let [questions (build-questions c-list)]
	`(with-monad logic-m
				 (fn [s#]
					 (take 1 
					   (~'m-plus (map (fn [c#] (c# s#))
									  (lazy-seq (list ~@questions)))))))))

; exist is used to create new logic variables that can then be bound to values
(defmacro exist [v-list & goals]
  `(with-monad logic-m
			   (let [~@(mapcat (fn [v]
								   `(~v (lvar)))
							   (seq v-list))] 
				 (m-chain (list ~@goals)))))

(defn all [& args]
  (with-monad logic-m
              (m-chain args)))

; run computes the results of a mini-kanren expression
(defmacro run [x & goals]
  `(with-monad logic-m
			   (let [~x (lvar)]
				 (map (fn [s#]
						  (reify (deep-lget s# ~x)))
					  (filter (complement nil?)
							  ((m-chain (list ~@goals)) {}))))))
)

; various logic programming functions from "Reasoned Schemer"

(do
(defn cons-o
  "Generates a goal that associates '(cons f r) with 'l'."
  [f r l]
  (cond 
    (or (nil? r) (= r ())) (& (list f) l)
    (lvar? r) (& (lcons f r) l)
    (is-seq? r) (exist (new-r)
                       (& new-r r)
                       (& (lcons f new-r) l))
    :else (& (lcons f (list r)) l)))

(defn first-o
 "Generates a goal that associates 'f' with
the first element of 'l'."
	  [l f]
	  (exist [r]
			 (cons-o f r l)))

(defn rest-o
	  "Generates a goal that associates 'r' with
	  the rest of 'l'."
	  [l r]
	  (fn [s-list]
		  ((exist (f)
				  (cons-o f r l))
		   s-list)))

(defn null-o [x]
	  (fn [s-list]
		  ((& [] x) s-list)))

(defn eq-o [x y]
	  (& x y))

(defn pair-o [l]
  (if (= l [])
    fail
    (fn [s-list]
      ((exist (f r)
              (cons-o f r l)) s-list))))

(defn list-o [l]
	  (cond-e
		((null-o l) succeed)
		((pair-o l) (exist (f r)
						   (rest-o l r)
						   (list-o r)))
		(else fail)))

(comment
(defn map-o [m]
	  (fn [s]
		  (when (map? (deep-lget s m))
			(list s))))

(defn vector-o [m]
	  (fn [s]
		  (when (vector? (deep-lget s m))
			(list s))))

(defn symbol-o [symb]
	  (fn [s]
		  (when-let [result (unify true (symbol? (deep-lget s symb)) s)]
			  (list result))))
  )

(defn lol-o [l]
	  (cond-e
		((null-o l) succeed)
		((exist (a)
				(first-o l a)
				(list-o a))
		 (exist (d)
				(rest-o l d)
				(lol-o d)))
		(else fail)))

(defn twins-o [s]
	  (exist (x)
			 (& (list x x) s)))

(defn lot-o [l]
	  (cond-e
		((null-o l) succeed)
		((exist (f)
				(first-o l f)
				(twins-o f))
		 (exist (r)
				(rest-o l r)
				(lot-o r)))
		(else fail)))

(defn listof-o [pred-o l]
	  (cond-e
		((null-o l) succeed)
		((exist (f)
				(first-o l f)
				(pred-o f))
		 (exist (r)
				(rest-o l r)
				(listof-o pred-o r)))
		(else fail)))

(defn lot-o [l]
	  (listof-o twins-o l))

(defn lol-o [l]
	  (listof-o list-o l))	

(defn member-o [x l]
  (exist [r]
         (cond-e
           ((& [x | _] l))
           ((& [_ | r] l) (member-o x r)))))

(defn pmember-o [x l]
  (cond-e
    ((first-o l x) (rest-o l ()))
    ((first-o l x) (exist (f r)
                          (rest-o l (lcons f r))))
    (else (exist (r)
                 (rest-o l r)
                 (pmember-o x r)))))

(defn mem-o [x l out]
	  (cond-e
		((first-o l x) (& l out))
		(else (exist (r)
					 (rest-o l r)
					 (mem-o x r out)))))

(defn rember-o [x l out]
	  (cond-e
		((null-o l) (& () out))
		((first-o l x) (rest-o l out))
		(else (exist (f r res)
					 (cons-o f r l)
					 (rember-o x r res)
					 (cons-o f res out)))))

(defn surprise-o [s]
	  (rember-o s '(a b c) '(a b c)))

(defn append-o [l s out]
	  (cond-e
		((null-o l) (& s out))
		(else
		  (exist (f r res)
				 (cons-o f r l)
				 (cons-o f res out)
				 (append-o r s res)))))

(defn unwrap-o [x out]
	  (cond-e
		(succeed (& x out))
		(else (pair-o x) (exist (f)
								(first-o x f)
								(unwrap-o f out)))))

(defn flattenrev-o [s out]
	  (cond-e
		(succeed (cons-o s () out))
		((null-o s) (& () out))
		(else (exist (f r res-f res-r)
					 (cons-o f r s)
					 (flattenrev-o f res-f)
					 (flattenrev-o r res-r)
					 (append-o res-f res-r out)))))

(defn any-o [g]
	  (cond-e
		(g succeed)
		(else (any-o g))))

(def never-o
	 (any-o fail))

(def always-o
	 (any-o succeed))

(defn once-o [g]
	  (cond-u
		(g succeed)
		(else fail)))

(defn sal-o [g]
	  (cond-e
		(succeed succeed)
		(else g)))

(defn not-pasta-o [x]
	  (cond-a
		((& 'pasta x) fail)
		(else succeed)))
)

; questions from the book as unit tests

(do
(defmacro frame [frame-num value expr]
  `(do
;	 (println "\nframe" ~frame-num)
	 (let [result# ~expr]
;	   (println "result:" result#)
;	   (flush)
	   (assert (= ~value result#)))))

(frame "1.10" []
	   (run q
			fail))

(frame 1.11 '(true)
	   (run q
			(& true q)))

(frame 1.12 []
	   (run q
			fail
			(& true q)))

(frame 1.13 '(true)
	   (run q
			succeed
			(& true q)))

(frame 1.15 '(corn)
	   (run q
			succeed
			(& 'corn q)))


(frame 1.23 '(true)
	   (run q
			(exist [x]
				   (& true x)
				   (& true q))))

(frame 1.26 '(true)
	   (run q
			(exist (x)
				   (& x true)
				   (& true q))))

(frame 1.27 '(true)
	   (run q
			(exist (x)
				   (& x true)
				   (& q true))))

(frame 1.29 '(_.0)
	   (run q
			(let [x false]
			  (exist (x)
					 (& x true)))))

(frame "1.30" '((_.0 _.1))
	   (run r
			(exist (x y)
				   (& (lcons x (lcons y ())) r))))

(frame "1.30a" '((_.0 _.1))
	   (run r
			(exist (x y)
				   (& [x y] r))))

(frame 1.32 '((_.0 _.1 _.0))
	   (run r
			(exist (x)
				   (let [y x]
					 (exist (x)
							(& (lcons y (lcons x (lcons y ()))) r))))))

(frame "1.32a" '((_.0 _.1 _.0))
	   (run r
			(exist (x)
				   (let [y x]
					 (exist (x)
							(& [y x y] r))))))

(frame 1.34 []
	   (run q
			(& true q)
			(& false q)))

(frame 1.35 '(false)
	   (run q
			(& false q)
			(& false q)))

(frame 1.36 '(true)
	   (run q
			(let [x q]
			  (& true x))))

(frame 1.37 '(_.0)
		   (run r
				(exist (x)
					   (& r x))))

(frame 1.38 '(true)
	   (run q
			(exist (x)
				   (& true x)
				   (& x q))))

(frame 1.39 '(true)
	   (run q
			(exist (x)
				   (& x q)
				   (& true x))))

(frame 1.47 [:olive :oil]
	   (run x
			(cond-e 
			  ((& x :olive) succeed)
			  ((& x :oil) succeed)
			  (else fail))))

(frame "1.50" '(olive _.0 oil)
	   (run x
			(cond-e 
			  ((& x 'virgin) fail)
			  ((& x 'olive) succeed)
			  (succeed succeed)
			  ((& x 'oil) succeed)
			  (else fail))))

(frame 1.55 '((split pea soup) (navy bean soup))
	   (run r
			(exist (x y)
				   (cond-e
					 ((& x 'split) (& y 'pea))
					 ((& x 'navy) (& y 'bean))
					 (else fail))
				   (& (lcons x (lcons y (lcons 'soup ()))) r))))

(frame "1.55a" '((split pea soup) (navy bean soup))
	   (run r
			(exist (x y)
				   (cond-e
					 ((& x 'split) (& y 'pea))
					 ((& x 'navy) (& y 'bean))
					 (else fail))
				   (& [x y 'soup] r))))

; frame 1.56
(defn teacup-o [x]
	  (cond-e
		((& 'tea x) succeed)
		((& 'cup x) succeed)
		(else fail)))

(frame 1.56 '(tea cup)
	   (run x
			(teacup-o x)))

(frame 1.57 '((tea true) (cup true) (false true))
	   (run r
			(exist (x y)
				   (cond-e 
					 ((teacup-o x) (& true y) succeed)
					 ((& false x) (& true y))
					 (else fail))
				   (& (lcons x (lcons y ())) r))))

(frame "1.57a" '((tea true) (cup true) (false true))
	   (run r
			(exist (x y)
				   (cond-e 
					 ((teacup-o x) (& true y) succeed)
					 ((& false x) (& true y))
					 (else fail))
				   (& [x y] r))))

(frame 1.58 '((_.0 _.1) (_.0 _.1))
	   (run r
			(exist (x y z)
				   (cond-e
					 ((& y x) (exist (x) (& z x)))
					 ((exist (x) (& y x)) (& z x))
					 (else fail))
				   (& (lcons y (lcons z ())) r))))

(frame "1.58a" '((_.0 _.1) (_.0 _.1))
	   (run r
			(exist (x y z)
				   (cond-e
					 ((& y x) (exist (x) (& z x)))
					 ((exist (x) (& y x)) (& z x))
					 (else fail))
				   (& [y z] r))))

(frame 1.59 '((false _.0) (_.0 false))
	   (run r
			(exist (x y z)
				   (cond-e
					 ((& y x) (exist (x) (& z x)))
					 ((exist (x) (& y x)) (& z x))
					 (else fail))
				   (& false x)
				   (& (lcons y (lcons z ())) r))))

(frame 2.6 '(a)
	   (run r
			(first-o '(a c o r n) r)))

(frame "2.6a" [:a]
	   (run r
			(first-o [:a :c :o :r :n] r)))

(frame 2.8 '(pear)
	   (run r 
			(exist (x y)
				   (first-o (list r y) x)
				   (& 'pear x))))

(frame "2.8a" ['pear]
	   (run r 
			(exist (x y)
				   (first-o [r y] x)
				   (& 'pear x))))

(frame 2.15 '(c)
	   (run r
			(exist (v)
				   (rest-o '(a c o r n) v)
				   (first-o v r))))

(frame "2.15a" ['c]
	   (run r
			(exist (v)
				   (rest-o ['a 'c 'o 'r 'n] v)
				   (first-o v r))))

(frame "2.20" '(o)
	   (run x
			(rest-o '(c o r n) (list x 'r 'n))))

(frame 2.21 '((a c o r n))
	   (run l
			(exist (x)
				   (rest-o l '(c o r n))
				   (first-o l x)
				   (& 'a x))))

(frame 2.22 '(((a b c) d e))
	   (run l
			(cons-o '(a b c) '(d e) l)))

(frame 2.23 '(d)
	   (run x
			(cons-o x '(a b c) '(d a b c))))

(frame 2.24 '((e a d c))
	   (run r
			(exist (x y z)
				   (& (list 'e 'a 'd x) r)
				   (cons-o y (list 'a z 'c) r))))

(frame 2.25 '(d)
	   (run x
			(cons-o x (list 'a x 'c) (list 'd 'a x 'c))))

(frame 2.26 '((d a d c))
	   (run l
			(exist (x)
				   (& (list 'd 'a x 'c) l)
				   (cons-o x (list 'a x 'c) l))))

(frame 2.27 '((d a d c))
	   (run l
			(exist (x)
				   (cons-o x (list 'a x 'c) l)
				   (& (list 'd 'a x 'c) l))))

(frame 2.29 '((b e a n s))
	   (run l
			(exist (d x y w s)
				   (cons-o w '(a n s) s)
				   (rest-o l s)
				   (first-o l x)
				   (& 'b x)
				   (rest-o l d)
				   (first-o d y)
				   (& 'e y))))

(frame 2.32 []
	   (run q
			(null-o '(grape raisin pear))
			(& true q)))

(frame 2.33 '(true)
	   (run q
			(null-o ())
			(& true q)))

(frame 2.34 '(())
	   (run x
			(null-o x)))

(frame 2.38 []
	   (run q
			(eq-o 'pear 'plum)
			(& true q)))

(frame 2.39 '(true)
	   (run q
			(eq-o 'plum 'plum)
			(& true q)))

(frame 2.54 '(true)
	   (run q
			(pair-o (list q q))
			(& true q)))

(frame 2.55 []
	   (run q
			(pair-o ())
			(& true q)))

(frame 2.56 []
	   (run q
			(pair-o 'pair)
			(& true q)))

(frame 2.57 '[_.0 | _.1]
      (first (run x
                  (pair-o x))))

(frame 2.58 '(_.0)
	   (run r
			(pair-o (list r 'pear))))

(frame "3.10" '(())
	   (take 1
			 (run x
				  (list-o ['a 'b 'c | x]))))

(frame 3.14 '(() (_.0) (_.0 _.1) (_.0 _.1 _.2) (_.0 _.1 _.2 _.3))
	   (take 5
			 (run x
				  (list-o ['a 'b 'c | x]))))

(frame 3.21 '(true)
	   (run q
			(exist (x y)
				   (lol-o (list '(a b) (list x 'c) (list 'd y)))
				   (& true q))))

(frame 3.23 '(())
	   (take 1
			 (run x
				  (lol-o [['a 'b] ['c 'd] | x]))))

(frame 3.24 '(() (()) (()()) (()()()) (()()()()))
	   (take 5
			 (run x
				  (lol-o [['a 'b] ['c 'd] | x]))))

(frame 3.32 '(true)
	   (run q
			(twins-o '(tofu tofu))
			(& true q)))

(frame 3.33 '(tofu)
	   (run z
			(twins-o (list z 'tofu))))

(frame "" '(thing-1)
	   (run t
			(lot-o (list (list 'thing-1 t) '(thing-2 thing-2)))))

(frame "" '(thing-1)
	   (run t
			(exist (u)
				   (lot-o (list (list 'thing-1 t) (list u 'thing-2))))))

(frame 3.42 '(()
			  ((_.0 _.0))
			  ((_.0 _.0) (_.1 _.1))
			  ((_.0 _.0) (_.1 _.1) (_.2 _.2))
			  ((_.0 _.0) (_.1 _.1) (_.2 _.2) (_.3 _.3))) 
	   (take 5
			 (run z
				  (lot-o [['g 'g] | z]))))

(frame 3.45 '((e (_.0 _.0) ())
			  (e (_.0 _.0) ((_.1 _.1)))
			  (e (_.0 _.0) ((_.1 _.1) (_.2 _.2)))
			  (e (_.0 _.0) ((_.1 _.1) (_.2 _.2) (_.3 _.3)))
			  (e (_.0 _.0) ((_.1 _.1) (_.2 _.2) (_.3 _.3) (_.4 _.4))))
	   (take 5
			 (run r
				  (exist (w x y z)
						 (lot-o (incomplete (list '(g g) (list 'e w) (list x y)) z))
						 (& (list w (list x y) z) r)))))

(frame 3.47 '(((g g) (e e) (_.0 _.0))
			  ((g g) (e e) (_.0 _.0) (_.1 _.1))
			  ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2)))
	   (take 3
			 (run out
				  (exist (w x y z)
						 (& (incomplete (list '(g g) (list 'e w) (list x y)) z) out)
						 (lot-o out)))))

(frame 3.57 '(true)
	   (run q
			(member-o 'olive '(virgin olive oil))
			(& true q)))

(frame 3.58 '(hummus)
	   (take 1
			 (run y
				  (member-o y '(hummus with pita)))))

(frame 3.62 '(hummus with pita)
	   (run y
			(member-o y '(hummus with pita))))

(frame 3.66 '(e)
	   (run x
			(member-o 'e (list 'pasta x 'fagioli))))

(frame 3.71 '((e _.0) (_.0 e))
	   (run r
			(exist (x y)
				   (member-o 'e (list 'pasta x 'fagioli y))
				   (& (list x y) r))))

(frame 3.73 '[tofu | _.0]
      (first (run l
                  (member-o 'tofu l))))


(frame 3.76 '((tofu | _.0)
              (_.0 tofu | _.1)
              (_.0 _.1 tofu | _.2)
              (_.0 _.1 _.2 tofu | _.3)
              (_.0 _.1 _.2 _.3 tofu | _.4))
	   (take 5
			 (run l
				  (member-o 'tofu l))))

(frame 3.88 '(true true)
	   (run q
			(pmember-o 'tofu '(a b tofu d tofu))
			(& true q)))

(frame "4.10" '((tofu d tofu e))
	   (take 1
			 (run out
				  (mem-o 'tofu '(a b tofu d tofu e) out))))

(frame 4.11 '((tofu d tofu e))
	   (take 1
			 (run out
				  (exist (x)
						 (mem-o 'tofu (list 'a 'b x 'd 'tofu 'e) out)))))

(frame 4.12 '(tofu)
			 (run r
				  (mem-o r
						 '(a b tofu d tofu e)
						 '(tofu d tofu e))))

(frame 4.15 '(tofu)
			 (run x
				  (mem-o 'tofu
						 '(tofu e)
						 (list x 'e))))

(frame 4.16 []
			 (run x
				  (mem-o 'tofu
						 '(tofu e)
						 (list 'peas x))))

(frame 4.17 '((tofu d tofu e) (tofu e))
	   (run out
			(exist (x)
				   (mem-o 'tofu (list 'a 'b x 'd 'tofu 'e) out))))

(frame 4.18 '(_.0
              _.0
             (tofu | _.0)
             (_.0 tofu | _.1)
             (_.0 _.1 tofu | _.2)
             (_.0 _.1 _.2 tofu | _.3)
             (_.0 _.1 _.2 _.3 tofu | _.4))
	   (take 7 
			 (run z
				  (exist (u)
						 (mem-o 'tofu (incomplete '(a b tofu d tofu e) z) u)))))

(frame "4.30" '((a b d peas e))
	   (take 1
			 (run out
				  (exist (y)
						 (rember-o 'peas (list 'a 'b y 'd 'peas 'e) out)))))

(frame 4.31 '((b a d _.0 e)
			  (a b d _.0 e)
			  (a b d _.0 e)
			  (a b d _.0 e)
			  (a b _.0 d e)
			  (a b e d _.0)
			  (a b _.0 d _.1 e))
	   (run out
			(exist (y z)
				   (rember-o y (list 'a 'b y 'd z 'e) out))))

(frame 4.49 '((d d) (d d) (_.0 _.0) (e e))
	   (run r
			(exist (y z)
				   (rember-o y (list y 'd z 'e) (list y 'd 'e))
				   (& (list y z) r))))

(frame 4.57 '(_.0
              _.0
              _.0
              _.0
              _.0
              ()
              (_.0 | _.1)
              (_.0)
              (_.0 _.1 | _.2)
              (_.0 _.1)
              (_.0 _.1 _.2 | _.3)
              (_.0 _.1 _.2)
              (_.0 _.1 _.2 _.3 | _.4))
	   (take 13
			 (run w
				  (exist (y z out)
						 (rember-o y (incomplete (list 'a 'b y 'd z) w) out)))))

(frame 4.69 '(d)
	   (run r
			(& 'd r)
			(surprise-o r)))

(frame "4.70" '(_.0)
	   (run r
			(surprise-o r)))

(frame 4.71 '(b)
	   (run r
			(& r 'b)
			(surprise-o r)))

(frame "5.10" '((cake tastes yummy))
	   (run x
			(append-o '(cake) '(tastes yummy) x)))

(frame 5.11 '((cake with ice _.0 tastes yummy))
	   (run x
			(exist (y)
				   (append-o (list 'cake 'with 'ice y)
							 '(tastes yummy)
							 x))))

(frame 5.12 '(cake with ice cream | _.0)
      (first (run x
                  (exist (y)
                         (append-o '(cake with ice cream) y x)))))

(frame 5.13 '((cake with ice d t))
	   (take 1
			 (run x
				  (exist (y)
						 (append-o (incomplete '(cake with ice) y) '(d t) x)))))

(frame 5.26 '((() (cake with ice d t))
			  ((cake) (with ice d t))
			  ((cake with) (ice d t))
			  ((cake with ice) (d t))
			  ((cake with ice d) (t))
			  ((cake with ice d t) ()))
	   (take 6
			 (run r
				  (exist (x y)
						 (append-o x y '(cake with ice d t))
						 (& (list x y) r)))))

(frame 5.33 '(()
			  (_.0)
			  (_.0 _.1)
			  (_.0 _.1 _.2)
			  (_.0 _.1 _.2 _.3)
			  (_.0 _.1 _.2 _.3 _.4)
			  (_.0 _.1 _.2 _.3 _.4 _.5))
	   (take 7
			 (run x
				  (exist (y z)
						 (append-o x y z)))))

(frame 5.34 '(_.0 _.0 _.0 _.0 _.0 _.0 _.0)
	   (take 7
			 (run y
				  (exist (x z)
						 (append-o x y z)))))

(frame 5.46 '((((pizza)))
			  ((pizza))
			  (pizza)
			  pizza)
	   (run x
			(unwrap-o '(((pizza))) x)))

(frame 5.75 '((((a b) c))
			  ((a b) (c))
			  ((a b) c ())
			  ((a b) c)
			  (a (b) (c))
			  (a (b) c ())
			  (a (b) c)
			  (a b () (c))
			  (a b () c ())
			  (a b () c)
			  (a b (c))
			  (a b c ())
			  (a b c))
	   (run x
			(flattenrev-o '((a b) c) x)))

(frame 5.80 574
	   (count 
		 (run x
			  (flattenrev-o '((((a (((b))) c))) d) x))))

(frame "6.10" '(true true true true true)
	   (take 5
			 (run q
				  always-o
				  (& true q))))

(frame 6.19 '(true)
	   (take 1
			 (run q
				  (cond-i
					((& false q) )
					(else (& true q)))
				  (& true q))))

(frame "6.20" '(true true true true true)
	   (take 5
			 (run q
				  (cond-i
					((& false q) )
					(else (any-o (& true q))))
				  (& true q))))

(frame 6.24 '(tea false cup)
	   (take 5
			 (run r
				  (cond-i
					((teacup-o r) succeed)
					((& false r) succeed)
					(else fail)))))

(frame 10.5 '(olive)
	   (run x
			(cond-a
			  ((& 'olive x) succeed)
			  ((& 'oil x) succeed)
			  (else fail))))

(frame 10.7 []
	   (run x
			(cond-a
			  ((& 'virgin x) fail)
			  ((& 'olive x) succeed)
			  ((& 'oil x) succeed)
			  (else fail))))

(frame 10.8 []
	   (run q
			(exist [x y]
				   (& 'split x)
				   (& 'pea y)
				   (cond-a
					 ((& 'split x) (& x y))
					 (else succeed)))
			(& true q)))

(frame 10.9 '(true)
	   (run q
			(exist [x y]
				   (& 'split x)
				   (& 'pea y)
				   (cond-a
					 ((& x y) (& 'split x))
					 (else succeed)))
			(& true q)))

(frame 10.11 '(spaghetti)
	   (run x
			(cond-a
			  ((not-pasta-o x) fail)
			  (else (& 'spaghetti x)))))

(frame 10.12 []
	   (run x
			(& 'spaghetti x) 
			(cond-a
			  ((not-pasta-o x) fail)
			  (else (& 'spaghetti x)))))

(frame 10.14 '(true)
	   (run q
			(cond-u
			  (always-o succeed)
			  (else fail))
			(& true q)))

(frame 10.18 []
	   (take 1
			 (run q
				  (cond-u
					(always-o succeed)
					(else fail))
				  fail
				  (& true q))))

(frame 10.19 '(tea)
	   (run x
			(once-o (teacup-o x))))

(frame "10.20" []
	   (run q
			(once-o (sal-o never-o))
			fail))

(frame 10.24 '(false)
	   (run r
			(& false r)
			(cond-u
			  ((teacup-o r) succeed)
			  ((& false r) succeed)
			  (else fail))))

; solve the 'zebra' problem
(defn- on-right [x y l]
  (exist [fst rst scnd]
         (& [fst scnd | _] l)
         (cond-e
           ((& fst x) (& scnd y))
           ((rest-o l rst) (on-right x y rst)))))

(defn- next-to [x y l]
  (cond-e
    ((on-right x y l))
    ((on-right y x l))))

(defn- zebra [h]
  (all
    (& [_ _ [_ _ :milk _ _] _ _] h)
    (first-o h [:norwegian _ _ _ _])
    (next-to [:norwegian _ _ _ _] [_ _ _ _ :blue] h)
    (on-right [_ _ _ _ :ivory] [_ _ _ _ :green] h)
    (member-o [:englishman _ _ _ :red] h)
    (member-o [_ :kools _ _ :yellow] h)
    (member-o [:spaniard _ _ :dog _] h)
    (member-o [_ _ :coffee _ :green] h) 
    (member-o [:ukrainian _ :tea _ _] h)
    (member-o [_ :luckystrikes :oj _ _] h)
    (member-o [:japanese :parliaments _ _ _] h)
    (member-o [_ :oldgolds _ :snails _] h)
    (next-to [_ _ _ :horse _] [_ :kools _ _ _] h)
    (next-to [_ _ _ :fox _] [_ :chesterfields _ _ _] h)))

(assert (= '[[[:norwegian :kools _.0 :fox :yellow]
                [:ukrainian :chesterfields :tea :horse :blue]
                [:englishman :oldgolds :milk :snails :red]
                [:spaniard :luckystrikes :oj :dog :ivory]
                [:japanese :parliaments :coffee _.1 :green]]]
           (run q
                (zebra q))))
       )

(println "success mini-kanren")

