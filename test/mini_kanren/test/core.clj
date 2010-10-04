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

(ns mini-kanren.test.core
  (:use mini-kanren.core :reload-all)
  (:use clojure.test))

; questions from the book as unit tests

(defmacro frame [frame-num value expr]
  `(deftest ~(symbol (format "frame-%s" frame-num))
            (let [result# ~expr]
              (is (= ~value result#)))))

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

(deftest test-zebra
        (is (= '[[[:norwegian :kools _.0 :fox :yellow]
                  [:ukrainian :chesterfields :tea :horse :blue]
                  [:englishman :oldgolds :milk :snails :red]
                  [:spaniard :luckystrikes :oj :dog :ivory]
                  [:japanese :parliaments :coffee _.1 :green]]]
               (run q
                    (zebra q)))))

