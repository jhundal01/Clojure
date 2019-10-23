(ns clojure-noob.core

  (:gen-class))
; recursion needed in simplify 2-5 lines, look into deepsubstitue recursion

(def p1 '(and x (or x (and y (not z)))))
(def p2 '(and (and z false) (or x true false)))
(def p3 '(or true a))

(defn bind-values [m l] ; deep-susbtituting the bindings in the expression
  (map 
  (fn [i] 
         (cond                      
           (seq? i) (bind-values m i) 
           :default (m i i)))
          l))
; (defn nor-convert [exp])
;   (let [exp (if (seq?) exp)
;     (map (fn [i] (if (seq? i)(simpligy i)i)) exp)]
;     (let [op (first exp) args (distinct (rest exp))]
;     (if (= op 'not)
    
;     )
;     )
;   )


; Simplify is a function that computes the solution to a particular form with an operator.
; It takes in n number of arguments with one operator.
(defn simplify [exp]
 (let [exp (if (seq? exp)
    (map (fn [i] (if (seq? i) (simplify i)i)) exp) exp)] ; checking if exp is a sequence and mapping simpligy over it and replacing the value of exp
       (let [op (first exp) args (distinct (rest exp))]; splitting the exp into op and args
  (if (= op 'or) ; checking if the operator is "or".
    (let [nofalse (remove false? args)] ; removing false form the expression as it makes or computations easier
      (cond
          (= 0 (count nofalse)) false ; if the expression had just the argument "false" then nofalse will be empty and the answer will be false
          (some true? nofalse) true ; if an or expression includes true, the solution is true
          (= 1 (count nofalse)) (first nofalse) ; if the expression has an argument except true or false return that, first to exclude parenthesis. 
          :otherwise (conj nofalse op))); if the expression has no false or true, it returns nofalse.
  (if (= op 'and) ; checking if the operator is "and".
    (let [notrue (remove true? args)] ; removing true form the expression as it makes computations easier
      (cond
          (= 0 (count notrue)) true ; if the expression had just the argument "true" then notrue will be empty and the answer will be true
          (some false? notrue) false ; if an or expression includes true, the solution is true
          (= 1 (count notrue)) (first notrue) ; if the expression has an argument except true or false return that, first to exclude parenthesis. 
          :otherwise (conj notrue op))) ; if the expression has no false or true, it returns notrue. 
  (if (= op 'not)
    (if (= 1 (count args))
      (cond
          (some true? args) false ; if argument is true, output false
          (some false? args) true; if argument is false, output true
          :else "not can only accept true or false")"not accepts only one argument")))))))
(defn evalexp [exp bindings] 
  (simplify (bind-values bindings exp)))
(defn -main
  [& args])
