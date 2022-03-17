(ns sob.cnf
  (:gen-class)
  (:require [sob.ast :as ast]
            [clojure.walk :as w]))


;; 一些用choose串联的处理函数


(defn remove-double-negation
  "return nil or reduced boolean expr"
  [k e]
  (if (ast/notE? e)
    (let [[_ e1] e]
      (if (ast/notE? e1)
        (let [[_ e2] e1] (k e2)) nil)) nil))

(defn n-distribute-over-and
  "transform not (and x y) into or (not x) (not y)"
  [k e]
  (if (ast/notE? e)
    (let [[_ e1] e]
      (if (ast/andE? e1)
        (let [[_ x y] e1]
          (ast/orE (k (ast/notE x)) (k (ast/notE y)))) nil)) nil))

(defn n-distribute-over-or
  "transform not (or x y) into and (not x) (not y)"
  [k e]
  (if (ast/notE? e)
    (let [[_ e1] e]
      (if (ast/orE? e1)
        (let [[_ x y] e1]
          (ast/andE (k (ast/notE x)) (k (ast/notE y)))) nil)) nil))

(defn put-not-into-const
  "transform not (const e) into const (! e)"
  [k e]
  (if (ast/notE? e)
    (let [[_ e1] e]
      (if (ast/constE? e1)
        (not e1) nil)) nil))


(defn partial-foldr
  "foldr for non-empty list"
  [f l]
  (if (empty? (rest l))
    (first l)
    (f (first l) (partial-foldr f (rest l)))))

(defmacro choose
  "choose the first non-nil value"
  [& branchs]
  (partial-foldr
   (fn [curr next]
     (let [tmp (gensym)]
       `(let [~tmp ~curr]
          (if (nil? ~tmp) ~next ~tmp)))) branchs))

(defn fix-negations
  "removing negations"
  [e]
  (let [k fix-negations]
    (choose
     ;; 去除双重否定
     (remove-double-negation k e)
     ;; 德摩根
     (n-distribute-over-and  k e)
     (n-distribute-over-or   k e)
     ;; 处理常量
     (put-not-into-const     k e)
     ;; 递归
     (ast/bexpr-map          k e))))

