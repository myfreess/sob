(ns sob.ast
  (:gen-class))
  
;; type Expr
;; 我靠，是不是我跟typed clojure犯冲啊
;; 每次import之后repl就打不开
;; (defn varE [ch] ch)
;; (defn constE [bool] bool)
(defn andE [& e] (into [:and] e))
(defn orE [& e] (into [:or] e))
(defn notE [e] [:not e])


;; Predicates

(def compound? vector?)
(def constE? boolean?)
(def varE? char?)
(defn andE? [e] (and (compound? e) (= (first e) :and)))
(defn orE?  [e] (and (compound? e) (= (first e) :or)))
(defn notE? [e] (and (compound? e) (= (first e) :not)))

(defn bexpr-map
  "fmap for boolean expr's polynomial functor"
  [f e]
  (cond
    (constE? e) e
    (varE? e)   e
    (notE? e)
    (let [[_ e1] e] (notE (f e1)))
    (andE? e)
    (let [[_ x y] e] (andE (f x) (f y)))
    (orE? e)
    (let [[_ x y] e] (orE (f x) (f y)))
    :else nil))

