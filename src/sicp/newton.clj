(ns sicp.newton)

(defn new-if
  [predicate then-clause else-clause]
  (cond (= predicate true) then-clause
    :else else-clause))

(defn square
  [num]
  (* num num))

(defn abs
  [num]
  (if (< num 0) (- num) num))

(defn good-enough?
  [guess num]
  (< (abs (- (square guess) num)) 0.001))

(defn average
  [a b]
  (/ (+ a b) 2))

(defn improve
  [guess num]
  (average guess (/ num guess)))

(defn square-iter
  [guess num]
  (if (good-enough? guess num) guess
    (square-iter (improve guess num) num)))

(defn square-root
  [num]
  (square-iter 1.0 num))

(defn cubic-root-iter [x y]
  (/
    (+ (/ x (square y)) (* 2 y))
    3))