;;; EJERCICIO 1.1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prod-esc-rec (x y)
;;; Calcula el producto escalar de dos vectores de forma recursiva
;;;
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;;
;;; OUTPUT: producto escalar entre x e y
;;;      
(defun prod-esc-rec (x y)
   (if (or (null x) (null y))
      0
      (+ (* (first x) (first y))
         (prod-esc-rec (rest x) (rest y)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prod-esc-mapcar (x y)
;;; Calcula el producto escalar de dos vectores usando mapcar
;;;
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;;
;;; OUTPUT: producto escalar entre x e y
;;; 
(defun prod-esc-mapcar (x y)
   (apply #'+ (mapcar #'* x y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; check (x)
;;; Revisa que todos los elementos de una lista sean positivos
;;;
;;; INPUT: x: lista
;;;
;;; OUTPUT: T si todos son positivos; nil si alguno no lo es
;;; 
(defun check (x)
   (every #'identity (mapcar #'(lambda (y) (> y 0)) x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-rec (x y)
;;; Calcula la similitud coseno de un vector de forma recursiva
;;;
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;;
;;; OUTPUT: similitud coseno entre x e y
;;;      
(defun sc-rec (x y)
   (if (or (null(check x)) (null(check y)) (not (= (length x) (length y))))
   nil
   (/ (prod-esc-rec x y)
      (* (sqrt (prod-esc-rec x x))
         (sqrt (prod-esc-rec y y))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-mapcar (x y)
;;; Calcula la similitud coseno de un vector usando mapcar
;;;
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;;
;;; OUTPUT: similitud coseno entre x e y
;;;
(defun sc-mapcar (x y)
   (if (or (null(check x)) (null(check y)) (not (= (length x) (length y))))
   nil
   (/ (prod-esc-mapcar x y)
      (* (sqrt (prod-esc-mapcar x x))
         (sqrt (prod-esc-mapcar y y))))))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EJERCICIO 2 ;;;;;;;;;;;;;;;;;;;;;;;

;; Finds a root of f between the points a and b using bisection.
;;
;; If f(a)f(b)>0 there is no guarantee that there will be a root in the
;; interval, and the function will return NIL.
;;
;; f: function of a single real parameter with real values whose root
;; we want to find
;; a: lower extremum of the interval in which we search for the root
;; b: b>a upper extremum of the interval in which we search for the root
;; tol: tolerance for the stopping criterion: if b-a < tol the function
;; returns (a+b)/2 as a solution.
;;
(defun bisect (f a b tol)
   (let ((pto-medio (/ (+ a b) 2)))
   (cond    ((= (funcall f pto-medio) 0) pto-medio)
         ((> (* (funcall f a) (funcall f b)) 0) NIL)
         ((< (- b a) tol) pto-medio)
         ((> (* (funcall f a) (funcall f pto-medio)) 0) (bisect f pto-medio b (/ tol 2))) 
         (t (bisect f a pto-medio (/ tol 2))))))