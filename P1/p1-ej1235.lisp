;;; EJERCICIO 1.1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; check-list (x)
;;; Revisa que el argumento es una lista
;;;
;;; INPUT: x: elemento a comprobar si es lista
;;;
;;; OUTPUT: T si x es una lista, nil en caso contrario
;;;
(defun check-list (x)
   (if (null x)
      t
      (and (list x) (check-list (rest x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; check-pos (x)
;;; Revisa que todos los elementos de una lista sean positivos
;;;
;;; INPUT: x: lista
;;;
;;; OUTPUT: T si todos son positivos; nil si alguno no lo es
;;;
(defun check-pos (x)
   (every #'identity (mapcar #'(lambda (y) (> y 0)) x)))

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
;;; sc-rec (x y)
;;; Calcula la similitud coseno de un vector de forma recursiva
;;;
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;;
;;; OUTPUT: similitud coseno entre x e y
;;;
(defun sc-rec (x y)
   (if (or (not (check-list x)) (not (check-list y)) (null(check-pos x)) (null(check-pos y)) (not (= (length x) (length y))))
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
   (if (or (not (check-list x)) (not (check-list y)) (null(check-pos x)) (null(check-pos y)) (not (= (length x) (length y))))
   nil
   (/ (prod-esc-mapcar x y)
      (* (sqrt (prod-esc-mapcar x x))
         (sqrt (prod-esc-mapcar y y))))))

;;; EJERCICIO 1.2

<<<<<<< HEAD



=======
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-conf (x vs conf)
;;; Devuelve aquellos vectores similares a una categoria
;;;
;;; INPUT: x: vector, representado como una lista
;;; vs: vector de vectores, representado como una lista de listas
;;; conf: Nivel de confianza
;;; OUTPUT: Vectores cuya similitud es superior al nivel de confianza, ordenados
;;;
;;; NO ESTÁ TERMINADO
(defun sc-conf (x vs conf)
   (remove conf (mapcar #'(lambda (y) (list (sc-rec x y) y)) vs) :test #'> :key #'car))
>>>>>>> 9dfa51f6e25328abe74dbf0f610c09dce5ae4c30

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
<<<<<<< HEAD
         ((> (* (funcall f a) (funcall f pto-medio)) 0) (bisect f pto-medio b tol)) 
         (t (bisect f a pto-medio tol)))))



(defun allroot-aux (f lst tol ret)
   (cond ((and (null (rest lst)) (not (null (first lst)))) ret)
        (t (append ret (bisect f (first lst) (second lst) tol))
        (allroot-aux f (rest lst) tol ret))))

;;
;; Finds all the roots that are located between consecutive values of a list
;; of values
;;
;; Parameters:
;;
;; f: function of a single real parameter with real values whose root
;; we want to find
;; lst: ordered list of real values (lst[i] < lst[i+1])
;; tol: tolerance for the stopping criterion: if b-a < tol the function
;; returns (a+b)/2 as a solution.
;;
;; Whenever sgn(f(lst[i])) != sgn(f(lst[i+1])) this function looks for a
;; root in the corresponding interval.
;;
;; Returns: A list o real values containing the roots of the function in the
;: given sub-intervals
;;
(defun allroot (f lst tol)
   (allroot-aux f lst tol NIL))

;;(allroot #'(lambda(x) (sin (* 6.28 x))) '(0.25 0.75 1.25 1.75 2.25) 0.0001)
;; Para ver si tiene solo un elemento: 
;; (and (null (rest lst)) (not (null (first lst))))
;;(allroot #'(lambda(x) (sin (* 6.26 x))) '(0.25 0.75) 0.001)
;;(bisect #'(lambda(x) (sin (* 6.28 x))) 1.1 2.1 0.001)
=======
         ((> (* (funcall f a) (funcall f pto-medio)) 0) (bisect f pto-medio b (/ tol 2)))
         (t (bisect f a pto-medio (/ tol 2))))))
>>>>>>> 9dfa51f6e25328abe74dbf0f610c09dce5ae4c30
