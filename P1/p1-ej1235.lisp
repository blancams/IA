;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EJERCICIO 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EJERCICIO 1.1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; check-zero (x)
;;; Comprueba si x es el vector cero
;;;
;;; INPUT: x: vector, representado como una lista
;;;
;;; OUTPUT: t si x es el vector cero, nil en caso contrario
;;;
(defun check-zero (x)
   (if (null x)
      t
      (and (= (first x) 0) (check-zero (rest x)))))

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
   (if (or (null x) (null y) (check-zero x) (check-zero y))
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
   (if (or (null x) (null y) (check-zero x) (check-zero y))
   nil
   (/ (prod-esc-mapcar x y)
      (* (sqrt (prod-esc-mapcar x x))
         (sqrt (prod-esc-mapcar y y))))))

;;; EJERCICIO 1.2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-conf-gendict (x vs)
;;; Genera un diccionario con la similitud coseno entre un vector y todos los vectores
;;; de una lista de vectores
;;;
;;; INPUT: x: vector, representado como una lista
;;; vs: vector de vectores, representado como una lista de listas
;;;
;;; OUTPUT: diccionario formado por las similitudes y los vectores
;;;
(defun sc-conf-gendict (x vs)
   (mapcar #'(lambda (y) (list (sc-rec x y) y)) vs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-conf-selvec (lst)
;;; De un diccionario obtiene el vector correspondiente a cada similitud
;;;
;;; INPUT: lst: diccionario de similitudes-vectores
;;;
;;; OUTPUT: lista de vectores procedentes del diccionario en el mismo orden
;;;
(defun sc-conf-selvec (lst)
   (mapcar #'(lambda (y) (car (cdr y))) lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-conf (cat vs conf)
;;; Devuelve aquellos vectores similares a una categoria
;;;
;;; INPUT: cat: vector que representa a una categoria, representado como una lista
;;; vs: vector de vectores, representado como una lista de listas
;;; conf: Nivel de confianza
;;; OUTPUT: Vectores cuya similitud es superior al nivel de confianza, ordenados
;;;
(defun sc-conf (cat vs conf)
   (sc-conf-selvec (sort (remove conf (sc-conf-gendict cat vs) :test #'> :key #'first) #'< :key #'first)))

;;; EJERCICIO 1.3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-class-gendict (x vs func)
;;; Realiza la misma funcion que sc-conf-gendict pero ahora tenemos en cuenta
;;; que los vectores tienen un identificador, y la funcion para calcular la
;;; similitud se pasa como parametro
;;;
;;; INPUT: x: vector, representado como una lista
;;; vs: vector de vectores, representado como una lista de listas
;;; func: funcion con la que calcular la similitud
;;;
;;; OUTPUT: diccionario formado por las similitudes y los identificadores de los vectores
;;;
(defun sc-class-gendict (x vs func)
   (mapcar #'(lambda (y) (list (funcall func (rest x) (rest y)) (car y))) vs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-class-selvec (lst)
;;; Realiza la misma funcion que sc-conf-selvec pero ahora en vez de devolver el
;;; vector, devuelve su identificacion junto con la similitud en un par
;;;
;;; INPUT: lst: diccionario de similitudes-vectores
;;;
;;; OUTPUT: lista de pares con identificadores de vectores y similitudes
;;;
(defun sc-class-selvec (lst)
   (mapcar #'(lambda (y) (cons (second y) (first y))) lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-conf-alt (x vs func)
;;; Devuelve lista de pares identificador-similitud ordenados por similitud a uno dado
;;;
;;; INPUT: x: vector, representado como una lista
;;; vs: vector de vectores, representado como una lista de listas
;;; func: funcion para calcular similitud
;;; OUTPUT: Pares identificador-similitud ordenados
;;;
(defun sc-conf-alt (x vs func)
   (sc-class-selvec (sort (sc-class-gendict x vs func) #'< :key #'first)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-classifier (cats texts func)
;;; Clasifica a los textos en categorías.
;;;
;;; INPUT: cats: vector de vectores, representado como una lista de listas
;;; texts: vector de vectores, representado como una lista de listas
;;; func: función para evaluar la similitud coseno
;;; OUTPUT: Pares identificador de categoría con resultado de similitud coseno
;;;
(defun sc-classifier (cats texts func)
   (mapcar #'(lambda (y) (car (sc-conf-alt y cats func))) texts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EJERCICIO 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   (cond ((< (- b a) tol) pto-medio)
         ((> (* (funcall f a) (funcall f b)) 0) NIL)
         ((>= (* (funcall f a) (funcall f pto-medio)) 0) (bisect f pto-medio b tol))
         (t (bisect f a pto-medio tol)))))



(defun allroot-aux (f lst tol ret)
   (if (and (null (rest lst)) (not (null (first lst))))
      ret
      (allroot-aux f (rest lst) tol (append ret (list (bisect f (first lst) (second lst) tol))))))

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

(defun allind-aux (f a incr tol max ret)
   (let ((b (+ a incr)))
   (if (= b max)
      ret
      (allind-aux f b incr tol max (append ret (list (bisect f a b tol)))))))

(defun allind (f a b N tol)
   (allind-aux f a (/ (- b a) (expt 2 N)) tol b NIL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EJERCICIO 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EJERCICIO 3.1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst (elt lst)
;;; Genera una lista con las combinaciones de un elemento con los elementos de una lista
;;;
;;; INPUT: elt: elemento a combinar
;;; lst: lista de elementos a combinar
;;;
;;; OUTPUT: Lista con todas las posibles combinaciones
;;;
(defun combine-elt-lst (elt lst)
   (if (null lst)
      NIL
      (append (list elt (first lst)) (combine-elt-lst elt (rest lst)))))

;;; EJERCICIO 3.2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst (lst1 lst2)
;;; Genera el producto cartesiano lst1 X lst2 (todas las combinaciones de
;;; elementos entre ambas listas)
;;;
;;; INPUT: lst1: primera lista de elementos a combinar
;;; lst2: segunda lista de elementos a combinar
;;;
;;; OUTPUT: Lista con todas las posibles combinaciones
;;;
(defun combine-lst-lst (lst1 lst2)
   (if (or (null lst1) (null lst2))
      NIL
      (append (combine-elt-lst (first lst1) lst2) (combine-lst-lst (rest lst1) lst2))))

;;; EJERCICIO 3.3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lol (elt lol)
;;; Genera una lista con las combinaciones de un elemento con los elementos de una lista de listas
;;;
;;; INPUT: elt: elemento a combinar
;;; lol: lista de listas de elementos a combinar
;;;
;;; OUTPUT: Lista con todas las posibles combinaciones
;;;
(defun combine-elt-lol (elt lol)
   (mapcar #'(lambda (y) (cons elt y)) lol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lol (lst lol)
;;; Genera el producto cartesiano de una lista con todas las listas de una lista de listas
;;;
;;; INPUT: lst: lista de elementos a combinar
;;; lol: lista de listas de elementos a combinar
;;;
;;; OUTPUT: Lista con todas las posibles combinaciones
;;;
(defun combine-lst-lol (lst lol)
   (mapcan #'(lambda (z) (combine-elt-lol z lol)) lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lsts (lstolsts)
;;; Genera una lista con todas las disposiciones de elementos pertenecientes a listas
;;; contenidas en una lista de listas
;;;
;;; INPUT: lstolsts: lista de listas de la que sacar todas las combinaciones
;;;
;;; OUTPUT: Lista con todas las posibles combinaciones
;;;
(defun combine-list-of-lsts (lstolsts)
   (if (null lstolsts)
      '(nil)
      (append (combine-lst-lol (first lstolsts) (combine-list-of-lsts (rest lstolsts))))))
