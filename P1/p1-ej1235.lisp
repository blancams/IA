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
      (and (listp x) (check-list (rest x)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auxsort (x lst)
;;; Funcion auxiliar para la implementacion de insercion directa en diccionarios
;;;
;;; INPUT: x: vector a añadir (en orden) a la lista
;;; lst: lista a la que introducir el vector x
;;;
;;; OUTPUT: lista ordenada con el elemento x introducido
;;;
(defun auxsort (x lst)
(if (null lst)
   (list x)
   (if (> (car x) (car (car lst)))
       (cons x lst)
       (cons (car lst) (auxsort x (cdr lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sortdict (lst)
;;; Funcion que implementa la ordenacion en un diccionario en base al primer elemento del par
;;;
;;; INPUT: lst: diccionario a ordenar
;;;
;;; OUTPUT: diccionario ordenado
;;;
(defun sortdict (lst)
(if (null lst)
   lst
   (auxsort (car lst) (sortdict (cdr lst)))))

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
;;; sc-conf (x vs conf)
;;; Devuelve aquellos vectores similares a una categoria
;;;
;;; INPUT: x: vector, representado como una lista
;;; vs: vector de vectores, representado como una lista de listas
;;; conf: Nivel de confianza
;;; OUTPUT: Vectores cuya similitud es superior al nivel de confianza, ordenados
;;;
(defun sc-conf (x vs conf)
   (sc-conf-selvec (sortdict (remove conf (sc-conf-gendict x vs) :test #'> :key #'car))))

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
   (sc-class-selvec (sortdict (sc-class-gendict x vs func))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-classifier (cats texts func)
;;; Clasifica a los textos en categorías.
;;;
;;; INPUT: cats: vector de vectores, representado como una lista de listas
;;; vs: vector de vectores, representado como una lista de listas
;;; func: referencia a función para evaluar la similitud coseno
;;; OUTPUT: Pares identificador de categoría con resultado de similitud coseno
;;;
(defun sc-classifier (cats texts func)
   (mapcar #'(lambda (y) (car (sc-conf-alt y cats func))) texts))

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




(defun combine-elt-lst (elt lst)
   (if (null lst)
      NIL
      (append (list (list elt (first lst))) (combine-elt-lst elt (rest lst)))))

(defun combine-lst-lst (lst1 lst2)
   (if (or (null lst1) (null lst2))
      NIL
      (append (combine-elt-lst (first lst1) lst2) (combine-lst-lst (rest lst1) lst2))))


(defun combine-elt-list-of-lsts (elt lstolsts)
   (if (null lstolsts)
      NIL
      (append (list (append (first lstolsts) elt)) (combine-elt-list-of-lsts elt (rest lstolsts)))))

(defun combine-list-of-lsts-lst (lst lstolsts)
   (if (or (null lst) (null lstolsts))
      NIL
      (append (combine-elt-list-of-lsts (first lst) lstolsts) 
         (combine-lst-lst (rest lst) lstolsts))))

(defun combine-list-of-lsts-aux (done todo)
   (if (null todo)
      done
      (combine-list-of-lsts-aux (combine-lst-lst done (first todo)) (rest todo))))



(defun combine-list-of-lsts (lstolsts) 
    (combine-list-of-lsts-aux (combine-lst-lst (first lstolsts) (second lstolsts)) (cddr lstolsts)))

