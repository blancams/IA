;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EJERCICIO 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EJERCICIO 1.1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; check-zero (x)
;;; Comprueba si x es el vector cero
;;;
;;; INPUT: x: vector, representado como una lista
;;;
;;; OUTPUT: t si x es el vector cero o nil, nil en caso contrario
;;;
(defun check-zero (x)
   (if (null x)
      t
      (and (= (first x) 0)
           (check-zero (rest x)))))
;;;
;;; EJEMPLOS:
;;; (check-zero '()) ;-> t          ; caso base
;;; (check-zero '(0 0 0)) ;-> t     ; caso tipico
;;; (check-zero '(1 0 0)) ;-> nil   ; caso tipico
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;
;;; EJEMPLOS:
;;; (prod-esc-rec '() '()) ;-> 0          ; caso base
;;; (prod-esc-rec '(1 2) '(3 4)) ;-> 11   ; caso tipico
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;;;
;;; EJEMPLOS:
;;; (prod-esc-mapcar '() '()) ;-> 0          ; caso base
;;; (prod-esc-mapcar '(1 2) '(3 1)) ;-> 5    ; caso tipico
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
   (unless (or (check-zero x) (check-zero y))
      (/ (prod-esc-rec x y)
         (* (sqrt (prod-esc-rec x x))
            (sqrt (prod-esc-rec y y))))))
;;;
;;; EJEMPLOS:
;;; (sc-rec '() '()) ;-> nil                 ; caso no permitido
;;; (sc-rec '(0 0) '(0 0)) ;-> nil           ; caso no permitido
;;; (sc-rec '(1 2) '(2 3)) ;-> 0.99227786    ; caso tipico
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
   (unless (or (check-zero x) (check-zero y))
      (/ (prod-esc-mapcar x y)
         (* (sqrt (prod-esc-mapcar x x))
            (sqrt (prod-esc-mapcar y y))))))
;;;
;;; EJEMPLOS:
;;; (sc-mapcar '() '()) ;-> nil          ; caso no permitido
;;; (sc-mapcar '(0 0) '(0 0)) ;-> nil    ; caso no permitido
;;; (sc-mapcar '(1 2) '(2 4)) ;-> 1.0    ; caso tipico
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;
;;; EJEMPLOS:
;;; (sc-conf-gendict '(1 2) '((1 2) (1 3))) ;-> ((1.0 (1 2)) (0.98994946 (1 3)))
;;; (sc-conf-gendict '(2 3) '((1 1) (1 2))) ;-> ((0.9805807 (1 1)) (0.99227786 (1 2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-conf-selvec (lst)
;;; De un diccionario obtiene el vector correspondiente a cada similitud
;;;
;;; INPUT: lst: diccionario de similitudes-vectores
;;;
;;; OUTPUT: lista de vectores procedentes del diccionario en el mismo orden
;;;
(defun sc-conf-selvec (lst)
   (mapcar #'(lambda (y) (first (second y))) lst))
;;;
;;; EJEMPLOS:
;;; (sc-conf-selvec '((1.0 (1 2)) (0.98994946 (1 3)))) ;-> ((1 2) (1 3))
;;; (sc-conf-selvec '((0.9805807 (1 1)) (0.99227786 (1 2)))) ;-> ((1 1) (1 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   (sc-conf-selvec (sort (remove conf (sc-conf-gendict cat vs) :test #'> :key #'first) #'> :key #'first)))
;;;
;;; EJEMPLOS:
;;; (sc-conf '(1 2) '((1 2) (1 3) (1 4) (2 30)) 0.95) ;-> ((1 2) (1 3) (1 4))
;;; (sc-conf '(2 5) '((1 5) (4 10)) 0.9) ;-> ((4 10) (1 5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;
;;; EJEMPLOS:
;;; (sc-class-gendict '(1 1 2) '((1 1 2) (2 1 3)) #'sc-rec) ;-> ((1.0 1) (0.98994946 2))
;;; (sc-class-gendict '(1 2 3) '((1 1 1) (2 1 2)) #'sc-mapcar) ;-> ((0.9805807 1) (0.99227786 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;
;;; EJEMPLOS:
;;; (sc-class-selvec '((1.0 1) (0.98994946 2))) ;-> ((1 . 1.0) (2 . 0.98994946))
;;; (sc-class-selvec '((0.9805807 1) (0.99227786 2))) ;-> ((1 . 0.9805807) (2 . 0.99227786))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   (sc-class-selvec (sort (sc-class-gendict x vs func) #'> :key #'first)))
;;;
;;; EJEMPLOS:
;;; (sc-conf-alt '(2 1 2) '((1 1 2) (2 1 3) (3 1 4) (4 2 30)) #'sc-rec) ;-> ((1 . 1.0) (2 . 0.98994946) (3 . 0.97618705) (4 . 0.9221943))
;;; (sc-conf-alt '(3 2 5) '((1 1 5) (2 4 10)) #'sc-mapcar) ;-> ((2 . 1.0) (1 . 0.98328197))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;
;;; EJEMPLOS:
;;; (sc-classifier '((1 1 2) (2 5 10)) '((1 1 3) (2 1 4) (3 2 5)) #'sc-rec) ;-> ((1 . 0.98994946) (2 . 0.9761871) (1 . 0.9965458))
;;; (sc-classifier '((1 43 23 12) (2 33 54 24)) '((1 3 22 134) (2 43 26 58)) #'sc-mapcar) ;-> ((2 . 0.48981872) (1 . 0.81555086))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   (unless (null lst)
      (append (list (list elt (first lst)))
              (combine-elt-lst elt (rest lst)))))
;;;
;;; EJEMPLOS:
;;; (combine-elt-lst 'a nil) ;-> nil                     ; caso base
;;; (combine-elt-lst nil '(1 2)) ;-> ((NIL 1) (NIL 2))   ; caso atipico
;;; (combine-elt-lst 'a '(1 2)) ;-> ((A 1) (A 2))        ; caso tipico
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   (unless (or (null lst1) (null lst2))
      (append (combine-elt-lst (first lst1) lst2)
              (combine-lst-lst (rest lst1) lst2))))
;;;
;;; EJEMPLOS:
;;; (combine-lst-lst '() '()) ;-> nil                                ; caso no permitido
;;; (combine-lst-lst '() '(1 2)) ;-> nil                             ; caso base
;;; (combine-lst-lst '(1 2) '(a b)) ;-> ((1 A) (1 B) (2 A) (2 B))    ; caso tipico
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;
;;; EJEMPLOS:
;;; (combine-elt-lol 'a nil) ;-> nil                           ; caso no permitido
;;; (combine-elt-lol 'a '((1 2) (3 4))) ;-> ((A 1 2) (A 3 4))  ; caso tipico
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;
;;; EJEMPLOS:
;;; (combine-lst-lol nil '((1 2) (3 4))) ;-> nil                                    ; caso no permitido
;;; (combine-lst-lol '(1 2) nil) ;-> nil                                            ; caso no permitido
;;; (combine-lst-lol '(a b) '((1 2) (3 4))) ;-> ((A 1 2) (A 3 4) (B 1 2) (B 3 4))   ; caso tipico
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      (append (combine-lst-lol (first lstolsts)
              (combine-list-of-lsts (rest lstolsts))))))
;;;
;;; EJEMPLOS:
;;; (combine-list-of-lsts '()) ;-> (nil)                                ; caso base
;;; (combine-list-of-lsts '(() (+ -) (1 2 3 4))) ;-> nil                ; caso no permitido
;;; (combine-list-of-lsts '((a b c) () (1 2 3 4))) ;-> nil              ; caso no permitido
;;; (combine-list-of-lsts '((a b c) (+ -) ())) ;-> nil                  ; caso no permitido
;;; (combine-list-of-lsts '((a b c))) ;-> ((a) (b) (c))                 ; caso particular
;;; (combine-list-of-lsts '((a b c) (+ -) (1 2 3 4))) ;->               ; caso tipico
;;; ((A + 1) (A + 2) (A + 3) (A + 4) (A - 1) (A - 2) (A - 3) (A - 4)
;;; (B + 1) (B + 2) (B + 3) (B + 4) (B - 1) (B - 2) (B - 3) (B - 4)
;;; (C + 1) (C + 2) (C + 3) (C + 4) (C - 1) (C - 2) (C - 3) (C - 4))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
