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
;;; Genera un diccionario con la similitud coseno entre un vector
;;; y todos los vectores de una lista de vectores
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
;;; (sc-conf-gendict '(1 2) '((1 2) (1 3)))
;;; -> ((1.0 (1 2)) (0.98994946 (1 3)))
;;; (sc-conf-gendict '(2 3) '((1 1) (1 2)))
;;; -> ((0.9805807 (1 1)) (0.99227786 (1 2)))
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
  (sc-conf-selvec
   (sort
    (remove conf (sc-conf-gendict cat vs) :test #'> :key #'first)
    #'> :key #'first)))
;;;
;;; EJEMPLOS:
;;; (sc-conf '(1 2) '((1 2) (1 3) (1 4) (2 30)) 0.95) ;-> ((1 2) (1 3) (1 4))
;;; (sc-conf '(2 5) '((1 5) (4 10)) 0.9) ;-> ((4 10) (1 5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EJERCICIO 1.3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-class-gendict (x vs func)
;;; Realiza la misma funcion que sc-conf-gendict pero ahora tenemos
;;; en cuentaque los vectores tienen un identificador, y la funcion
;;; para calcular la similitud se pasa como parametro
;;;
;;; INPUT: x: vector, representado como una lista
;;; vs: vector de vectores, representado como una lista de listas
;;; func: funcion con la que calcular la similitud
;;;
;;; OUTPUT: diccionario formado por las similitudes y los
;;; identificadores de los vectores
;;;
(defun sc-class-gendict (x vs func)
  (mapcar #'(lambda (y)
              (list (funcall func (rest x)
                             (rest y))
                    (car y)))
    vs))
;;;
;;; EJEMPLOS:
;;; (sc-class-gendict '(1 1 2) '((1 1 2) (2 1 3)) #'sc-rec)
;;; -> ((1.0 1) (0.98994946 2))
;;; (sc-class-gendict '(1 2 3) '((1 1 1) (2 1 2)) #'sc-mapcar)
;;; -> ((0.9805807 1) (0.99227786 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-class-selvec (lst)
;;; Realiza la misma funcion que sc-conf-selvec pero ahora en vez
;;; de devolver el vector, devuelve su identificacion junto con la
;;; similitud en un par
;;;
;;; INPUT: lst: diccionario de similitudes-vectores
;;;
;;; OUTPUT: lista de pares con identificadores de vectores y similitudes
;;;
(defun sc-class-selvec (lst)
  (mapcar #'(lambda (y)
              (cons (second y)
                    (first y)))
    lst))
;;;
;;; EJEMPLOS:
;;; (sc-class-selvec '((1.0 1) (0.98994946 2)))
;;; -> ((1 . 1.0) (2 . 0.98994946))
;;; (sc-class-selvec '((0.9805807 1) (0.99227786 2)))
;;; -> ((1 . 0.9805807) (2 . 0.99227786))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-conf-alt (x vs func)
;;; Devuelve lista de pares identificador-similitud ordenados por
;;; similitud a uno dado
;;;
;;; INPUT: x: vector, representado como una lista
;;; vs: vector de vectores, representado como una lista de listas
;;; func: funcion para calcular similitud
;;; OUTPUT: Pares identificador-similitud ordenados
;;;
(defun sc-conf-alt (x vs func)
  (sc-class-selvec
   (sort (sc-class-gendict x vs func)
         #'> :key #'first)))
;;;
;;; EJEMPLOS:
;;; (sc-conf-alt '(2 1 2) '((1 1 2) (2 1 3) (3 1 4) (4 2 30)) #'sc-rec)
;;; -> ((1 . 1.0) (2 . 0.98994946) (3 . 0.97618705) (4 . 0.9221943))
;;; (sc-conf-alt '(3 2 5) '((1 1 5) (2 4 10)) #'sc-mapcar)
;;; -> ((2 . 1.0) (1 . 0.98328197))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-classifier (cats texts func)
;;; Clasifica a los textos en categorias.
;;;
;;; INPUT:
;;; cats: vector de vectores, representado como una lista de listas
;;; texts: vector de vectores, representado como una lista de listas
;;; func: funcion para evaluar la similitud coseno
;;; OUTPUT: Pares identificador de categori�a con resultado de similitud coseno
;;;
(defun sc-classifier (cats texts func)
  (mapcar #'(lambda (y)
              (car (sc-conf-alt y cats func)))
    texts))
;;;
;;; EJEMPLOS:
;;; (sc-classifier '((1 1 2) (2 5 10)) '((1 1 3) (2 1 4) (3 2 5)) #'sc-rec)
;;; -> ((1 . 0.98994946) (2 . 0.9761871) (1 . 0.9965458))
;;; (sc-classifier '((1 43 23 12) (2 33 54 24)) '((1 3 22 134) (2 43 26 58))
;;; #'sc-mapcar) -> ((2 . 0.48981872) (1 . 0.81555086))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EJERCICIO 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bisect (f a b tol)
;;
;; Encuentra una raiz de f entre los puntos a y b utilizando biseccion.
;;
;; Si f(a)f(b)>=0, no hay garantia de que exista una raiz en el
;; intervalo y la funcion devuelve NIL.
;;
;; INPUT:
;;
;;    f: funcion real de un solo parametro con valores reales
;;       de la que queremos encontrar la raiz.
;;    a: limite inferior del intervalo en el que queremos buscar la raiz.
;;    b: b>a limite superior del intervalo en el que queremos buscar la raiz.
;;    tol: tolerancia para el criterio de parada: si b-a < tol la funcion
;;         devuelve (a+b)/2 como solucion.
;;
;; OUTPUT:
;;
;;    raiz de la funcion, o NIL si no se ha encontrado ninguna.
;;
(defun bisect (f a b tol)
  (let ((pto-medio (/ (+ a b) 2)))
    (unless (>= (* (funcall f a) (funcall f b)) 0)
      (if (< (- b a) tol)
          pto-medio
        (if (>= (* (funcall f a) (funcall f pto-medio)) 0)
            (bisect f pto-medio b tol)
          (bisect f a pto-medio tol))))))
;;
;; EJEMPLOS:
;;
;; (bisect #'(lambda(x) (sin (* 6.26 x))) 0.0 0.7 0.001) ;-> NIL
;; (bisect #'(lambda(x) (sin (* 6.26 x))) 0.1 0.7 0.001) ;-> 0.5016602
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clean (lst)
;;
;; Funcion no destructiva que devuelve una lista vacia si todos
;; los elementos de lst son NIL.
;;
;; INPUT:
;;
;;    lst: lista que se quiere evaluar.
;;
;; OUTPUT:
;;
;;    NIL si todos los elementos de lst son NIL;
;;    lst en caso contrario.
;;
(defun clean (lst)
  (unless (every #'null lst)
    lst))
;;
;; EJEMPLOS:
;;
;; (clean '(NIL NIL NIL)) ;-> NIL
;; (clean '(NIL 1 NIL)) ;-> (NIL 1 NIL)
;; (clean '(1 2 3)) ;-> (1 2 3)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; allroot (f lst tol)
;;
;; Encuentra todas las raices situadas entre valores consecutivos
;; de una lista ordenada.
;;
;; Siempre que sgn(f(lst[i])) != sgn(f(lst[i+1])) la funcion buscara
;; una raiz en el correspondiente intervalo.
;;
;; INPUT:
;;
;;    f: funcion real de un solo parametro con valores reales
;;       de la que queremos encontrar la raiz.
;;    lst: lista ordenada de valores reales (lst[i] < lst[i+1]).
;;    tol: tolerancia para el criterio de parada: si b-a < tol la funcion
;;         devuelve (a+b)/2 como solucion.
;;
;; OUTPUT:
;;
;;    Una lista de valores reales conteniendo las raices de la
;;    funcion en los sub-intervalos dados.
;;
(defun allroot-aux (f lst tol)
  (unless (and (null (rest lst)) (not (null (first lst))))
    (cons (bisect f (first lst) (second lst) tol)
          (allroot-aux f (rest lst) tol))))

(defun allroot (f lst tol)
  (clean (allroot-aux f lst tol)))
;;
;; EJEMPLOS:
;;
;; (allroot #'(lambda(x) (sin (* 6.28 x))) '(0.1 2.25) 0.0001) ;-> NIL
;; (allroot #'(lambda(x) (sin (* 6.28 x))) '(0.25 0.75 1.25 1.75 2.25) 0.0001)
;; ;-> (0.50027466 1.0005188 1.5007629 2.001007)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; allind(f a b N tol)
;;;
;;; Divide un intervalo en cierto numero de sub-intervalos y encuentra
;;; todas las raices de la funcion f en los mismos.
;;;
;;; El intervalo [a,b] es dividido en intervalos [x[i],x[i+1]] con
;;; x[i] = a + i*dlt; en cada intervalo se busca una raiz, y todas
;;; las raices encontradas son devueltas en una lista.
;;;
;;; INPUT:
;;;
;;;    f: funcion real de un solo parametro con valores reales
;;;       de la que queremos encontrar la raiz.
;;;    a: limite inferior del intervalo en el que queremos buscar la raiz.
;;;    b: b>a limite superior del intervalo en el que queremos buscar la raiz.
;;;    N: Exponente del numero de intervalos en el que se divide [a,b]:
;;;       [a,b] se divide en 2^N intervalos
;;;    tol: tolerancia para el criterio de parada: si b-a < tol la funcion
;;;         devuelve (a+b)/2 como solucion.
;;;
;;; OUTPUT:
;;;
;;;    Lista con todas las raices encontradas.
;;;
(defun allind-aux (f x incr tol max)
  (let ((y (+ x incr)))
    (unless (> y max)
      (cons (bisect f x y tol) (allind-aux f y incr tol max)))))

(defun allind (f a b N tol)
  (clean (allind-aux f a (coerce (/ (- b a) (expt 2 N)) 'real) tol b))
;;;
;;; EJEMPLOS:
;;;
;;; (allind #'(lambda(x) (sin (* 6.28 x))) 0.1 2.25 1 0.0001) ;-> NIL
;;; (allind #'(lambda(x) (sin (* 6.28 x))) 0.1 2.25 2 0.0001)
;;; ;-> (0.50027084 1.0005027 1.5007347 2.0010324);
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EJERCICIO 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EJERCICIO 3.1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst (elt lst)
;;; Genera una lista con las combinaciones de un elemento con los
;;; elementos de una lista
;;;
;;; INPUT: elt: elemento a combinar
;;; lst: lista de elementos a combinar
;;;
;;; OUTPUT: Lista con todas las posibles combinaciones
;;;
(defun combine-elt-lst (elt lst)
  (unless (null lst)
    (cons (list elt (first lst))
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
;;; Genera una lista con las combinaciones de un elemento con los
;;; elementos de una lista de listas
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
;;; Genera el producto cartesiano de una lista con todas las listas
;;; de una lista de listas
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
;;; (combine-lst-lol nil '((1 2) (3 4))) -> nil   ; caso no permitido
;;; (combine-lst-lol '(1 2) nil) -> nil           ; caso no permitido
;;; (combine-lst-lol '(a b) '((1 2) (3 4)))
;;; -> ((A 1 2) (A 3 4) (B 1 2) (B 3 4))          ; caso tipico
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lsts (lstolsts)
;;; Genera una lista con todas las disposiciones de elementos
;;; pertenecientes a listas contenidas en una lista de listas
;;;
;;; INPUT:
;;; lstolsts: lista de listas de la que sacar todas las combinaciones
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
;;; (combine-list-of-lsts '()) -> (nil)                                ; caso base
;;; (combine-list-of-lsts '(() (+ -) (1 2 3 4))) -> nil                ; caso no permitido
;;; (combine-list-of-lsts '((a b c) () (1 2 3 4))) -> nil              ; caso no permitido
;;; (combine-list-of-lsts '((a b c) (+ -) ())) -> nil                  ; caso no permitido
;;; (combine-list-of-lsts '((a b c))) -> ((a) (b) (c))                 ; caso particular
;;; (combine-list-of-lsts '((a b c) (+ -) (1 2 3 4))) ->               ; caso tipico
;;; ((A + 1) (A + 2) (A + 3) (A + 4) (A - 1) (A - 2) (A - 3) (A - 4)
;;; (B + 1) (B + 2) (B + 3) (B + 4) (B - 1) (B - 2) (B - 3) (B - 4)
;;; (C + 1) (C + 2) (C + 3) (C + 4) (C - 1) (C - 2) (C - 3) (C - 4))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EJERCICIO 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definicion de simbolos que representan valores de verdad,
;; conectores y predicados para evaluar si una expresion LISP
;; es un valor de verdad o un conector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +bicond+ '<=>)
(defconstant +cond+   '=>)
(defconstant +and+    '^)
(defconstant +or+     'v)
(defconstant +not+    '¬)

(defun truth-value-p (x)
  (or (eql x T) (eql x NIL)))

(defun unary-connector-p (x)
  (eql x +not+))

(defun binary-connector-p (x)
  (or (eql x +bicond+)
      (eql x +cond+)))

(defun n-ary-connector-p (x)
  (or (eql x +and+)
      (eql x +or+)))

(defun connector-p (x)
  (or (unary-connector-p  x)
      (binary-connector-p x)
      (n-ary-connector-p   x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.1
;; positive-literal-p
;;
;; Predicado para determinar si una expresion en LISP
;; es un literal positivo
;;
;; RECIBE   : expresion
;; EVALUA A : T si la expresion es un literal positivo,
;;            NIL en caso contrario.
;;
(defun positive-literal-p (x)
  (not (or (truth-value-p x)
           (connector-p x)
           (listp x))))
;;
;; EJEMPLOS:
;; (positive-literal-p 'p)
;; evalua a T
;;
;; (positive-literal-p T)
;; (positive-literal-p NIL)
;; (positive-literal-p '~)
;; (positive-literal-p '=>)
;; (positive-literal-p '(p))
;; (positive-literal-p '(~ p))
;; (positive-literal-p '(~ (v p q)))
;;; evaluan a NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.2
;; negative-literal-p
;;
;; Predicado para determinar si una expresion
;; es un literal negativo
;;
;; RECIBE   : expresion x
;; EVALUA A : T si la expresion es un literal negativo,
;;            NIL en caso contrario.
;;
(defun negative-literal-p (x)
  (unless (or (atom x)
              (not (and (unary-connector-p (first x))
                        (positive-literal-p (second x))
                        (null (third x)))))
    t))
;;
;; EJEMPLOS:
;; (negative-literal-p '(~ p))        ; T
;; (negative-literal-p NIL)           ; NIL
;; (negative-literal-p '~)            ; NIL
;; (negative-literal-p '=>)           ; NIL
;; (negative-literal-p '(p))          ; NIL
;; (negative-literal-p '((~ p)))      ; NIL
;; (negative-literal-p '(~ T))        ; NIL
;; (negative-literal-p '(~ NIL))      ; NIL
;; (negative-literal-p '(~ =>))       ; NIL
;; (negative-literal-p 'p)            ; NIL
;; (negative-literal-p '((~ p)))      ; NIL
;; (negative-literal-p '(~ (v p q)))  ; NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.3
;; literal-p
;;
;; Predicado para determinar si una expresion es un literal
;;
;; RECIBE   : expresion x
;; EVALUA A : T si la expresion es un literal,
;;            NIL en caso contrario.
;;
(defun literal-p (x)
  (or (positive-literal-p x)
      (negative-literal-p x)))
;;
;; EJEMPLOS:
;; (literal-p 'p)
;; (literal-p '(~ p))
;; ;-> evaluan a T
;; (literal-p '(p))
;; (literal-p '(~ (v p q)))
;; ->; evaluan a  NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wff-prefix-p
;;
;; Predicado para determinar si una expresion esta en formato prefijo
;;
;; RECIBE   : expresion x
;; EVALUA A : T si x esta en formato prefijo, NIL en caso contrario.
;;
(defun wff-prefix-p (x)
  (unless (null x)             ;; NIL no es FBF en formato prefijo (por convencion)
    (or (literal-p x)          ;; Un literal es FBF en formato prefijo
        (and (listp x)         ;; En caso de que no sea un literal debe ser una lista
             (let ((connector (first x))
                   (rest_1    (rest  x)))
               (cond
                ((unary-connector-p connector)  ;; Si el primer elemento es un connector unario
                 (and (null (rest rest_1))      ;; deberia tener la estructura (<conector> FBF)
                      (wff-prefix-p (first rest_1))))
                ((binary-connector-p connector) ;; Si el primer elemento es un conector binario
                 (let ((rest_2 (rest rest_1)))  ;; deberia tener la estructura
                   (and (null (rest rest_2))    ;; (<conector> FBF1 FBF2)
                        (wff-prefix-p (first rest_1))
                        (wff-prefix-p (first rest_2)))))
                ((n-ary-connector-p connector)  ;; Si el primer elemento es un conector enario
                 (or (null rest_1)              ;; conjuncion o disyuncion vacias
                     (and (wff-prefix-p (first rest_1)) ;; tienen que ser FBF los operandos
                          (let ((rest_2 (rest rest_1)))
                            (or (null rest_2)           ;; conjuncion o disyuncion con un elemento
                                (wff-prefix-p (cons connector rest_2)))))))
                (t NIL)))))))                   ;; No es FBF en formato prefijo
;;
;; EJEMPLOS:
;; (wff-prefix-p '(v))
;; (wff-prefix-p '(^))
;; (wff-prefix-p '(v A))
;; (wff-prefix-p '(^ (~ B)))
;; (wff-prefix-p '(v A (~ B)))
;; (wff-prefix-p '(v (~ B) A ))
;; (wff-prefix-p '(^ (V P (=> A (^ B (~ C) D))) (^ (<=> P (~ Q)) P) E))
;; evaluan a T
;; (wff-prefix-p 'NIL)
;; (wff-prefix-p '(~))
;; (wff-prefix-p '(=>))
;; (wff-prefix-p '(<=>))
;; (wff-prefix-p '(^ (V P (=> A ( B ^ (~ C) ^ D))) (^ (<=> P (~ Q)) P) E))
;; evaluan a NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.4
;; wff-infix-p
;;
;; Predicado para determinar si una expresion esta en formato infijo
;;
;; RECIBE   : expresion x
;; EVALUA A : T si x esta en formato infijo,
;;            NIL en caso contrario.
;;
(defun wff-infix-p (x)
  (unless (null x)
    (or (literal-p x)
        (and (listp x)
             (let ((op1 (first x))
                   (op2 (first (rest x)))
                   (op3 (rest (rest x))))
               (cond
                ((unary-connector-p op1)
                 (and (null op3)
                      (wff-infix-p op2)))
                ((binary-connector-p op2)
                 (and (null (rest op3))
                      (wff-infix-p (first x))
                      (wff-infix-p (first op3))))
                ((n-ary-connector-p op2)
                 (let ((tmp (first (rest op3))))
                   (when (or (eql op2 tmp)
                             (null tmp))
                     (and (wff-infix-p op1)
                          (wff-infix-p (first op3))))))
                ((n-ary-connector-p op1)
                 (null (rest x)))
                (t NIL)))))))
;;
;; EJEMPLOS:
;;
;; (wff-infix-p 'a) 					; T
;; (wff-infix-p '(^)) 					; T
;; (wff-infix-p '(v)) 					; T
;; (wff-infix-p '(A ^ (v))) 			        ; T
;; (wff-infix-p '( a ^ b ^ (p v q) ^ (~ r) ^ s))  	; T
;; (wff-infix-p '(A => B)) 				; T
;; (wff-infix-p '(A => (B <=> C))) 			; T
;; (wff-infix-p '( B => (A ^ C ^ D))) 			; T
;; (wff-infix-p '( B => (A ^ C))) 			; T
;; (wff-infix-p '( B ^ (A ^ C))) 			; T
;; (wff-infix-p '((p v (a => (b ^ (~ c) ^ d))) ^
;; ((p <=> (~ q)) ^ p ) ^ e))                           ; T
;; (wff-infix-p nil) 					; NIL
;; (wff-infix-p '(a ^)) 				; NIL
;; (wff-infix-p '(^ a)) 				; NIL
;; (wff-infix-p '(a)) 					; NIL
;; (wff-infix-p '((a))) 				; NIL
;; (wff-infix-p '((a) b))   			        ; NIL
;; (wff-infix-p '(^ a b q (~ r) s))  		        ; NIL
;; (wff-infix-p '( B => A C)) 			        ; NIL
;; (wff-infix-p '( => A)) 				; NIL
;; (wff-infix-p '(A =>)) 				; NIL
;; (wff-infix-p '(A => B <=> C)) 		        ; NIL
;; (wff-infix-p '( B => (A ^ C v D))) 		        ; NIL
;; (wff-infix-p '( B ^ C v D )) 			; NIL
;; (wff-infix-p '((p v (a => e (b ^ (~ c) ^ d))) ^
;; ((p <=> (~ q)) ^ p ) ^ e))                           ; NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prefix-to-infix
;; Convierte FBF en formato prefijo a FBF en formato infijo
;;
;; RECIBE   : FBF en formato prefijo
;; EVALUA A : FBF en formato infijo
;;
(defun prefix-to-infix (wff)
  (when (wff-prefix-p wff)
    (if (literal-p wff)
        wff
      (let ((connector      (first wff))
            (elements-wff (rest wff)))
        (cond
         ((unary-connector-p connector)
          (list connector (prefix-to-infix (second wff))))
         ((binary-connector-p connector)
          (list (prefix-to-infix (second wff))
                connector
                (prefix-to-infix (third wff))))
         ((n-ary-connector-p connector)
          (cond
           ;;; conjuncion o disyuncion vacias.
           ((null elements-wff)
            ;;; por convencion, se acepta como fbf en formato infijo
            wff)
           ;;; conjuncion o disyuncion con un unico elemento
           ((null (cdr elements-wff))
            (prefix-to-infix (car elements-wff)))
           (t (cons (prefix-to-infix (first elements-wff))
                    (mapcan #'(lambda(x)
                                (list connector
                                      (prefix-to-infix x)))
                      (rest elements-wff))))))
         (t NIL)))))) ;; no deberia llegar a este paso nunca
;;
;;  EJEMPLOS:
;;
;; (prefix-to-infix '(v))          ; (V)
;; (prefix-to-infix '(^))          ; (^)
;; (prefix-to-infix '(v a))        ; A
;; (prefix-to-infix '(^ a))        ; A
;; (prefix-to-infix '(^ (~ a)))    ; (~ a)
;; (prefix-to-infix '(v a b))      ; (A v B)
;; (prefix-to-infix '(v a b c))    ; (A V B V C)
;; (prefix-to-infix '(^ (V P (=> A (^ B (~ C) D)))
;; (^ (<=> P (~ Q)) P) E)) -> ((P V (A => (B ^ (~ C) ^ D))) ^
;; ((P <=> (~ Q)) ^ P) ^ E)
;; (prefix-to-infix '(^ (v p (=> a (^ b (~ c) d)))))
;; -> (P V (A => (B ^ (~ C) ^ D)))
;; (prefix-to-infix '(^ (^ (<=> p (~ q)) p ) e))
;; -> (((P <=> (~ Q)) ^ P) ^ E)
;; (prefix-to-infix '( v (~ p) q (~ r) (~ s)))
;; -> ((~ P) V Q V (~ R) V (~ S))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.5
;; infix-to-prefix
;;
;; Convierte FBF en formato infijo a FBF en formato prefijo
;;
;; RECIBE   : FBF en formato infijo
;; EVALUA A : FBF en formato prefijo
;;
(defun infix-to-prefix-eliminate-connectors-aux (op)
  (unless (null op)
    (cons (first op) (infix-to-prefix-eliminate-connectors-aux
                      (rest (rest op))))))

(defun infix-to-prefix (wff)
  (when (wff-infix-p wff)
    (if (literal-p wff)
        wff
      (let ((op1 (first wff))
            (op2 (first (rest wff)))
            (op3 (rest (rest wff)))
            (funcion-aux (infix-to-prefix-eliminate-connectors-aux)))
        (cond
         ((unary-connector-p op1)
          (list op1
                (infix-to-prefix op2)))
         ((binary-connector-p op2)
          (list op2
                (infix-to-prefix op1)
                (infix-to-prefix (first op3))))
         ((n-ary-connector-p op2)
          (if (null op3)
              (infix-to-prefix op1)
            (cons op2
                  (cons (infix-to-prefix op1)
                        (mapcar #'infix-to-prefix
                          (funcion-aux op3))))))
         (t NIL))))))
;;
;; EJEMPLOS
;;
;; (infix-to-prefix nil)      -> NIL
;; (infix-to-prefix 'a)       -> a
;; (infix-to-prefix '((a)))   -> NIL
;; (infix-to-prefix '(a))     -> NIL
;; (infix-to-prefix '(((a)))) -> NIL
;; (prefix-to-infix (infix-to-prefix '((p v (a => (b ^ (~ c) ^ d)))
:: ^ ((p <=> (~ q)) ^ p) ^ e)))
;; -> ((P V (A => (B ^ (~ C) ^ D))) ^ ((P <=> (~ Q)) ^ P) ^ E)
;; (infix-to-prefix '((p v (a => (b ^ (~ c) ^ d))) ^
;; ((p <=> (~ q)) ^ p) ^ e))
;; -> (^ (V P (=> A (^ B (~ C) D))) (^ (<=> P (~ Q)) P) E)
;; (infix-to-prefix '(~ ((~ p) v q v (~ r) v (~ s))))
;; -> (~ (V (~ P) Q (~ R) (~ S)))
;; (infix-to-prefix (prefix-to-infix'(V (~ P) Q (~ R) (~ S))))
;; -> (V (~ P) Q (~ R) (~ S))
;; (infix-to-prefix (prefix-to-infix'(~ (V (~ P) Q (~ R) (~ S)))))
;; -> (~ (V (~ P) Q (~ R) (~ S)))
;; (infix-to-prefix 'a)  ; A
;; (infix-to-prefix '((p v (a => (b ^ (~ c) ^ d))) ^
;; ((p <=> (~ q)) ^ p) ^ e))
;; -> (^ (V P (=> A (^ B (~ C) D))) (^ (<=> P (~ Q)) P) E)
;; (infix-to-prefix '(~ ((~ p) v q v (~ r) v (~ s))))
;; -> (~ (V (~ P) Q (~ R) (~ S)))
;; (infix-to-prefix (prefix-to-infix '(^ (^ (<=> p (~ q)) p ) e)))
;; -> '(^ (^ (<=> p (~ q)) p ) e))
;; (infix-to-prefix (prefix-to-infix '( v (~ p) q (~ r) (~ s))))
;; -> '( v (~ p) q (~ r) (~ s)))
;; (infix-to-prefix '(p v (a => (b ^ (~ c) ^ d))))
;; -> (V P (=> A (^ B (~ C) D)))
;; (infix-to-prefix '(((P <=> (~ Q)) ^ P) ^ E))
;; -> (^ (^ (<=> P (~ Q)) P) E)
;; (infix-to-prefix '((~ P) V Q V (~ R) V (~ S)))
;; -> (V (~ P) Q (~ R) (~ S))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.6
;; clause-p
;;
;; Predicado para determinar si una FBF es una clausula
;;
;; RECIBE   : FBF en formato prefijo
;; EVALUA A : T si FBF es una clausula, NIL en caso contrario.
;;
(defun clause-p (wff)
  (and (listp wff)
       (eql (first wff) +or+)
       (every #'identity
              (mapcar #'(lambda (lit)
                          (literal-p lit))
                (rest wff)))))
;;
;; EJEMPLOS:
;;
;; (clause-p '(v))             ; T
;; (clause-p '(v p))           ; T
;; (clause-p '(v (~ r)))       ; T
;; (clause-p '(v p q (~ r) s)) ; T
;; (clause-p NIL)                    ; NIL
;; (clause-p 'p)                     ; NIL
;; (clause-p '(~ p))                 ; NIL
;; (clause-p NIL)                    ; NIL
;; (clause-p '(p))                   ; NIL
;; (clause-p '((~ p)))               ; NIL
;; (clause-p '(^ a b q (~ r) s))     ; NIL
;; (clause-p '(v (^ a b) q (~ r) s)) ; NIL
;; (clause-p '(~ (v p q)))           ; NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.7
;; cnf-p
;;
;; Predicado para determinar si una FBF esta en FNC
;;
;; RECIBE   : FFB en formato prefijo
;; EVALUA A : T si FBF esta en FNC con conectores,
;;            NIL en caso contrario.
;;
(defun cnf-p (wff)
  (unless (literal-p wff)
    (and (equal (first wff) +and+)
    	 (every #'clause-p (rest wff)))))
;;
;; EJEMPLOS:
;;
;; (cnf-p '(^ (v a  b c) (v q r) (v (~ r) s) (v a b))) ; T
;; (cnf-p '(^ (v a  b (~ c)) ))                        ; T
;; (cnf-p '(^ ))                                       ; T
;; (cnf-p '(^(v )))                                    ; T
;; (cnf-p '(~ p))                                      ; NIL
;; (cnf-p '(^ a b q (~ r) s))                          ; NIL
;; (cnf-p '(^ (v a b) q (v (~ r) s) a b))              ; NIL
;; (cnf-p '(v p q (~ r) s))                            ; NIL
;; (cnf-p '(^ (v a b) q (v (~ r) s) a b))              ; NIL
;; (cnf-p '(^ p))                                      ; NIL
;; (cnf-p '(v ))                                       ; NIL
;; (cnf-p NIL)                                         ; NIL
;; (cnf-p '((~ p)))                                    ; NIL
;; (cnf-p '(p))                                        ; NIL
;; (cnf-p '(^ (p)))                                    ; NIL
;; (cnf-p '((p)))                                      ; NIL
;; (cnf-p '(^ a b q (r) s))                            ; NIL
;; (cnf-p '(^ (v a  (v b c)) (v q r) (v (~ r) s) a b)) ; NIL
;; (cnf-p '(^ (v a (^ b c)) (^ q r) (v (~ r) s) a b))  ; NIL
;; (cnf-p '(~ (v p q)))                                ; NIL
;; (cnf-p '(v p q (r) s))                              ; NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.1: Incluya comentarios en el codigo adjunto
;; eliminate-biconditional
;;
;; Dada una FBF, evalua a una FBF equivalente
;; que no contiene el connector <=>
;;
;; RECIBE   : FBF en formato prefijo
;; EVALUA A : FBF equivalente en formato prefijo
;;            sin connector <=>
;;
(defun eliminate-biconditional (wff)
  (if (or (null wff) (literal-p wff))  ;; caso base de la recursion
      wff
    (let ((connector (first wff)))
      (if (eq connector +bicond+)  ;; si el conector es <=>
          (let ((wff1 (eliminate-biconditional (second wff)))    ;; eliminamos los bicondicionales que pueda haber
                (wff2 (eliminate-biconditional (third  wff))))   ;; en las FBFs implicadas
            (list +and+                                          ;; (<=> A B) equivale a
                  (list +cond+ wff1 wff2)                        ;; (^ (=> A B) (=> B A)) en notacion prefijo
                  (list +cond+ wff2 wff1)))
        (cons connector                                          ;; si el conector no es <=>
              (mapcar #'eliminate-biconditional (rest wff))))))) ;; repetimos el proceso con el resto
;;
;; EJEMPLOS:
;;
;; (eliminate-biconditional '(<=> p  (v q s p) ))
;; -> (^ (=> P (v Q S P)) (=> (v Q S P) P))
;; (eliminate-biconditional '(<=>  (<=> p  q) (^ s (~ q))))
;; -> (^ (=> (^ (=> P Q) (=> Q P)) (^ S (~ Q)))
;;      (=> (^ S (~ Q)) (^ (=> P Q) (=> Q P))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.2
;; eliminate-conditional
;;
;; Dada una FBF, que contiene conectores => evalua a
;; una FBF equivalente que no contiene el connector =>
;;
;; RECIBE   : wff en formato prefijo sin el connector <=>
;; EVALUA A : wff equivalente en formato prefijo
;;            sin el connector =>
;;
(defun eliminate-conditional (wff)
  (if (or (null wff) (literal-p wff))
      wff
    (let ((connector (first wff)))
      (if (eq connector +cond+)
          (let ((wff1 (eliminate-conditional (second wff)))
                (wff2 (eliminate-conditional (third  wff))))
            (list +or+
                  (list +not+ wff1)
                  wff2))
        (cons connector
              (mapcar #'eliminate-conditional (rest wff)))))))
;;
;; EJEMPLOS:
;;
;; (eliminate-conditional '(=> p q)) -> (V (~ P) Q)
;; (eliminate-conditional '(=> p (v q s p))) -> (V (~ P) (V Q S P))
;; (eliminate-conditional '(=> (=> (~ p) q) (^ s (~ q))))
;; -> (V (~ (V (~ (~ P)) Q)) (^ S (~ Q)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exchange-and-or
;;
;; Intercambia conectores OR por conectores AND (los demás
;; los mantiene igual)
;;
;; RECIBE   : conector
;; EVALUA A : +and+ si recibe +or+, +or+ si recibe +and+, el
;;            mismo valor si se recibe otro
;;
(defun exchange-and-or (connector)
  (cond
   ((eq connector +and+) +or+)
   ((eq connector +or+) +and+)
   (t connector)))
;;
;; EJEMPLOS:
;; (exchange-and-or +or+) -> ^
;; (exchange-and-or +and+) -> v
;; (exchange-and-or +cond+) -> =>
;; (exchange-and-or 'a) -> A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.3
;; reduce-scope-of-negation
;;
;; Dada una FBF, que no contiene los conectores <=>, =>
;; evalua a una FNF equivalente en la que la negacion
;; aparece unicamente en literales negativos
;;
;; RECIBE   : FBF en formato prefijo sin conector <=>, =>
;; EVALUA A : FBF equivalente en formato prefijo en la que
;;            la negacion  aparece unicamente en literales
;;            negativos.
;;
(defun reduce-scope-of-negation (wff)
  (unless (null wff)
    (if (literal-p wff)
        wff
      (if (equal +not+ (first wff))
          (if (negative-literal-p (second wff))
              (second (second wff)) ;Caso doble negacion
            (let ((resto (rest wff)))
              (append (mapcar #'(lambda(x) (if (n-ary-connector-p x)
                                               (exchange-and-or x)
                                             (reduce-scope-of-negation
                                              (list +not+ x))))
                        (first resto))
                      (reduce-scope-of-negation (rest resto)))))
        (cons (first wff)
              (mapcar #'reduce-scope-of-negation (rest wff)))))))
;;
;;  EJEMPLOS:
;;
;; (reduce-scope-of-negation '(~ (v p (~ q) r)))
;; -> (^ (~ P) Q (~ R))
;; (reduce-scope-of-negation '(~ (^ p (~ q) (v  r s (~ a)))))
;; -> (V (~ P) Q (^ (~ R) (~ S) A))
;; (reduce-scope-of-negation '(^ (v P (v (~ a) (^ b (~ c) d)))
;; (^ (^ (v (~ p) (~ q)) (v (~ (~ q)) p)) p) e))
;; -> (^ (V P (V (~ A) (^ B (~ C) D))) (^ (^ (V (~ P) (~ Q)) (V Q P)) P) E)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.4: Comente el codigo adjunto
;; cnf
;;
;; Dada una FBF, que no contiene los conectores <=>, => en la
;; que la negacion aparece unicamente en literales negativos
;; evalua a una FNC equivalente en FNC con conectores ^, v
;;
;; RECIBE   : FBF en formato prefijo sin conector <=>, =>,
;;            en la que la negacion aparece unicamente
;;            en literales negativos
;; EVALUA A : FBF equivalente en formato prefijo FNC
;;            con conectores ^, v
;;
(defun combine-elt-lst (elt lst)
  (if (null lst)                                ;; si lst=NIL, se devuelve ((elt))
      (list (list elt))                         ;; en caso contrario, se devuelve lo mismo que en el ejercicio 3.1,
    (mapcar #'(lambda (x) (cons elt x)) lst)))  ;; el producto cartesiano entre {elt} y {lst}.

(defun exchange-NF-aux (nf)
  (if (null nf)                                         ;; caso base: si nf=NIL, se devuelve NIL
      NIL                                               ;; caso general: se toma el primer elemento de la lista
    (let ((lst (first nf)))                             ;; que se pasa por la entrada (llamado lst aqui), si es un
      (mapcan #'(lambda (x)                             ;; literal se combina recursivamente con el resto de nf, y si
                  (combine-elt-lst                      ;; no lo es, se ignora el conector (que sera el primer elemento)
                   x                                    ;; y se realiza la operacion para cada elemento de la lista restante.
                   (exchange-NF-aux (rest nf))))        ;; se asume que el primer elemento de nf no es un conector, sino que
        (if (literal-p lst) (list lst) (rest lst))))))  ;; es un literal y/o una lista
;; EJEMPLO:
;; (exchange-NF-aux '(p q (^ r m) (^ n a) s)) -> ((P Q R N S) (P Q R A S) (P Q M N S) (P Q M A S))
;; Se crea una lista en la que aparecen listas con todos los literales del argumento y que contienen las diferentes
;; combinaciones de los literales que estan dentro de listas con operadores contenidas tambien en el argumento.
;; En este ejemplo, las operaciones que se realizan son:
;; 1- combinación entre s y NIL que da ((S))
;; 2- combinación entre los literales de (^ n a) y ((S)), de lo que surgen ((N S)) y ((A S)) dando lugar a ((N S) (A S))
;; 3- combinación entre los literales de (^ r m) y ((N S) (A S)), dando lugar a ((R N S) (R A S) (M N S) (M A S))
;; 4- combinación entre q y la salida anterior, y posteriormente entre p y la salida anterior, dando lugar a la salida final

(defun exchange-NF (nf)
  (if (or (null nf) (literal-p nf))                 ;; caso base: si se introduce NIL o un literal, se devuelve lo mismo
      nf                                            ;; caso general: se asume que nf es la FBF en formato prefijo, por lo que
    (let ((connector (first nf)))                   ;; su primer elemento es un conector que será ^ o v, y al estar bien formada
      (cons (exchange-and-or connector)             ;; en su interior tendra operaciones con el conector opuesto. Por tanto, se
            (mapcar #'(lambda (x)                   ;; sustituye la conjuncion por la disyuncion o viceversa para formar una nueva
                          (cons connector x))       ;; formula, y los argumentos de cualquiera de las dos acciones seran los literales
                (exchange-NF-aux (rest nf)))))))    ;; que surgen de exchange-NF-aux operados a traves de la conjuncion o disyuncion inicial
;; EJEMPLO:
;; (exchange-NF '(v p  q  (^ r  m)  (^ n  a)  s )) -> (^ (V P Q R N S) (V P Q R A S) (V P Q M N S) (V P Q M A S))
;; Como se puede ver, se ha tomado el conector inicial (una disyuncion) y se ha formado una formula con la operacion
;; opuesta (una conjuncion) cuyos argumentos son las listas que surgieron de exchange-NF-aux y en cada una aplicado
;; el operador inicial (el conector de disyuncion). De esta forma se consigue una formula equivalente dado que, si en la
;; formula inicial ocurrian p, q o s, la expresion era verdadera, y en la final pasa lo mismo (porque p, q y s estan en
;; todas las disyunciones); tambien, si r y m (o n y a) ocurrian a la vez, la expresion inicial era verdadera, y en la
;; expresion final, como r aparece en la mitad de las disyunciones y m en la otra mitad (mismo caso para n y a), si las dos
;; son verdad seran verdades todas las disyunciones, mientras que si alguna de las dos no es verdad, no todas las disyunciones
;; tienen por que serlo.

(defun simplify (connector lst-wffs )
  (if (literal-p lst-wffs)                                          ;; caso base: si la lista de FBF es en realidad un literal, se devuelve el mismo
      lst-wffs                                                      ;; caso general: se concatenan todas las FBF con el mapcan, y cuando en ellas
    (mapcan #'(lambda (x)                                           ;; aparece el mismo conector que se pasa por argumento, se acceden de manera
                (cond                                               ;; recursiva para que en el caso en el que haya expresiones redundantes como
                 ((literal-p x) (list x))                           ;; (v a (v b c)), se reduzcan a (v a b c)
                 ((equal connector (first x))
                  (mapcan                                           ;; este mapcan se ejecuta si coinciden conector argumento y conector de la FBF
                      #'(lambda (y) (simplify connector (list y)))  ;; y ejecuta simplify de forma recursiva para ese mismo conector y para los elementos
                    (rest x)))                                      ;; de la conjuncion, que cuando sean literales se devolveran tal cual y cuando sean
                 (t (list x))))                                     ;; operaciones se procesaran de la misma forma
      lst-wffs)))
;; EJEMPLOS:
;; (simplify 'v '((v p q (v a b)) (v a c (^ f r)))) -> (P Q A B A C (^ F R))
;; (simplify '^ '((v p q (v a b)) (v a c (^ f r)))) -> ((V P Q (V A B)) (V A C (^ F R)))
;; (simplify '^ '((v p q (v a b)) (^ a c (^ f r)))) -> ((V P Q (V A B)) A C F R)
;; En los ejemplos anteriores se puede ver que cuando coinciden el conector argumento con el conector de las FBF, si dentro de las FBF
;; hay mas operaciones con el mismo conector, se simplifican y se concatenan los literales tal cual, mientras que cuando los conectores
;; no coinciden se concatenan tal y como estan (esto tambien ocurre cuando en la recursion en algun momento aparecen conectores diferentes)

(defun cnf (wff)
  (cond
   ((cnf-p wff) wff)                                                        ;; Si la FBF ya esta en FNC, se devuelve tal cual.
   ((literal-p wff)                                                         ;; Si la FBF es un literal,
    (list +and+ (list +or+ wff)))                                           ;; se construye la FNC '(^ (v lit)).
   ((let ((connector (first wff)))                                          ;; En caso contrario, si el primer conector es una disyuncion,
      (cond                                                                 ;; se pasa por exchange-NF para que el primer conector sea la
       ((equal +and+ connector)                                             ;; conjuncion y previamente se simplifica la FBF con la disyuncion.
        (cons +and+ (simplify +and+ (mapcar #'cnf (rest wff)))))            ;; Si el primer conector es una conjuncion, se construye recursivamente
       ((equal +or+ connector)                                              ;; la FNC y se van simplificando las FBF intermedias con la conjuncion.
        (cnf (exchange-NF (cons +or+ (simplify +or+ (rest wff)))))))))))    ;; Tras todo este proceso se obtiene la FNC deseada.
;; EJEMPLOS:
;; (cnf 'a) -> (^ (V A))
;; (cnf NIL) -> NIL
;; (cnf '(~ a)) -> (^ (V (~ A)))
;; (cnf '(v (~ a) b c)) -> (^ (V (~ A) B C))
;; (cnf '(^ (v (~ a) b c) (~ e) (^ e f (~ g) h) (v m n) (^ r s q) (v u q) (^ x y))) ->
;; (^ (V (~ A) B C) (V (~ E)) (V E) (V F) (V (~ G)) (V H) (V M N) (V R) (V S) (V Q) (V U Q) (V X) (V Y))
;; (cnf '(^ (v p  (~ q)) a (v k  r  (^ m  n)))) ->
;; (^ (V P (~ Q)) (V A) (V K R M) (V K R N))
;; (cnf '(v p  q  (^ r  m)  (^ n  a)  s )) ->
;; (^ (V P Q R N S) (V P Q R A S) (V P Q M N S) (V P Q M A S))
;; (cnf '(^ (v a b (^ y r s) (v k l)) c (~ d) (^ e f (v h i) (^ o p)))) ->
;; (^ (V A B Y K L) (V A B R K L) (V A B S K L) (V C) (V (~ D)) (V E) (V F) (V H I) (V O) (V P))
;; (cnf '(^ (v a b (^ y r s)) c (~ d) (^ e f (v h i) (^ o p)))) ->
;; (^ (V A B Y) (V A B R) (V A B S) (V C) (V (~ D)) (V E) (V F) (V H I) (V O) (V P))
;; (cnf '(^ (^ y r s (^ p q (v c d))) (v a b))) ->
;; (^ (V Y) (V R) (V S) (V P) (V Q) (V C D) (V A B))
;; (cnf '(^ (v (~ a) b c) (~ e) r s (v e f (~ g) h) k (v m n) d)) ->
;; (^ (V (~ A) B C) (V (~ E)) (V R) (V S) (V E F (~ G) H) (V K) (V M N) (V D))
;; (cnf '(^ (v p (~ q)) (v k r (^ m  n)))) ->
;; (^ (V P (~ Q)) (V K R M) (V K R N))
;; (cnf '(v (v p q) e f (^ r  m) n (^ a (~ b) c) (^ d s))) ->
;; (^ (V P Q E F R N A D) (V P Q E F R N A S) (V P Q E F R N (~ B) D)
;; (V P Q E F R N (~ B) S) (V P Q E F R N C D) (V P Q E F R N C S)
;; (V P Q E F M N A D) (V P Q E F M N A S) (V P Q E F M N (~ B) D)
;; (V P Q E F M N (~ B) S) (V P Q E F M N C D) (V P Q E F M N C S))
;; (cnf '(^ (^ (~ y) (v r (^ s (~ x)) (^ (~ p) m (v c d))) (v (~ a) (~ b))) g)) ->
;; (^ (V (~ Y)) (V R S (~ P)) (V R S M) (V R S C D) (V R (~ X) (~ P)) (V R (~ X) M) (V R (~ X) C D) (V (~ A) (~ B)) (V G))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.5:
;; eliminate-connectors
;;
;; Dada una FBF en  FNC
;; evalua a lista de listas sin conectores
;; que representa una conjuncion de disyunciones de literales
;;
;; RECIBE   : FBF en FNC con conectores ^, v
;; EVALUA A : FBF en FNC (con conectores ^, v eliminados)
;;
(defun eliminate-connectors (cnf)
  (unless (null cnf)
    (cond
     ((literal-p cnf) cnf)
     ((equal (first cnf) +and+) (eliminate-connectors (rest cnf)))
     (t (cons (mapcar #'eliminate-connectors (rest (first cnf)))
              (eliminate-connectors (rest cnf)))))))
;;
;; EJEMPLOS:
;;
;; (eliminate-connectors 'nil) -> NIL
;; (eliminate-connectors (cnf '(^ (v p  (~ q))  (v k  r  (^ m  n)))))
;; -> ((P (~ Q)) (K R M) (K R N))
;; (eliminate-connectors (cnf '(^ (v (~ a) b c) (~ e)
;; (^ e f (~ g) h) (v m n) (^ r s q) (v u q) (^ x y))))
;; -> (((~ A) B C) ((~ E)) (E) (F) ((~ G)) (H) (M N) (R) (S)
:: (Q) (U Q) (X) (Y))
;; (eliminate-connectors (cnf '(v p  q  (^ r  m)  (^ n  q)  s )))
;; -> ((P Q R N S) (P Q R Q S) (P Q M N S) (P Q M Q S))
;; (eliminate-connectors (print (cnf '(^ (v p  (~ q)) (~ a)
;; (v k  r  (^ m  n)))))) -> ((P (~ Q)) ((~ A)) (K R M) (K R N))
;; (eliminate-connectors '(^)) -> NIL
;; (eliminate-connectors '(^ (v p (~ q)) (v) (v k r)))
;; -> ((P (~ Q)) NIL (K R))
;; (eliminate-connectors '(^ (v a b))) -> ((A B))
;; (eliminate-connectors '(^ (v p (~ q)) (v k r))) -> ((P (~ Q)) (K R))
;; (eliminate-connectors '(^ (v p (~ q)) (v q (~ a)) (v s e f) (v b)))
;; -> ((P (~ Q)) (Q (~ A)) (S E F) (B))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.6
;; wff-infix-to-cnf
;;
;; Dada una FBF en formato infijo
;; evalua a lista de listas sin conectores
;; que representa la FNC equivalente
;;
;; RECIBE   : FBF
;; EVALUA A : FBF en FNC (con conectores ^, v eliminados)
;;
(defun wff-infix-to-cnf (wff)
  (eliminate-connectors
   (cnf
    (reduce-scope-of-negation
     (eliminate-conditional
      (eliminate-biconditional
       (infix-to-prefix wff)))))))
;;
;; EJEMPLOS:
;;
;; (wff-infix-to-cnf 'a) -> ((A))
;; (wff-infix-to-cnf '(~ a)) -> (((~ A)))
;; (wff-infix-to-cnf  '( (~ p) v q v (~ r) v (~ s)))
;; -> (((~ P) Q (~ R) (~ S)))
;; (wff-infix-to-cnf  '((p v (a => (b ^ (~ c) ^ d))) ^
;; ((p <=> (~ q)) ^ p) ^ e)) ->
;; ((P (~ A) B) (P (~ A) (~ C)) (P (~ A) D) ((~ P) (~ Q)) (Q P) (P) (E))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test-contenido
;;
;; Comprueba si un literal esta contenido en una expresion
;;
;; RECIBE   : x (literal)
;;            y (expresion)
;; EVALUA A : T si x esta contenido en y
;;            NIL en caso contrario
;;
(defun test-contenido (x y)
  (some #'(lambda(z) (equal x z)) y))
;;
;; EJEMPLOS :
;;
;; (test-contenido 'c '(a b c)) -> T
;; (test-contenido 'c '(a b)) -> NIL
;; (test-contenido 'c 'nil) -> NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.1
;; eliminate-repeated-literals
;;
;; Eliminacion de literales repetidos una clausula
;;
;; RECIBE   : K - clausula (lista de literales, disyuncion implicita)
;; EVALUA A : clausula equivalente sin literales repetidos
;;
(defun eliminate-repeated-literals (k)
  (unless (null k)
    (let* ((primero   (first k))
           (resto     (rest k))
           (eliminar-sig (eliminate-repeated-literals resto)))
      (if (test-contenido primero resto)
          eliminar-sig
        (cons primero
              eliminar-sig)))))
;;
;; EJEMPLO:
;;
;; (eliminate-repeated-literals '(a b (~ c) (~ a) a c (~ c) c a))
;; -> (B (~ A) (~ C) C A)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test-clauses
;;
;; Comprueba si una clausula esta contenida en otra
;;
;; RECIBE   : k1, k2 - clausulas
;; EVALUA A : T si k1 esta contenida en k2
;;            NIL en caso contrario
;;
(defun test-clauses (k1 k2)
  (if (null k1)
    t
    (let ((primero (first k1)))
      (when (test-contenido primero k2)
        (unless (test-contenido primero
                                (remove primero (copy-list k2)
                                        :test #'equal))
          (and t (test-clauses (rest k1) k2)))))))
;;
;; EJEMPLOS:
;; (test-clauses '(a b) '(a b c d)) -> T
;; (test-clauses '(a b c d) '(a b)) -> NIL
;; (test-clauses 'nil '(a b)) -> T
;; (test-clauses '(a b) 'nil) -> NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.2
;; eliminate-repeated-clauses
;;
;; Eliminacion de clausulas repetidas en una FNC
;;
;; RECIBE   : cnf - FBF en FNC (lista de clausulas, conjuncion implicita)
;; EVALUA A : FNC equivalente sin clausulas repetidas
;;
(defun eliminate-repeated-clauses (cnf)
  (unless (null cnf)
    (let* ((elt         (eliminate-repeated-literals (first cnf)))
           (resto        (rest cnf))
           (eliminar-sig (eliminate-repeated-clauses resto)))
      (if (some #'(lambda(x) (and (test-clauses elt x)
                                  (test-clauses x elt))) resto)
          eliminar-sig
        (cons elt
              eliminar-sig)))))
;;
;; EJEMPLO:
;;
;; (eliminate-repeated-clauses '(((~ a) c) (c (~ a)) ((~ a) (~ a) b c b)
;; (a a b) (c (~ a) b  b) (a b))) -> ((C (~ A)) (C (~ A) B) (A B))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.3
;; subsume
;;
;; Predicado que determina si una clausula subsume otra
;;
;; RECIBE   : K1, K2 clausulas
;; EVALUA a : K1 si K1 subsume a K2
;;            NIL en caso contrario
;;
(defun subsume (k1 k2)
  (when (test-clauses k1 k2)
    (list k1)))
;;
;;  EJEMPLOS:
;;
;; (subsume '(a) '(a b (~ c))) -> ((a))
;; (subsume NIL '(a b (~ c))) -> (NIL)
;; (subsume '(a b (~ c)) '(a)) -> NIL
;; (subsume '(b (~ c)) '(a b (~ c))) -> ((b (~ c)))
;; (subsume '(a b (~ c)) '( b (~ c))) -> NIL
;; (subsume '(d b (~ e)) '(d  b (~ c))) -> nil
;; (subsume '(a b (~ c)) '((~ a) b (~ c) a)) -> ((A B (~ C)))
;; (subsume '((~ a) b (~ c) a) '(a b (~ c))) -> nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eliminar-aux
;;
;; Elimina las clausulas subsumidas por k de cnf
;;
;; RECIBE   : k   (clausula)
;;            cnf (FBF en FNC)
;; EVALUA A : FBF en FNC equivalente a cnf sin clausulas subsumidas por k
;;
(defun eliminar-aux (k cnf)
  (unless (null cnf)
    (let* ((primero       (first cnf))
           (eliminar-sig  (eliminar-aux k (rest cnf))))
      (if (null (subsume k primero))
          (cons primero
                eliminar-sig)
        eliminar-sig))))
;;
;; EJEMPLO:
;; (eliminar-aux '(a b) '((a b c) (b c) (a (~ c) b)
;; ((~ a) b) (a b (~ a)) (c b a))) -> ((B C) ((~ A) B))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.4
;; eliminate-subsumed-clauses
;;
;; Eliminacion de clausulas subsumidas en una FNC
;;
;; RECIBE   : cnf (FBF en FNC)
;; EVALUA A : FBF en FNC equivalente a cnf sin clausulas subsumidas
;;
(defun eliminate-subsumed-clauses (cnf)
  (unless (null cnf)
    (let ((primero  (first cnf))
          (resto    (rest cnf)))
      (if (every #'null (mapcar #'(lambda(x)
                                    (subsume x primero)) resto))
          (cons primero
                (eliminate-subsumed-clauses
                 (eliminar-aux primero
                               resto)))
        (eliminate-subsumed-clauses resto)))))
;;
;;  EJEMPLOS:
;;
;; (eliminate-subsumed-clauses '((a b c) (b c) (a (~ c) b)
;; ((~ a) b) (a b (~ a)) (c b a))) -> ((A (~ C) B) ((~ A) B) (B C))
;; (eliminate-subsumed-clauses '((a b c) (b c) (a (~ c) b) (b)
;; ((~ a) b) (a b (~ a)) (c b a))) -> ((B))
;; (eliminate-subsumed-clauses '((a b c) (b c) (a (~ c) b) ((~ a))
;; ((~ a) b) (a b (~ a)) (c b a))) -> ((A (~ C) B) ((~ A)) (B C))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.5
;; tautology-p
;;
;; Predicado que determina si una clausula es tautologia
;;
;; RECIBE   : K (clausula)
;; EVALUA a : T si K es tautologia
;;            NIL en caso contrario
;;
(defun tautology-p (k)
  (unless (null k)
    (let ((resto (rest k)))
      (or (test-contenido
           (reduce-scope-of-negation (list +not+ (first k)))
           resto)
          (tautology-p resto)))))
;;
;;  EJEMPLOS:
;;
;; (tautology-p '((~ B) A C (~ A) D)) -> T
;; (tautology-p '((~ B) A C D))       -> NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.6
;; eliminate-tautologies
;;
;; Eliminacion de clausulas en una FBF en FNC que son tautologia
;;
;; RECIBE   : cnf - FBF en FNC
;; EVALUA A : FBF en FNC equivalente a cnf sin tautologias
;;
(defun eliminate-tautologies (cnf)
  (unless (null cnf)
    (let ((primero       (first cnf))
          (eliminar-sig  (eliminate-tautologies (rest cnf))))
      (if (tautology-p primero)
          eliminar-sig
        (cons primero
              eliminar-sig)))))
;;
;;  EJEMPLOS:
;;
;; (eliminate-tautologies
;;  '(((~ b) a) (a (~ a) b c) ( a (~ b)) (s d (~ s) (~ s)) (a)))
;; -> (((~ B) A) (A (~ B)) (A))
;; (eliminate-tautologies '((a (~ a) b c)))
;; -> NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.7
;; simplify-cnf
;;
;; simplifica FBF en FNC
;;        * elimina literales repetidos en cada una de las clausulas
;;        * elimina clausulas repetidas
;;        * elimina tautologias
;;        * elimina clausulass subsumidas
;;
;; RECIBE   : cnf  FBF en FNC
;; EVALUA A : FNC equivalente sin clausulas repetidas,
;;            sin literales repetidos en las clausulas
;;            y sin clausulas subsumidas
;;
(defun simplify-cnf (cnf)
  (unless (null cnf)
    (eliminate-subsumed-clauses
     (eliminate-repeated-clauses
      (eliminate-tautologies cnf)))))
;;
;;  EJEMPLOS:
;;
;; (simplify-cnf '((a a) (b) (a) ((~ b)) ((~ b)) (a b c a)
;; (s s d) (b b c a b))) -> ((B) ((~ B)) (S D) (A)) en cualquier orden
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extract-clauses
;;
;; Unificacion de las funciones de extraen las clausulas de una FNC
;;
;; RECIBE   : cnf      - FBF en FNC simplificada
;;            lit      - literal positivo
;;            function - funcion para la condicion de extracion
;; EVALUA A : clausulas de cnf que cumplen la funcion function de lit
;;
(defun extract-clauses (lit cnf function)
  (unless (null cnf)
    (let ((primero      (first cnf))
          (extraer-sig  (extract-clauses lit (rest cnf) function)))
      (if (funcall function lit primero)
        (cons primero
              extraer-sig)
        extraer-sig))))
;;
;; EJEMPLO:
;; (extract-clauses 'p '((a b) (p b) (a b)) #'test-contenido) -> ((P B))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.1
;; extract-neutral-clauses
;;
;; Construye el conjunto de clausulas lit-neutras para una FNC
;;
;; RECIBE   : cnf  - FBF en FNC simplificada
;;            lit  - literal positivo
;; EVALUA A : cnf_lit^(0) subconjunto de clausulas de cnf
;;            que no contienen el literal lit ni ~lit
;;

;; Funcion auxiliar
(defun aux-neutral (x y)
  (every #'(lambda(z) (not (if (positive-literal-p z)
                               (equal z x)
                             (equal (second z) x))))
         y))

(defun extract-neutral-clauses (lit cnf)
  (extract-clauses lit cnf #'aux-neutral))
;;
;;  EJEMPLOS:
;;
;; (extract-neutral-clauses 'p '((p (~ q) r) (p q) (r (~ s) q)
;; (a b p) (a (~ p) c) ((~ r) s))) -> ((R (~ S) Q) ((~ R) S))
;; (extract-neutral-clauses 'r NIL)
;; -> NIL
;; (extract-neutral-clauses 'r '(NIL))
;; -> (NIL)
;; (extract-neutral-clauses 'r '((p (~ q) r) (p q) (r (~ s) q)
;; (a b p) (a (~ p) c) ((~ r) s))) -> ((P Q) (A B P) (A (~ P) C))
;; (extract-neutral-clauses 'p '((p (~ q) r) (p q) (r (~ s) p q)
;; (a b p) (a (~ p) c) ((~ r) p s))) -> NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.2
;; extract-positive-clauses
;;
;; Construye el conjunto de clausulas lit-positivas para una FNC
;;
;; RECIBE   : cnf    - FBF en FNC simplificada
;;            lit - literal positivo
;; EVALUA A : cnf_lit^(+) subconjunto de clausulas de cnf
;;            que contienen el literal lit
;;
(defun extract-positive-clauses (lit cnf)
	(extract-clauses lit cnf #'test-contenido))
;;
;;  EJEMPLOS:
;;
;; (extract-positive-clauses 'p '((p (~ q) r) (p q) (r (~ s) q)
;; (a b p) (a (~ p) c) ((~ r) s))) -> ((P (~ Q) R) (P Q) (A B P))
;; (extract-positive-clauses 'r NIL) -> NIL
;; (extract-positive-clauses 'r '(NIL)) -> NIL
;; (extract-positive-clauses 'r '((p (~ q) r) (p q) (r (~ s) q)
;; (a b p) (a (~ p) c) ((~ r) s))) -> ((P (~ Q) R) (R (~ S) Q))
;; (extract-positive-clauses 'p '(((~ p) (~ q) r) ((~ p) q)
;; (r (~ s) (~ p) q) (a b (~ p)) ((~ r) (~ p) s))) -> NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.3
;; extract-negative-clauses
;;
;; Construye el conjunto de clausulas lit-negativas para una FNC
;;
;; RECIBE   : cnf    - FBF en FNC simplificada
;;            lit - literal positivo
;; EVALUA A : cnf_lit^(-) subconjunto de clausulas de cnf
;;            que contienen el literal ~lit
;;

;; Funcion auxiliar
(defun aux-neg (x y)
  (some #'(lambda(z) (when (negative-literal-p z)
                       (equal (second z) x)))
        y))

(defun extract-negative-clauses (lit cnf)
  (extract-clauses lit cnf #'aux-neg))
;;
;; EJEMPLOS:
;;
;; (extract-negative-clauses 'p '((p (~ q) r) (p q) (r (~ s) q)
;; (a b p) (a (~ p) c) ((~ r) s))) -> ((A (~ P) C))
;; (extract-negative-clauses 'r NIL) -> NIL
;; (extract-negative-clauses 'r '(NIL)) -> NIL
;; (extract-negative-clauses 'r '((p (~ q) r) (p q) (r (~ s) q)
;; (a b p) (a (~ p) c) ((~ r) s))) -> (((~ R) S))
;; (extract-negative-clauses 'p '(( p (~ q) r) ( p q) (r (~ s) p q)
;; (a b p) ((~ r) p s))) -> NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eliminar-rastro
;;
;; Funcion no destructiva que elimina la presencia de un literal
;; en una clausula
;;
;; RECIBE   : lit  - literal positivo
;;            k    - clausula simplificada
;; EVALUA A : clausula los literales lit ~lit eliminados
;;
(defun eliminar-rastro (lit k)
  (remove (reduce-scope-of-negation (list +not+ lit))
          (remove lit (copy-list k) :test #'equal) :test #'equal))
;;
;; EJEMPLO:
;; (auxiliar 'a '(a b c (~ a))) -> (B C)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; possible-resolve-on
;;
;; Determina si es posible aplicar resolucion
;;
;; RECIBE   : lit       - literal positivo
;;            k1, k2    - clausulas simplificadas
;; EVALUA A : T si es posible
;;            NIL en caso contrario
;;
(defun possible-resolve-on (lit k1 k2)
  (if (test-contenido lit k1)
      (test-contenido (list +not+ lit) k2)
    (and (test-contenido (list +not+ lit) k1)
         (test-contenido lit k2))))
;;
;; EJEMPLOS:
;; (possible-resolve-on 'p '(a b (~ c)) '(p b a q r s)) -> NIL
;; (possible-resolve-on 'p '(a b (~ c) p) '((~ p) b a q r s)) -> T
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.4
;; resolve-on
;;
;; Resolvente de dos clausulas
;;
;; RECIBE   : lit      - literal positivo
;;            K1, K2      - clausulas simplificadas
;; EVALUA A : res_lit(K1,K2)
;;                        - lista que contiene la
;;                          clausula que resulta de aplicar resolucion
;;                          sobre K1 y K2, con los literales repetidos
;;                          eliminados
;;
(defun resolve-on (lit k1 k2)
  (unless (or (null k1) (null k2))
    (when (possible-resolve-on lit k1 k2)
      (list (eliminate-repeated-literals
             (eliminar-rastro lit (append k1 k2)))))))
;;
;;  EJEMPLOS:
;;
;; (resolve-on 'p '(a b (~ c) p) '((~ p) b a q r s))
;; -> (((~ C) B A Q R S))
;; (resolve-on 'p '(a b (~ c) (~ p)) '( p b a q r s))
;; -> (((~ C) B A Q R S))
;; (resolve-on 'p '(p) '((~ p)))
;; -> (NIL)
;; (resolve-on 'p NIL '(p b a q r s))
;; -> NIL
;; (resolve-on 'p NIL NIL)
;; -> NIL
;; (resolve-on 'p '(a b (~ c) (~ p)) '(p b a q r s))
;; -> (((~ C) B A Q R S))
;; (resolve-on 'p '(a b (~ c)) '(p b a q r s))
;; -> NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extract-positive-literals-clause
;;
;; Extra la lista de literales positivos de una clausula
;;
;; RECIBE   : clause  - clausula
;; EVALUA A : NIL si no hay literales positivos en clause,
;;            lista de literales positivos en caso contrario
(defun extract-positive-literals-clause (clause)
  (if (null clause)
      nil
    (let ((first-literal (first clause))
          (next-it (extract-positive-literals-clause (rest clause))))
      (if (positive-literal-p first-literal)
          (cons first-literal next-it)
        next-it))))
;;
;; EJEMPLOS:
;; (extract-positive-literals-clause '((~ a) (~ b))) -> NIL
;; (extract-positive-literals-clause '(a b (~ c) d)) -> (A B D)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extract-positive-literals-cnf
;;
;; Extra la lista de literales positivos de una expresion en FNC
;;
;; RECIBE   : cnf  - expresion en CNF
;; EVALUA A : NIL si no hay literales positivos en cnf,
;;            lista de literales positivos en caso contrario
(defun extract-positive-literals-cnf (cnf)
  (eliminate-repeated-literals (mapcan #'(lambda(x)
                                           (extract-positive-literals-clause x))
                                 cnf)))
;;
;; EJEMPLO:
;; (extract-positive-literals-cnf '((a b d) ((~ p) q) ((~ c) a b)
;; ((~ b) (~ p) d) (c d (~ a)))) -> (Q A B C D)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.5
;; build-RES
;;
;; Construye el conjunto de clausulas RES para una FNC
;;
;; RECIBE   : lit - literal positivo
;;            cnf    - FBF en FNC simplificada
;;
;; EVALUA A : RES_lit(cnf) con las clauses repetidas eliminadas
;;
(defun build-RES-aux (lit positivas negativas)
  (unless (or (null positivas) (null negativas))
    (append (mapcan #'(lambda(x) (resolve-on lit x (first negativas)))
              positivas)
            (build-RES-aux lit positivas (rest negativas)))))

(defun build-RES (lit cnf)
  (unless (null cnf)
    (eliminate-repeated-clauses
     (append (extract-neutral-clauses lit cnf)
             (build-RES-aux lit
                            (extract-positive-clauses lit cnf)
                            (extract-negative-clauses lit cnf))))))
;;
;;  EJEMPLOS:
;;
;; (build-RES 'p NIL) -> NIL
;; (build-RES 'P '((A  (~ P) B) (A P) (A B))) -> ((A B))
;; (build-RES 'P '((B  (~ P) A) (A P) (A B))) -> ((B A))
;; (build-RES 'p '(NIL)) -> (NIL)
;; (build-RES 'p '((p) ((~ p)))) -> (NIL)
;; (build-RES 'q '((p q) ((~ p) q) (a b q) (p (~ q)) ((~ p) (~ q))))
;; -> ((P) ((~ P) P) ((~ P)) (B A P) (B A (~ P)))
;; (build-RES 'p '((p q) (c q) (a b q) (p (~ q)) (p (~ q))))
;; -> ((A B Q) (C Q))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.5
;; RES-SAT-p
;;
;; Comprueba si una FNC es SAT calculando RES para todos los
;; atomos en la FNC
;;
;; RECIBE   : cnf - FBF en FNC simplificada
;; EVALUA A :	T  si cnf es SAT
;;                NIL  si cnf es UNSAT
;;
(defun RES-SAT-aux (pos-literals cnf)
  (cond
   ((null cnf) NIL)
   ((null pos-literals) (list NIL))
   (t (RES-SAT-aux (rest pos-literals)
                   (simplify-cnf (build-RES (first pos-literals)
                                            cnf))))))

(defun RES-SAT-p (cnf)
  (unless (equal (RES-SAT-aux (extract-positive-literals-cnf cnf)
                              cnf)
                 '(NIL))
    t))
;;
;; EJEMPLOS:
;;
;; SAT Examples
;;
;; (RES-SAT-p nil)  -> T
;; (RES-SAT-p '((p) ((~ q)))) -> T
;; (RES-SAT-p '((a b d) ((~ p) q) ((~ c) a b) ((~ b) (~ p) d)
;; (c d (~ a)))) -> T
;; (RES-SAT-p '(((~ p) (~ q) (~ r)) (q r) ((~ q) p) ((~ q))
;; ((~ p) (~ q) r))) -> T
;;
;; UNSAT Examples
;;
;; (RES-SAT-p '((P (~ Q)) NIL (K R))) -> NIL
;; (RES-SAT-p '(nil))         -> NIL
;; (RES-SAT-p '((S) nil))     -> NIL
;; (RES-SAT-p '((p) ((~ p)))) -> NIL
;; (RES-SAT-p '(((~ p) (~ q) (~ r)) (q r) ((~ q) p) (p) (q)
;; ((~ r)) ((~ p) (~ q) r))) -> NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.6:
;; logical-consequence-RES-SAT-p
;;
;; Resolucion basada en RES-SAT-p
;;
;; RECIBE   : wff - FBF en formato infijo
;;            w   - FBF en formato infijo
;;
;; EVALUA A : T   si w es consecuencia logica de wff
;;            NIL en caso de que no sea consecuencia logica.
(defun logical-consequence-RES-SAT-p (wff w)
  (let ((alpha (wff-infix-to-cnf wff))
        (beta (wff-infix-to-cnf (list +not+ w))))
    (when (null (RES-SAT-p (append alpha beta)))
      t)))
;;
;;  EJEMPLOS:
;;
;; (logical-consequence-RES-SAT-p NIL 'a) -> NIL
;; (logical-consequence-RES-SAT-p NIL NIL) -> NIL
;; (logical-consequence-RES-SAT-p '(q ^ (~ q)) 'a) -> T
;; (logical-consequence-RES-SAT-p '(q ^ (~ q)) '(~ a)) -> T
;; (logical-consequence-RES-SAT-p '((p => (~ p)) ^ p) 'q) -> T
;; (logical-consequence-RES-SAT-p '((p => (~ p)) ^ p) '(~ q)) -> T
;; (logical-consequence-RES-SAT-p '((p => q) ^ p) 'q) -> T
;; (logical-consequence-RES-SAT-p '((p => q) ^ p) '(~ q)) -> NIL
;; (logical-consequence-RES-SAT-p '(((~ p) => q) ^ (p => (a v (~ b))) ^
;;   (p => ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) '(~ a)) -> T
;; (logical-consequence-RES-SAT-p '(((~ p) => q) ^ (p => (a v (~ b))) ^
;;   (p => ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 'a) -> T
;; (logical-consequence-RES-SAT-p '(((~ p) => q) ^ (p => ((~ a) ^ b)) ^
;;   ( (~ p) => (r  ^ (~ q)))) 'a) -> NIL
;; (logical-consequence-RES-SAT-p '(((~ p) => q) ^ (p => ((~ a) ^ b)) ^
;;   ( (~ p) => (r  ^ (~ q)))) '(~ a)) -> T
;; (logical-consequence-RES-SAT-p '(((~ p) => q) ^ (p <=> ((~ a) ^ b)) ^
;;   ( (~ p) => (r  ^ (~ q)))) 'q) -> NIL
;; (logical-consequence-RES-SAT-p '(((~ p) => q) ^ (p <=> ((~ a) ^ b)) ^
;;   ( (~ p) => (r  ^ (~ q)))) '(~ q)) -> NIL
;; (logical-consequence-RES-SAT-p '(((~ p) => q) ^ (p <=> ((~ a) ^ b)) ^
;;   ( (~ p) => (r  ^ (~ q)))) 'q) -> NIL
;; (logical-consequence-RES-SAT-p '(((~ p) => q) ^ (p <=> ((~ a) ^ b)) ^
;;   ( (~ p) => (r  ^ (~ q)))) '(~ q)) -> NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EJERCICIO 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 5.3, 5.4
;; bfs
;;
;; Ejecuta el algoritmo de b�squeda primero en anchura
;;
;; RECIBE   : end    - nodo meta
;;            queue  - cola de caminos explorados
;;            net    - grafo
;; EVALUA A : camino que lleva a la meta,
;;            NIL si no existe camino
;;
(defun new-paths (path node net)
  ;; toma la lista de vecinos de node y a�ade a cada camino el
  ;; camino que surge de seguir explorando por el vecino
  (mapcar #'(lambda(n)
              (cons n path))
    (rest (assoc node net))))

(defun bfs (end queue net)
  ;; en queue se encuentran todos los caminos (en orden inverso)
  ;; para ir de un nodo (el que seleccionemos al llamar a bfs o
  ;; los nodos de un conjunto que elijamos) a otro, con el objetivo
  ;; de llegar al nodo end; si queue se vacia, entonces es que no
  ;; existe un camino
  (if (null queue) '()
    ;; tomamos el primer camino de queue y su primer nodo (que por
    ;; el orden inverso, es el ultimo que se ha explorado)
    (let* ((path (first queue))
           (node (first path)))
      ;; si ese nodo es la meta, hemos terminado y se devuelve path
      ;; invertido (dado que queremos camino desde inicio hasta fin)
      (if (eql node end)
          (reverse path)
        ;; si ese nodo no es meta, repetimos el proceso para el resto
        ;; de caminos dentro de queue, y a�adiendo aquellos que surgen
        ;; de explorar el nodo que hemos explorado y no es meta
        (bfs end
             (append (rest queue)
                     (new-paths path node net))
             net)))))
;;
;; EJEMPLO:
;; (bfs 'f '((a)) '((a d) (b d f) (c e) (d f) (e b f) (f))) -> (A D F)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 5.5, 5.6
;; shortest-path
;;
;; Obtiene el camino optimo entre dos nodos
;;
;; RECIBE   : start  - nodo de inicio
;;            end    - nodo de llegada
;;            net    - grafo
;; EVALUA A : camino optimo entre start y end,
;;            NIL si no existe camino
;;
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))
;;
;; EJEMPLO:
;; (shortest-path 'a 'f '((a d) (b d f) (c e) (d f) (e b f) (f))) -> (A D F)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 5.8
;; bfs-improved, shortest-path-improved
;;
;; Solucionan el problema de la inexistencia de caminos
;; para grafos con ciclos
;;
;; Entradas y salidas iguales que para bfs y shortest-path
;;
(defun bfs-improved (end queue net)
  (if (null queue) '()
    (let* ((path (first queue))
           (node (first path)))
      (if (eql node end)
          (reverse path)
        (bfs-improved end
                      (append (rest queue)
                              (new-paths path node net))
                      (remove (assoc node net) net))))))

(defun shortest-path-improved (start end net)
  (bfs-improved end (list (list start)) net))
;;
;; EJEMPLO:
;; (shortest-path-improved 'f 'c '((a b d e) (b a d e f)
;; (c a g) (d a b g h) (e a b g h) (f b h) (g d e h) (h d e f g)))
;; -> NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
