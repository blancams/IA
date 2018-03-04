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
;; EJERCICIO 4.1.2
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
;; (positive-literal-p '¬)
;; (positive-literal-p '=>)
;; (positive-literal-p '(p))
;; (positive-literal-p '(¬ p))
;; (positive-literal-p '(¬ (v p q)))
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
;; (negative-literal-p '(¬ p))        ; T
;; (negative-literal-p NIL)           ; NIL
;; (negative-literal-p '¬)            ; NIL
;; (negative-literal-p '=>)           ; NIL
;; (negative-literal-p '(p))          ; NIL
;; (negative-literal-p '((¬ p)))      ; NIL
;; (negative-literal-p '(¬ T))        ; NIL
;; (negative-literal-p '(¬ NIL))      ; NIL
;; (negative-literal-p '(¬ =>))       ; NIL
;; (negative-literal-p 'p)            ; NIL
;; (negative-literal-p '((¬ p)))      ; NIL
;; (negative-literal-p '(¬ (v p q)))  ; NIL
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
;; (literal-p '(¬ p))
;; ;-> evaluan a T
;; (literal-p '(p))
;; (literal-p '(¬ (v p q)))
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
;; (wff-prefix-p '(^ (¬ B)))
;; (wff-prefix-p '(v A (¬ B)))
;; (wff-prefix-p '(v (¬ B) A ))
;; (wff-prefix-p '(^ (V P (=> A (^ B (¬ C) D))) (^ (<=> P (¬ Q)) P) E))
;; evaluan a T
;; (wff-prefix-p 'NIL)
;; (wff-prefix-p '(¬))
;; (wff-prefix-p '(=>))
;; (wff-prefix-p '(<=>))
;; (wff-prefix-p '(^ (V P (=> A ( B ^ (¬ C) ^ D))) (^ (<=> P (¬ Q)) P) E))
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
;;(wff-infix-p 'a) 						; T
;;(wff-infix-p '(^)) 					; T  ;; por convencion
;;(wff-infix-p '(v)) 					; T  ;; por convencion
;;(wff-infix-p '(A ^ (v))) 			      ; T
;;(wff-infix-p '( a ^ b ^ (p v q) ^ (¬ r) ^ s))  	; T
;;(wff-infix-p '(A => B)) 				; T
;;(wff-infix-p '(A => (B <=> C))) 			; T
;;(wff-infix-p '( B => (A ^ C ^ D))) 			; T
;;(wff-infix-p '( B => (A ^ C))) 			; T
;;(wff-infix-p '( B ^ (A ^ C))) 			; T
;;(wff-infix-p '((p v (a => (b ^ (¬ c) ^ d))) ^ ((p <=> (¬ q)) ^ p ) ^ e))  ; T
;;(wff-infix-p nil) 					; NIL
;;(wff-infix-p '(a ^)) 					; NIL
;;(wff-infix-p '(^ a)) 					; NIL
;;(wff-infix-p '(a)) 					; NIL
;;(wff-infix-p '((a))) 				      ; NIL
;;(wff-infix-p '((a) b))   			      ; NIL
;;(wff-infix-p '(^ a b q (¬ r) s))  		      ; NIL
;;(wff-infix-p '( B => A C)) 			      ; NIL
;;(wff-infix-p '( => A)) 				      ; NIL
;;(wff-infix-p '(A =>)) 				      ; NIL
;;(wff-infix-p '(A => B <=> C)) 		      ; NIL
;;(wff-infix-p '( B => (A ^ C v D))) 		      ; NIL
;;(wff-infix-p '( B ^ C v D )) 			      ; NIL
;;(wff-infix-p '((p v (a => e (b ^ (¬ c) ^ d))) ^ ((p <=> (¬ q)) ^ p ) ^ e)); NIL
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
           ((null elements-wff)        ;;; conjuncion o disyuncion vacias.
            wff)                       ;;; por convencion, se acepta como fbf en formato infijo
           ((null (cdr elements-wff))  ;;; conjuncion o disyuncion con un unico elemento
            (prefix-to-infix (car elements-wff)))
           (t (cons (prefix-to-infix (first elements-wff))
                    (mapcan #'(lambda(x) (list connector (prefix-to-infix x)))
                      (rest elements-wff))))))
         (t NIL)))))) ;; no deberia llegar a este paso nunca
;;
;;  EJEMPLOS:
;;
;; (prefix-to-infix '(v))          ; (V)
;; (prefix-to-infix '(^))          ; (^)
;; (prefix-to-infix '(v a))        ; A
;; (prefix-to-infix '(^ a))        ; A
;; (prefix-to-infix '(^ (¬ a)))    ; (¬ a)
;; (prefix-to-infix '(v a b))      ; (A v B)
;; (prefix-to-infix '(v a b c))    ; (A V B V C)
;; (prefix-to-infix '(^ (V P (=> A (^ B (¬ C) D))) (^ (<=> P (¬ Q)) P) E))
;; ;-> ((P V (A => (B ^ (¬ C) ^ D))) ^ ((P <=> (¬ Q)) ^ P) ^ E)
;; (prefix-to-infix '(^ (v p (=> a (^ b (¬ c) d))))) ; (P V (A => (B ^ (¬ C) ^ D)))
;; (prefix-to-infix '(^ (^ (<=> p (¬ q)) p ) e))     ; (((P <=> (¬ Q)) ^ P) ^ E)
;; (prefix-to-infix '( v (¬ p) q (¬ r) (¬ s)))       ; ((¬ P) V Q V (¬ R) V (¬ S))
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
    (cons (first op) (infix-to-prefix-eliminate-connectors-aux (rest (rest op))))))

(defun infix-to-prefix (wff)
  (when (wff-infix-p wff)
    (if (literal-p wff)
      wff
      (let ((op1 (first wff))
        	  (op2 (first (rest wff)))
            (op3 (rest (rest wff))))
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
                            (infix-to-prefix-eliminate-connectors-aux op3))))))
          (t NIL))))))
;;
;; EJEMPLOS
;;
;; (infix-to-prefix nil)      ;; NIL
;; (infix-to-prefix 'a)       ;; a
;; (infix-to-prefix '((a)))   ;; NIL
;; (infix-to-prefix '(a))     ;; NIL
;; (infix-to-prefix '(((a)))) ;; NIL
;; (prefix-to-infix (infix-to-prefix '((p v (a => (b ^ (¬ c) ^ d))) ^ ((p <=> (¬ q)) ^ p) ^ e)))
;; -> ((P V (A => (B ^ (¬ C) ^ D))) ^ ((P <=> (¬ Q)) ^ P) ^ E)
;; (infix-to-prefix '((p v (a => (b ^ (¬ c) ^ d))) ^  ((p <=> (¬ q)) ^ p) ^ e))
;; -> (^ (V P (=> A (^ B (¬ C) D))) (^ (<=> P (¬ Q)) P) E)
;; (infix-to-prefix '(¬ ((¬ p) v q v (¬ r) v (¬ s))))
;; -> (¬ (V (¬ P) Q (¬ R) (¬ S)))
;; (infix-to-prefix (prefix-to-infix'(V (¬ P) Q (¬ R) (¬ S))))
;; -> (V (¬ P) Q (¬ R) (¬ S))
;; (infix-to-prefix (prefix-to-infix'(¬ (V (¬ P) Q (¬ R) (¬ S)))))
;; -> (¬ (V (¬ P) Q (¬ R) (¬ S)))
;; (infix-to-prefix 'a)  ; A
;; (infix-to-prefix '((p v (a => (b ^ (¬ c) ^ d))) ^  ((p <=> (¬ q)) ^ p) ^ e))
;; -> (^ (V P (=> A (^ B (¬ C) D))) (^ (<=> P (¬ Q)) P) E)
;; (infix-to-prefix '(¬ ((¬ p) v q v (¬ r) v (¬ s))))
;; -> (¬ (V (¬ P) Q (¬ R) (¬ S)))
;; (infix-to-prefix  (prefix-to-infix '(^ (v p (=> a (^ b (¬ c) d)))))) ; '(v p (=> a (^ b (¬ c) d))))
;; (infix-to-prefix  (prefix-to-infix '(^ (^ (<=> p (¬ q)) p ) e))) ; '(^ (^ (<=> p (¬ q)) p ) e))
;; (infix-to-prefix (prefix-to-infix '( v (¬ p) q (¬ r) (¬ s))))  ; '( v (¬ p) q (¬ r) (¬ s)))
;; (infix-to-prefix '(p v (a => (b ^ (¬ c) ^ d)))) ; (V P (=> A (^ B (¬ C) D)))
;; (infix-to-prefix '(((P <=> (¬ Q)) ^ P) ^ E))  ; (^ (^ (<=> P (¬ Q)) P) E)
;; (infix-to-prefix '((¬ P) V Q V (¬ R) V (¬ S))); (V (¬ P) Q (¬ R) (¬ S))
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
;; (clause-p '(v (¬ r)))       ; T
;; (clause-p '(v p q (¬ r) s)) ; T
;; (clause-p NIL)                    ; NIL
;; (clause-p 'p)                     ; NIL
;; (clause-p '(¬ p))                 ; NIL
;; (clause-p NIL)                    ; NIL
;; (clause-p '(p))                   ; NIL
;; (clause-p '((¬ p)))               ; NIL
;; (clause-p '(^ a b q (¬ r) s))     ; NIL
;; (clause-p '(v (^ a b) q (¬ r) s)) ; NIL
;; (clause-p '(¬ (v p q)))           ; NIL
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
;; (cnf-p '(^ (v a  b c) (v q r) (v (¬ r) s) (v a b))) ; T
;; (cnf-p '(^ (v a  b (¬ c)) ))                        ; T
;; (cnf-p '(^ ))                                       ; T
;; (cnf-p '(^(v )))                                    ; T
;; (cnf-p '(¬ p))                                      ; NIL
;; (cnf-p '(^ a b q (¬ r) s))                          ; NIL
;; (cnf-p '(^ (v a b) q (v (¬ r) s) a b))              ; NIL
;; (cnf-p '(v p q (¬ r) s))                            ; NIL
;; (cnf-p '(^ (v a b) q (v (¬ r) s) a b))              ; NIL
;; (cnf-p '(^ p))                                      ; NIL
;; (cnf-p '(v ))                                       ; NIL
;; (cnf-p NIL)                                         ; NIL
;; (cnf-p '((¬ p)))                                    ; NIL
;; (cnf-p '(p))                                        ; NIL
;; (cnf-p '(^ (p)))                                    ; NIL
;; (cnf-p '((p)))                                      ; NIL
;; (cnf-p '(^ a b q (r) s))                            ; NIL
;; (cnf-p '(^ (v a  (v b c)) (v q r) (v (¬ r) s) a b)) ; NIL
;; (cnf-p '(^ (v a (^ b c)) (^ q r) (v (¬ r) s) a b))  ; NIL
;; (cnf-p '(¬ (v p q)))                                ; NIL
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
          (let ((wff1 (eliminate-biconditional (second wff))) ;; eliminamos los bicondicionales que pueda haber
                (wff2 (eliminate-biconditional (third  wff)))) ;; en las FBFs implicadas
            (list +and+                             ;; (<=> A B) equivale a
                  (list +cond+ wff1 wff2)           ;; (^ (=> A B) (=> B A)) en notacion prefijo
                  (list +cond+ wff2 wff1)))
        (cons connector                             ;; si el conector no es <=>
              (mapcar #'eliminate-biconditional (rest wff))))))) ;; repetimos el proceso con el resto
;;
;; EJEMPLOS:
;;
;; (eliminate-biconditional '(<=> p  (v q s p) ))
;; -> (^ (=> P (v Q S P)) (=> (v Q S P) P))
;; (eliminate-biconditional '(<=>  (<=> p  q) (^ s (¬ q))))
;; -> (^ (=> (^ (=> P Q) (=> Q P)) (^ S (¬ Q)))
;;      (=> (^ S (¬ Q)) (^ (=> P Q) (=> Q P))))
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
;; (eliminate-conditional '(=> p q))                      ;;; (V (¬ P) Q)
;; (eliminate-conditional '(=> p (v q s p)))              ;;; (V (¬ P) (V Q S P))
;; (eliminate-conditional '(=> (=> (¬ p) q) (^ s (¬ q)))) ;;; (V (¬ (V (¬ (¬ P)) Q)) (^ S (¬ Q)))
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
  	    		                              (reduce-scope-of-negation (list +not+ x))))
  	                       (first resto))
  	               (reduce-scope-of-negation (rest resto)))))
  	    (cons (first wff)
              (mapcar #'reduce-scope-of-negation (rest wff)))))))
;;
;;  EJEMPLOS:
;;
;; (reduce-scope-of-negation '(¬ (v p (¬ q) r)))
;; -> (^ (¬ P) Q (¬ R))
;; (reduce-scope-of-negation '(¬ (^ p (¬ q) (v  r s (¬ a)))))
;; -> (V (¬ P) Q (^ (¬ R) (¬ S) A))
;; (reduce-scope-of-negation '(^ (v P (v (¬ a) (^ b (¬ c) d))) (^ (^ (v (¬ p) (¬ q)) (v (¬ (¬ q)) p)) p) e))
;; -> (^ (V P (V (¬ A) (^ B (¬ C) D))) (^ (^ (V (¬ P) (¬ Q)) (V Q P)) P) E)
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
;; (cnf '(¬ a)) -> (^ (V (¬ A)))
;; (cnf '(v (¬ a) b c)) -> (^ (V (¬ A) B C))
;; (cnf '(^ (v (¬ a) b c) (¬ e) (^ e f (¬ g) h) (v m n) (^ r s q) (v u q) (^ x y))) ->
;; (^ (V (¬ A) B C) (V (¬ E)) (V E) (V F) (V (¬ G)) (V H) (V M N) (V R) (V S) (V Q) (V U Q) (V X) (V Y))
;; (cnf '(^ (v p  (¬ q)) a (v k  r  (^ m  n)))) ->
;; (^ (V P (¬ Q)) (V A) (V K R M) (V K R N))
;; (cnf '(v p  q  (^ r  m)  (^ n  a)  s )) ->
;; (^ (V P Q R N S) (V P Q R A S) (V P Q M N S) (V P Q M A S))
;; (cnf '(^ (v a b (^ y r s) (v k l)) c (¬ d) (^ e f (v h i) (^ o p)))) ->
;; (^ (V A B Y K L) (V A B R K L) (V A B S K L) (V C) (V (¬ D)) (V E) (V F) (V H I) (V O) (V P))
;; (cnf '(^ (v a b (^ y r s)) c (¬ d) (^ e f (v h i) (^ o p)))) ->
;; (^ (V A B Y) (V A B R) (V A B S) (V C) (V (¬ D)) (V E) (V F) (V H I) (V O) (V P))
;; (cnf '(^ (^ y r s (^ p q (v c d))) (v a b))) ->
;; (^ (V Y) (V R) (V S) (V P) (V Q) (V C D) (V A B))
;; (cnf '(^ (v (¬ a) b c) (¬ e) r s (v e f (¬ g) h) k (v m n) d)) ->
;; (^ (V (¬ A) B C) (V (¬ E)) (V R) (V S) (V E F (¬ G) H) (V K) (V M N) (V D))
;; (cnf '(^ (v p (¬ q)) (v k r (^ m  n)))) ->
;; (^ (V P (¬ Q)) (V K R M) (V K R N))
;; (cnf '(v (v p q) e f (^ r  m) n (^ a (¬ b) c) (^ d s))) ->
;; (^ (V P Q E F R N A D) (V P Q E F R N A S) (V P Q E F R N (¬ B) D)
;; (V P Q E F R N (¬ B) S) (V P Q E F R N C D) (V P Q E F R N C S)
;; (V P Q E F M N A D) (V P Q E F M N A S) (V P Q E F M N (¬ B) D)
;; (V P Q E F M N (¬ B) S) (V P Q E F M N C D) (V P Q E F M N C S))
;; (cnf '(^ (^ (¬ y) (v r (^ s (¬ x)) (^ (¬ p) m (v c d))) (v (¬ a) (¬ b))) g)) ->
;; (^ (V (¬ Y)) (V R S (¬ P)) (V R S M) (V R S C D) (V R (¬ X) (¬ P)) (V R (¬ X) M) (V R (¬ X) C D) (V (¬ A) (¬ B)) (V G))
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
;; EVALUA A : FBF en FNC (con conectores ^, v eliminaos)
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
;; (eliminate-connectors (cnf '(^ (v p  (¬ q))  (v k  r  (^ m  n))))) -> ((P (¬ Q)) (K R M) (K R N))
;; (eliminate-connectors (cnf '(^ (v (¬ a) b c) (¬ e) (^ e f (¬ g) h) (v m n) (^ r s q) (v u q) (^ x y)))) ->
;; (((¬ A) B C) ((¬ E)) (E) (F) ((¬ G)) (H) (M N) (R) (S) (Q) (U Q) (X) (Y))
;; (eliminate-connectors (cnf '(v p  q  (^ r  m)  (^ n  q)  s ))) ->
;; ((P Q R N S) (P Q R Q S) (P Q M N S) (P Q M Q S))
;; (eliminate-connectors (print (cnf '(^ (v p  (¬ q)) (¬ a) (v k  r  (^ m  n)))))) ->
;; ((P (¬ Q)) ((¬ A)) (K R M) (K R N))
;; (eliminate-connectors '(^)) -> NIL
;; (eliminate-connectors '(^ (v p (¬ q)) (v) (v k r))) -> ((P (¬ Q)) NIL (K R))
;; (eliminate-connectors '(^ (v a b))) -> ((A B))
;; (eliminate-connectors '(^ (v p (¬ q)) (v k r))) -> ((P (¬ Q)) (K R))
;; (eliminate-connectors '(^ (v p (¬ q)) (v q (¬ a)) (v s e f) (v b))) -> ((P (¬ Q)) (Q (¬ A)) (S E F) (B))
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
  (eliminate-connectors (cnf (reduce-scope-of-negation (eliminate-conditional (eliminate-biconditional (infix-to-prefix wff)))))))
;;
;; EJEMPLOS:
;;
;; (wff-infix-to-cnf 'a) -> ((A))
;; (wff-infix-to-cnf '(¬ a)) -> (((¬ A)))
;; (wff-infix-to-cnf  '( (¬ p) v q v (¬ r) v (¬ s))) -> (((¬ P) Q (¬ R) (¬ S)))
;; (wff-infix-to-cnf  '((p v (a => (b ^ (¬ c) ^ d))) ^ ((p <=> (¬ q)) ^ p) ^ e)) ->
;; ((P (¬ A) B) (P (¬ A) (¬ C)) (P (¬ A) D) ((¬ P) (¬ Q)) (Q P) (P) (E))
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
;; (test-contenido 'c '(a b c)) ;T
;; (test-contenido 'c '(a b)) ;NIL
;; (test-contenido 'c 'nil) ;NIL
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
;; (eliminate-repeated-literals '(a b (¬ c) (¬ a) a c (¬ c) c a))
;; ;-> (B (¬ A) (¬ C) C A)
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
        (unless (test-contenido primero (remove primero (copy-list k2) :test #'equal))
          (and t (test-clauses (rest k1) k2)))))))
;;
;; EJEMPLOS:
;; (test-clauses '(a b) '(a b c d)) ;T
;; (test-clauses '(a b c d) '(a b)) ;NIL
;; (test-clauses 'nil '(a b)) ;T
;; (test-clauses '(a b) 'nil) ;NIL
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
    (if (some #'(lambda(x) (and (test-clauses elt x) (test-clauses x elt))) resto)
      eliminar-sig
      (cons elt
      	    eliminar-sig)))))
;;
;; EJEMPLO:
;;
;; (eliminate-repeated-clauses '(((¬ a) c) (c (¬ a)) ((¬ a) (¬ a) b c b) (a a b) (c (¬ a) b  b) (a b)))
;; ->; ((C (¬ A)) (C (¬ A) B) (A B))
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
;; (subsume '(a) '(a b (¬ c))) ;-> ((a))
;; (subsume NIL '(a b (¬ c))) ;-> (NIL)
;; (subsume '(a b (¬ c)) '(a)) ;-> NIL
;; (subsume '(b (¬ c)) '(a b (¬ c))) ;-> ((b (¬ c)))
;; (subsume '(a b (¬ c)) '( b (¬ c))) ;-> NIL
;; (subsume '(d b (¬ e)) '(d  b (¬ c))) ;-> nil
;; (subsume '(a b (¬ c)) '((¬ a) b (¬ c) a)) ;-> ((A B (¬ C)))
;; (subsume '((¬ a) b (¬ c) a) '(a b (¬ c))) ;-> nil
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
;; (eliminar-aux '(a b) '((a b c) (b c) (a (¬ c) b)  ((¬ a) b) (a b (¬ a)) (c b a)))
;; ;-> ((B C) ((¬ A) B))
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
  	(if (every #'null (mapcar #'(lambda(x) (subsume x primero)) resto))
  	  (cons primero
  	  	    (eliminate-subsumed-clauses (eliminar-aux primero resto)))
  	  (eliminate-subsumed-clauses resto)))))
;;
;;  EJEMPLOS:
;;
;; (eliminate-subsumed-clauses '((a b c) (b c) (a (¬ c) b)  ((¬ a) b) (a b (¬ a)) (c b a)))
;; ;-> ((A (¬ C) B) ((¬ A) B) (B C))
;; (eliminate-subsumed-clauses '((a b c) (b c) (a (¬ c) b) (b)  ((¬ a) b) (a b (¬ a)) (c b a)))
;; ;-> ((B))
;; (eliminate-subsumed-clauses '((a b c) (b c) (a (¬ c) b) ((¬ a))  ((¬ a) b) (a b (¬ a)) (c b a)))
;; ;-> ((A (¬ C) B) ((¬ A)) (B C))
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
  	(or (test-contenido (reduce-scope-of-negation (list +not+ (first k)))
                        resto)
  	 	  (tautology-p resto)))))
;;
;;  EJEMPLOS:
;;
;; (tautology-p '((¬ B) A C (¬ A) D)) ;;; T
;; (tautology-p '((¬ B) A C D))       ;;; NIL
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
;;  '(((¬ b) a) (a (¬ a) b c) ( a (¬ b)) (s d (¬ s) (¬ s)) (a)))
;; -> (((¬ B) A) (A (¬ B)) (A))
;; (eliminate-tautologies '((a (¬ a) b c)))
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
  	(eliminate-subsumed-clauses (eliminate-repeated-clauses (eliminate-tautologies cnf)))))
;;
;;  EJEMPLOS:
;;
;; (simplify-cnf '((a a) (b) (a) ((¬ b)) ((¬ b)) (a b c a) (s s d) (b b c a b)))
;; ;-> ((B) ((¬ B)) (S D) (A)) ;; en cualquier orden
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extract-clauses
;;
;; Unificacion de las funciones de extraen las clausulas de una FNC
;;
;; RECIBE   : cnf      - FBF en FNC simplificada
;;            lit      - literal positivo
;;            function - funcion para la condicion de extracion de la clausula
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
;; (extract-clauses 'p '((a b) (p b) (a b)) #'test-contenido) ;-> ((P B))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; aux-neutral
;;
;; Comprueba si todos los literales de una lista son diferentes
;; de un literal dado (tanto negado como no)
;;
;; RECIBE   : x  - literal a comparar
;;            y  - lista de literales a comparar
;; EVALUA A : T si no hay coincidencia entre literal y literales
;;            de la lista,
;;            NIL si hay alguna coincidencia
(defun aux-neutral (x y)
  (every #'(lambda(z) (not (if (positive-literal-p z)
                             (equal z x)
                             (equal (second z) x))))
        y))
;; EJEMPLOS:
;; (aux-neutral 'a '(a b c)) -> NIL
;; (aux-neutral 'a '((¬ b) (¬ a))) -> NIL
;; (aux-neutral 'a '(b c)) -> T
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
;;            que no contienen el literal lit ni ¬lit
;;
(defun extract-neutral-clauses (lit cnf)
  (extract-clauses lit cnf #'aux-neutral))
;;
;;  EJEMPLOS:
;;
;; (extract-neutral-clauses 'p '((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))
;; -> ((R (¬ S) Q) ((¬ R) S))
;; (extract-neutral-clauses 'r NIL)
;; -> NIL
;; (extract-neutral-clauses 'r '(NIL))
;; -> (NIL)
;; (extract-neutral-clauses 'r '((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))
;; -> ((P Q) (A B P) (A (¬ P) C))
;; (extract-neutral-clauses 'p '((p (¬ q) r) (p q) (r (¬ s) p q) (a b p) (a (¬ p) c) ((¬ r) p s)))
;; -> NIL
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
;; (extract-positive-clauses 'p '((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))
;; ;-> ((P (¬ Q) R) (P Q) (A B P))
;; (extract-positive-clauses 'r NIL)
;; ;-> NIL
;; (extract-positive-clauses 'r '(NIL))
;; ;-> NIL
;; (extract-positive-clauses 'r '((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))
;; ;-> ((P (¬ Q) R) (R (¬ S) Q))
;; (extract-positive-clauses 'p '(((¬ p) (¬ q) r) ((¬ p) q) (r (¬ s) (¬ p) q) (a b (¬ p)) ((¬ r) (¬ p) s)))
;; ;-> NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; aux-neg
;;
;; Comprueba si existe algun literal de una lista que sea el
;; literal negado pasado por argumento
;;
;; RECIBE   : x  - literal a comparar
;;            y  - lista a comparar
;; EVALUA A : T si existe algun elemento de y que sea el negado
;;            de x,
;;            NIL en caso contrario
(defun aux-neg (x y)
  (some #'(lambda(z) (when (negative-literal-p z)
                       (equal (second z) x)))
        y))
;; EJEMPLOS:
;; (aux-neg 'a '(a b)) -> NIL
;; (aux-neg 'b '(a c)) -> NIL
;; (aux-neg 'a '((¬ a) b)) -> T
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
;;            que contienen el literal ¬lit
;;
(defun extract-negative-clauses (lit cnf)
  (extract-clauses lit cnf #'aux-neg))
;;
;;  EJEMPLOS:
;;
;; (extract-negative-clauses 'p '((p (¬ q) r) (p q) (r (¬ s) qz) (a b p) (a (¬ p) c) ((¬ r) s)))
;; -> ((A (¬ P) C))
;;(extract-negative-clauses 'r NIL)
;; -> NIL
;;(extract-negative-clauses 'r '(NIL))
;; -> NIL
;; (extract-negative-clauses 'r '((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))
;; -> (((¬ R) S))
;;(extract-negative-clauses 'p '(( p (¬ q) r) ( p q) (r (¬ s) p q) (a b p) ((¬ r) p s)))
;; -> NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eliminar-rastro
;;
;; Funcion no destructiva que elimina la presencia de un literal
;; en una clausula
;;
;; RECIBE   : lit  - literal positivo
;;            k    - clausula simplificada
;; EVALUA A : clausula los literales lit ¬lit eliminados
;;
(defun eliminar-rastro (lit k)
    (remove (reduce-scope-of-negation (list +not+ lit))
      (remove lit (copy-list k) :test #'equal) :test #'equal))
;;
;; EJEMLO:
;; (auxiliar 'a '(a b c (¬ a))) -> (B C)
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
;; (possible-resolve-on 'p '(a b (¬ c)) '(p b a q r s)) -> NIL
;; (possible-resolve-on 'p '(a b (¬ c) p) '((¬ p) b a q r s)) -> T
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
;; (resolve-on 'p '(a b (¬ c) p) '((¬ p) b a q r s))
;; -> (((¬ C) B A Q R S))
;; (resolve-on 'p '(a b (¬ c) (¬ p)) '( p b a q r s))
;; -> (((¬ C) B A Q R S))
;; (resolve-on 'p '(p) '((¬ p)))
;; -> (NIL)
;; (resolve-on 'p NIL '(p b a q r s))
;; -> NIL
;; (resolve-on 'p NIL NIL)
;; -> NIL
;; (resolve-on 'p '(a b (¬ c) (¬ p)) '(p b a q r s))
;; -> (((¬ C) B A Q R S))
;; (resolve-on 'p '(a b (¬ c)) '(p b a q r s))
;; -> NIL
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
;; (build-RES 'P '((A  (¬ P) B) (A P) (A B))) -> ((A B))
;; (build-RES 'P '((B  (¬ P) A) (A P) (A B))) -> ((B A))
;; (build-RES 'p '(NIL)) -> (NIL)
;; (build-RES 'p '((p) ((¬ p)))) -> (NIL)
;; (build-RES 'q '((p q) ((¬ p) q) (a b q) (p (¬ q)) ((¬ p) (¬ q))))
;; -> ((P) ((¬ P) P) ((¬ P)) (B A P) (B A (¬ P)))
;; (build-RES 'p '((p q) (c q) (a b q) (p (¬ q)) (p (¬ q))))
;; -> ((A B Q) (C Q))
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
;; (extract-positive-literals-clause '((¬ a) (¬ b))) -> NIL
;; (extract-positive-literals-clause '(a b (¬ c) d)) -> (A B D)
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
    (eliminate-repeated-literals (mapcan #'(lambda(x) (extract-positive-literals-clause x)) cnf)))
;;
;; EJEMPLO:
;; (extract-positive-literals-cnf '((a b d) ((¬ p) q) ((¬ c) a b) ((¬ b) (¬ p) d) (c d (¬ a))))
;; -> (Q A B C D)
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
      (t (RES-SAT-aux (rest pos-literals) (simplify-cnf (build-RES (first pos-literals) cnf))))))


(defun  RES-SAT-p (cnf)
   (unless (equal (RES-SAT-aux (extract-positive-literals-cnf cnf) cnf) '(NIL))
      t))
;;
;; EJEMPLOS:
;;
;;
;; SAT Examples
;;
;; (RES-SAT-p nil)  -> T
;; (RES-SAT-p '((p) ((¬ q)))) -> T
;; (RES-SAT-p '((a b d) ((¬ p) q) ((¬ c) a b) ((¬ b) (¬ p) d) (c d (¬ a)))) -> T
;; (RES-SAT-p '(((¬ p) (¬ q) (¬ r)) (q r) ((¬ q) p) ((¬ q)) ((¬ p) (¬ q) r))) -> T
;;
;; UNSAT Examples
;;
;; (RES-SAT-p '((P (¬ Q)) NIL (K R))) -> NIL
;; (RES-SAT-p '(nil))         -> NIL
;; (RES-SAT-p '((S) nil))     -> NIL
;; (RES-SAT-p '((p) ((¬ p)))) -> NIL
;; (RES-SAT-p '(((¬ p) (¬ q) (¬ r)) (q r) ((¬ q) p) (p) (q) ((¬ r)) ((¬ p) (¬ q) r))) -> NIL
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; (logical-consequence-RES-SAT-p '(q ^ (¬ q)) 'a) -> T
;; (logical-consequence-RES-SAT-p '(q ^ (¬ q)) '(¬ a)) -> T
;; (logical-consequence-RES-SAT-p '((p => (¬ p)) ^ p) 'q) -> T
;; (logical-consequence-RES-SAT-p '((p => (¬ p)) ^ p) '(¬ q)) -> T
;; (logical-consequence-RES-SAT-p '((p => q) ^ p) 'q) -> T
;; (logical-consequence-RES-SAT-p '((p => q) ^ p) '(¬ q)) -> NIL
;; (logical-consequence-RES-SAT-p '(((¬ p) => q) ^ (p => (a v (¬ b))) ^
;;   (p => ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) '(¬ a)) -> T
;; (logical-consequence-RES-SAT-p '(((¬ p) => q) ^ (p => (a v (¬ b))) ^
;;   (p => ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 'a) -> T
;; (logical-consequence-RES-SAT-p '(((¬ p) => q) ^ (p => ((¬ a) ^ b)) ^
;;   ( (¬ p) => (r  ^ (¬ q)))) 'a) -> NIL
;; (logical-consequence-RES-SAT-p '(((¬ p) => q) ^ (p => ((¬ a) ^ b)) ^
;;   ( (¬ p) => (r  ^ (¬ q)))) '(¬ a)) -> T
;; (logical-consequence-RES-SAT-p '(((¬ p) => q) ^ (p <=> ((¬ a) ^ b)) ^
;;   ( (¬ p) => (r  ^ (¬ q)))) 'q) -> NIL
;; (logical-consequence-RES-SAT-p '(((¬ p) => q) ^ (p <=> ((¬ a) ^ b)) ^
;;   ( (¬ p) => (r  ^ (¬ q)))) '(¬ q)) -> NIL
;; (logical-consequence-RES-SAT-p '(((¬ p) => q) ^ (p <=> ((¬ a) ^ b)) ^
;;   ( (¬ p) => (r  ^ (¬ q)))) 'q) -> NIL
;; (logical-consequence-RES-SAT-p '(((¬ p) => q) ^ (p <=> ((¬ a) ^ b)) ^
;;   ( (¬ p) => (r  ^ (¬ q)))) '(¬ q)) -> NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
