(defpackage :grupo02pareja031Uz2604            ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)                 ; y mancala, y exporta la funcion de evaluacion
  (:export :heuristica :*alias*))              ; heuristica y un alias para el torneo


(in-package grupo02pareja031Uz2604)

(defun heuristica (estado)
  (let ((tablero (estado-tablero estado))
        (mi-jugador (estado-lado-sgte-jugador estado))
        (oponente (lado-contrario (estado-lado-sgte-jugador estado))))
    (if (juego-terminado-p estado)
        (if (> (suma-fila tablero mi-jugador)
               (suma-fila tablero oponente))
            361
          -361)
      (+ (* (get-fichas tablero mi-jugador 0) -7.47)
         (* (get-fichas tablero mi-jugador 1) -2.99)
         (* (get-fichas tablero mi-jugador 2) -0.50)
         (* (get-fichas tablero mi-jugador 3) -2.81)
         (* (get-fichas tablero mi-jugador 4) -8.33)
         (* (get-fichas tablero mi-jugador 5) -7.03)
         (* (get-fichas tablero mi-jugador 6) 1.86)
         (* (get-fichas tablero oponente 0) -2.61)
         (* (get-fichas tablero oponente 1) 5.30)
         (* (get-fichas tablero oponente 2) 3.74)
         (* (get-fichas tablero oponente 3) -5.91)
         (* (get-fichas tablero oponente 4) 6.95)
         (* (get-fichas tablero oponente 5) -2.53)
         (* (get-fichas tablero oponente 6) -9.64)))))

(defvar *alias* '|Marvill-1.1.3|) ; alias que aparecera en el ranking
