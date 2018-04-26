(defpackage :grupo02pareja032Uz2604            ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)                 ; y mancala, y exporta la función de evaluación)
  (:export :heuristica :*alias*))              ; heurística y un alias para el torneo


(in-package grupo02pareja032Uz2604)

(defun heuristica (estado)
  (let ((tablero (estado-tablero estado))
        (mi-jugador (estado-lado-sgte-jugador estado))
        (oponente (lado-contrario (estado-lado-sgte-jugador estado))))
    (if (juego-terminado-p estado)
        (if (> (suma-fila tablero mi-jugador)
               (suma-fila tablero oponente))
            361
          -361)
      (+ (* (get-fichas tablero mi-jugador 0) 8.90)
      (+ (* (get-fichas tablero mi-jugador 1) 1.47)
      (+ (* (get-fichas tablero mi-jugador 2) -0.16)
      (+ (* (get-fichas tablero mi-jugador 3) -7.61)
      (+ (* (get-fichas tablero mi-jugador 4) -8.24)
      (+ (* (get-fichas tablero mi-jugador 5) -9.61)
      (+ (* (get-fichas tablero mi-jugador 6) 8.73)
         (* (get-fichas tablero oponente 0) 3.83)
         (* (get-fichas tablero oponente 1) 0.81)
         (* (get-fichas tablero oponente 2) -3.04)
         (* (get-fichas tablero oponente 3) 3.31)
         (* (get-fichas tablero oponente 4) 9.82)
         (* (get-fichas tablero oponente 5) 3.67)
         (* (get-fichas tablero oponente 6) -8.34)

(defvar *alias* '|Marvill-1.1.4|) ; alias que aparecerá en el ranking
