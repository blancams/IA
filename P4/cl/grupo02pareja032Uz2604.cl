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
      (+ (* (get-fichas tablero mi-jugador 0) 4.01)
      (+ (* (get-fichas tablero mi-jugador 1) 2.69)
      (+ (* (get-fichas tablero mi-jugador 2) -0.85)
      (+ (* (get-fichas tablero mi-jugador 3) -8.38)
      (+ (* (get-fichas tablero mi-jugador 4) -5.43)
      (+ (* (get-fichas tablero mi-jugador 5) -8.51)
      (+ (* (get-fichas tablero mi-jugador 6) 9.08)
         (* (get-fichas tablero oponente 0) 8.35)
         (* (get-fichas tablero oponente 1) 4.04)
         (* (get-fichas tablero oponente 2) 9.28)
         (* (get-fichas tablero oponente 3) -3.27)
         (* (get-fichas tablero oponente 4) 7.08)
         (* (get-fichas tablero oponente 5) 4.14)
         (* (get-fichas tablero oponente 6) -6.55)

(defvar *alias* '|Marvill-1.1.4|) ; alias que aparecerá en el ranking
