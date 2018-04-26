(defpackage :grupo02pareja031Uz2604            ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)                 ; y mancala, y exporta la función de evaluación)
  (:export :heuristica :*alias*))              ; heurística y un alias para el torneo


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
      (+ (* (get-fichas tablero mi-jugador 0) -0.95)
      (+ (* (get-fichas tablero mi-jugador 1) 9.28)
      (+ (* (get-fichas tablero mi-jugador 2) -7.31)
      (+ (* (get-fichas tablero mi-jugador 3) 3.22)
      (+ (* (get-fichas tablero mi-jugador 4) -0.65)
      (+ (* (get-fichas tablero mi-jugador 5) -9.25)
      (+ (* (get-fichas tablero mi-jugador 6) 8.78)
         (* (get-fichas tablero oponente 0) 9.26)
         (* (get-fichas tablero oponente 1) 4.92)
         (* (get-fichas tablero oponente 2) -5.11)
         (* (get-fichas tablero oponente 3) -6.99)
         (* (get-fichas tablero oponente 4) -1.55)
         (* (get-fichas tablero oponente 5) 8.94)
         (* (get-fichas tablero oponente 6) -7.79)

(defvar *alias* '|Marvill-1.1.3|) ; alias que aparecerá en el ranking
