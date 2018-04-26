(defpackage :grupo02pareja033Uz2604            ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)                 ; y mancala, y exporta la función de evaluación)
  (:export :heuristica :*alias*))              ; heurística y un alias para el torneo


(in-package grupo02pareja033Uz2604)

(defun heuristica (estado)
  (let ((tablero (estado-tablero estado))
        (mi-jugador (estado-lado-sgte-jugador estado))
        (oponente (lado-contrario (estado-lado-sgte-jugador estado))))
    (if (juego-terminado-p estado)
        (if (> (suma-fila tablero mi-jugador)
               (suma-fila tablero oponente))
            361
          -361)
      (+ (* (get-fichas tablero mi-jugador 0) -2.71)
      (+ (* (get-fichas tablero mi-jugador 1) 7.15)
      (+ (* (get-fichas tablero mi-jugador 2) -4.50)
      (+ (* (get-fichas tablero mi-jugador 3) -8.55)
      (+ (* (get-fichas tablero mi-jugador 4) -8.60)
      (+ (* (get-fichas tablero mi-jugador 5) -9.25)
      (+ (* (get-fichas tablero mi-jugador 6) 4.64)
         (* (get-fichas tablero oponente 0) 3.75)
         (* (get-fichas tablero oponente 1) -0.25)
         (* (get-fichas tablero oponente 2) -0.21)
         (* (get-fichas tablero oponente 3) -0.05)
         (* (get-fichas tablero oponente 4) -0.74)
         (* (get-fichas tablero oponente 5) 9.78)
         (* (get-fichas tablero oponente 6) -8.42)

(defvar *alias* '|Marvill-1.1.5|) ; alias que aparecerá en el ranking
