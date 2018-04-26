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
      (+ (* (get-fichas tablero mi-jugador 0) 1.91)
      (+ (* (get-fichas tablero mi-jugador 1) 3.82)
      (+ (* (get-fichas tablero mi-jugador 2) 2.60)
      (+ (* (get-fichas tablero mi-jugador 3) -1.99)
      (+ (* (get-fichas tablero mi-jugador 4) -4.80)
      (+ (* (get-fichas tablero mi-jugador 5) -7.92)
      (+ (* (get-fichas tablero mi-jugador 6) 7.04)
         (* (get-fichas tablero oponente 0) 6.13)
         (* (get-fichas tablero oponente 1) 6.89)
         (* (get-fichas tablero oponente 2) 7.62)
         (* (get-fichas tablero oponente 3) 9.44)
         (* (get-fichas tablero oponente 4) 9.33)
         (* (get-fichas tablero oponente 5) -1.38)
         (* (get-fichas tablero oponente 6) -9.28)

(defvar *alias* '|Marvill-1.1.5|) ; alias que aparecerá en el ranking
