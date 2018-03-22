;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Lab assignment 2: Search
;;    LAB GROUP:
;;    Couple:
;;    Author 1:
;;    Author 2:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Problem definition
;;
(defstruct problem
  states               ; List of states
  initial-state        ; Initial state
  f-goal-test          ; reference to a function that determines whether
                       ; a state fulfills the goal
  f-h                  ; reference to a function that evaluates to the
                       ; value of the heuristic of a state
  f-search-state-equal ; reference to a predicate that determines whether
                       ; two nodes are equal, in terms of their search state
  operators)           ; list of operators (references to functions) to generate succesors
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Node in search tree
;;
(defstruct node
  state           ; state label
  parent          ; parent node
  action          ; action that generated the current node from its parent
  (depth 0)       ; depth in the search tree
  (g 0)           ; cost of the path from the initial state to this node
  (h 0)           ; value of the heurstic
  (f 0))          ; g + h
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Actions
;;
(defstruct action
  name              ; Name of the operator that generated the action
  origin            ; State on which the action is applied
  final             ; State that results from the application of the action
  cost )            ; Cost of the action
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Search strategies
;;
(defstruct strategy
  name              ; name of the search strategy
  node-compare-p)   ; boolean comparison
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    END: Define structures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    BEGIN: Define galaxy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *planets* '(Avalon Davion Katril Kentares Mallory Proserpina Sirtis))

(defparameter *white-holes*
  '((Avalon Mallory 6.4) (Avalon Proserpina 8.6)
    (Davion Proserpina 5) (Davion Sirtis 6)
    (Katril Davion 9) (Katril Mallory 10)
    (Kentares Avalon 3) (Kentares Katril 10) (Kentares Proserpina 7)
    (Mallory Katril 10) (Mallory Proserpina 15)
    (Proserpina Avalon 8.6) (Proserpina Davion 5) (Proserpina Mallory 15) (Proserpina Sirtis 12)
    (Sirtis Davion 6) (Sirtis Proserpina 12)))

(defparameter *worm-holes*
  '((Avalon Kentares 4) (Avalon Mallory 9)
    (Davion Katril 5) (Davion Sirtis 8)
    (Katril Mallory 5) (Katril Davion 5) (Katril Sirtis 10)
    (Kentares Avalon 4) (Kentares Proserpina 12)
    (Mallory Avalon 9) (Mallory Katril 5) (Mallory Proserpina 11)
    (Proserpina Kentares 12) (Proserpina Sirtis 9) (Proserpina Mallory 11)
    (Sirtis Davion 8) (Sirtis Katril 10) (Sirtis Proserpina 9)))

(defparameter *sensors*
  '((Avalon 15) (Davion 5) (Katril 9)
    (Kentares 14) (Mallory 12) (Proserpina 7)
    (Sirtis 0)))

(defparameter *planet-origin* 'Katril)
(defparameter *planets-destination* '(Sirtis))
(defparameter *planets-forbidden*   '(Avalon))
(defparameter *planets-mandatory*   '(Kentares Proserpina Mallory))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 1 -- Evaluation of the heuristic
;;
;; Returns the value of the heuristics for a given state
;;
;;  Input:
;;    state: the current state (vis. the planet we are on)
;;    sensors: a sensor list, that is a list of pairs
;;                (state cost)
;;             where the first element is the name of a state and the second
;;             a number estimating the cost to reach the goal
;;
;;  Returns:
;;    The cost (a number) or NIL if the state is not in the sensor list
;;
(defun f-h-galaxy (state sensors)
  (second (assoc state sensors)))

(f-h-galaxy 'Sirtis *sensors*) ;-> 0
(f-h-galaxy 'Avalon *sensors*) ;-> 15
(f-h-galaxy 'Earth  *sensors*) ;-> NIL


;;
;; END: Exercise 1 -- Evaluation of the heuristic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 2 -- Navigation operators
;;
(defun navigate (name state holes planets-forbidden)
  (unless (null holes)
    (let ((primero      (first holes))
          (navigate-sig (navigate name state (rest holes) planets-forbidden)))
    (if (and (equal (first primero) state)
             (not (some #'(lambda(x) (equal x (second primero))) planets-forbidden)))
      (append (list (make-action :name name
                                 :origin state
                                 :final (second primero)
                                 :cost (third primero)))
              navigate-sig)
      navigate-sig))))

(defun navigate-white-hole (state white-holes)
  ; (unless (null white-holes)
  ;   (if (equal (first (first white-holes)) state)
  ;     (append (list (make-action :name 'navigate-white-hole
  ;                          :origin state
  ;                          :final (second (first white-holes))
  ;                          :cost (third (first white-holes))))
  ;             (navigate-white-hole state (rest white-holes)))
  ;     (navigate-white-hole state (rest white-holes)))))
  (navigate 'navigate-white-hole state white-holes NIL))

(defun navigate-worm-hole (state worm-holes planets-forbidden)
  ; (unless (null worm-holes)
  ;   (if (and (equal (first (first worm-holes)) state) (not (some #'(lambda(x) (equal x (second (first worm-holes)))) planets-forbidden)))
  ;     (append (list (make-action :name 'navigate-worm-hole
  ;                          :origin state
  ;                          :final (second (first worm-holes))
  ;                          :cost (third (first worm-holes))))
  ;             (navigate-worm-hole state (rest worm-holes) planets-forbidden))
  ;     (navigate-worm-hole state (rest worm-holes) planets-forbidden))))
  (navigate 'navigate-worm-hole state worm-holes planets-forbidden))


(navigate-worm-hole 'Mallory *worm-holes* *planets-forbidden*)  ;->
;;;(#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL PROSERPINA :COST 11))

(navigate-worm-hole 'Mallory *worm-holes* NIL)  ;->
;;;(#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL AVALON :COST 9)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL PROSERPINA :COST 11))


(navigate-white-hole 'Kentares *white-holes*) ;->
;;;(#S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL AVALON :COST 3)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL KATRIL :COST 10)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL PROSERPINA :COST 7))


(navigate-worm-hole 'Uranus *worm-holes* *planets-forbidden*)  ;-> NIL


;;
;; END: Exercise 2 -- Navigation operators
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 3A -- Goal test
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FEA DE COJONES LA FUNCION ;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-mandatory (node planets-mandatory)
  (if (null planets-mandatory) ; los voy eliminando asi que si me quedo sin ellos es bien
    t
    (unless (null node) ; si me quedo sin nodos antes es malo
      (let ((state  (node-state node))
            (parent (node-parent node)))
      (if (some #'(lambda(x) (equal x state))
                 planets-mandatory)
          (check-mandatory parent (remove state planets-mandatory)) ; elimino el planeta si ya he pasado por el
          (check-mandatory parent planets-mandatory)))))) ; y si no pues a seguir intentandolo

(defun f-goal-test-galaxy (node planets-destination planets-mandatory)
  (when (some #'(lambda(x) (equal x (node-state node)))
              planets-destination) ; para saber que hemos llegado a la meta al menos
    (check-mandatory node (copy-list planets-mandatory)))) ; lo he separado y es feo pero lo he hecho rapido

(defparameter node-01
   (make-node :state 'Avalon) )
(defparameter node-02
   (make-node :state 'Kentares :parent node-01))
(defparameter node-03
   (make-node :state 'Katril :parent node-02))
(defparameter node-04
   (make-node :state 'Kentares :parent node-03))

(f-goal-test-galaxy node-01 '(kentares urano) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-02 '(kentares urano) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-03 '(kentares urano) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-04 '(kentares urano) '(Avalon Katril)); -> T
;;
;; END: Exercise 3A -- Goal test
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 3B -- Equality between search states
;;
(defun pending-mandatory-planets (node planets-mandatory)
    (if (or (null node) (null planets-mandatory))
        planets-mandatory
        (let ((state (node-state node))
              (parent (node-parent node)))
            (if (some #'(lambda (planet) (equal planet state)) planets-mandatory)
                    (pending-mandatory-planets parent (remove state planets-mandatory))
                (pending-mandatory-planets parent planets-mandatory)))))

(defun pending-mandatory-planets-p (node-1 node-2 planets-mandatory)
    (let ((pending-1 (pending-mandatory-planets node-1 planets-mandatory))
          (pending-2 (pending-mandatory-planets node-2 planets-mandatory)))
        (when (and (subsetp pending-1 pending-2 :test 'equal)
                   (subsetp pending-2 pending-1 :test 'equal))
            t)))

(defun f-search-state-equal-galaxy (node-1 node-2 &optional planets-mandatory)
    (let ((equal-state (equal (node-state node-1)
                              (node-state node-2))))
        (if (null planets-mandatory)
              equal-state
            (and equal-state
                 (pending-mandatory-planets-p node-1
                                              node-2
                                              planets-mandatory)))))
(f-search-state-equal-galaxy node-01 node-01) ;-> T
(f-search-state-equal-galaxy node-01 node-02) ;-> NIL
(f-search-state-equal-galaxy node-02 node-04) ;-> T

(f-search-state-equal-galaxy node-01 node-01 '(Avalon)) ;-> T
(f-search-state-equal-galaxy node-01 node-02 '(Avalon)) ;-> NIL
(f-search-state-equal-galaxy node-02 node-04 '(Avalon)) ;-> T

(f-search-state-equal-galaxy node-01 node-01 '(Avalon Katril)) ;-> T
(f-search-state-equal-galaxy node-01 node-02 '(Avalon Katril)) ;-> NIL
(f-search-state-equal-galaxy node-02 node-04 '(Avalon Katril)) ;-> NIL
;;
;; END: Exercise 3B -- Equality between search states
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BEGIN: Exercise 4 -- Define the galaxy structure
;;
;;
(defparameter *galaxy-M35*
  (make-problem
   :states                *planets*
   :initial-state         *planet-origin*
   :f-goal-test           #'(lambda (node)
                              (f-goal-test-galaxy node *planets-destination*
                                                       *planets-mandatory*))
   :f-h                   #'(lambda(state)
                            (f-h-galaxy state *sensors*))
   :f-search-state-equal  #'(lambda(node-1 node-2)
                            (f-search-state-equal-galaxy node-1 node-2 *planets-mandatory*))
   :operators             (list #'(lambda (state)
                                    (navigate-white-hole state *white-holes*))
                                #'(lambda (state)
                                    (navigate-worm-hole state *worm-holes* *planets-forbidden*)))))

;;
;;  END: Exercise 4 -- Define the galaxy structure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN Exercise 5: Expand node
;;

;;;;;;;;;; Lo he tenido que separar en dos porque si no no se me ocurria otra
; forma que no fuese un mapcar de un mapcar y temia muerte asegurada a manos de
; Alberto.
(defun expand (node problem actions)
  (unless (null actions)
    (append (mapcar #'(lambda(x) (let* ((state (action-final x))
                                        (g     (+ (node-g node) (action-cost x)))
                                        (h     (funcall (problem-f-h problem) state)))
                                   (make-node :state state
                                              :parent node
                                              :action x
                                              :depth (+ (node-depth node) 1)
                                              :g g
                                              :h h
                                              :f (+ g h))))
                    (funcall (first actions) (node-state node)))
            (expand node problem (rest actions)))))

(defun expand-node (node problem)
  (expand node problem (problem-operators problem)))


(defparameter node-00
  (make-node :state 'Proserpina :depth 12 :g 10 :f 20))

(defparameter lst-nodes-00
  (expand-node node-00 *galaxy-M35*))

(print lst-nodes-00)

;;;(#S(NODE :STATE AVALON
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL AVALON :COST 8.6)
;;;         :DEPTH 13 :G 18.6 :H 15 :F 33.6)
;;; #S(NODE :STATE DAVION
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL DAVION :COST 5)
;;;         :DEPTH 13 :G 15 :H 5 :F 20)
;;; #S(NODE :STATE MALLORY
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 15)
;;;         :DEPTH 13 :G 25 :H 12 :F 37)
;;; #S(NODE :STATE SIRTIS
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 12)
;;;         :DEPTH 13 :G 22 :H 0 :F 22)
;;; #S(NODE :STATE KENTARES
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL KENTARES :COST 12)
;;;         :DEPTH 13 :G 22 :H 14 :F 36)
;;; #S(NODE :STATE SIRTIS
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 9)
;;;         :DEPTH 13 :G 19 :H 0 :F 19)
;;; #S(NODE :STATE MALLORY
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 11)
;;;         :DEPTH 13 :G 21 :H 12 :F 33))
;;
;; END Exercise 5: Expand node
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  BEGIN Exercise 6 -- Node list management
;;;
(defun insert-sort (node lst-nodes comparison)
  (if (null lst-nodes)
    (list node)
    (if (funcall comparison node (first lst-nodes))
      (cons node
            lst-nodes)
      (cons (first lst-nodes)
            (insert-sort node (rest lst-nodes) comparison)))))

(defun insert-nodes-strategy (nodes lst-nodes strategy)
    (if (null nodes)
      lst-nodes
      (insert-nodes-strategy (rest nodes)
                             (insert-sort (first nodes) lst-nodes (strategy-node-compare-p strategy))
                             strategy)))

(defparameter node-000
   (make-node :state 'Proserpina :depth 1 :g 20 :f 20) )
(defparameter node-001
   (make-node :state 'Avalon :depth 0 :g 0 :f 0) )
(defparameter node-002
   (make-node :state 'Kentares :depth 2 :g 50 :f 50) )
(defparameter lst-nodes-00
   (list
      (make-node :state 'Davion :depth 3 :g 10 :f 10)
      (make-node :state 'Mallory :depth 3 :g 25 :f 25)
      (make-node :state 'Katril :depth 3 :g 60 :f 60)))

(insert-nodes-strategy '(4 8 6 2) '(1 3 5 7) (make-strategy :name 'simple :node-compare-p #'<))

; (print (insert-nodes-strategy (list node-000 node-001 node-002)
;                         lst-nodes-00
;                         *uniform-cost*));->
;;;
;;;(#S(NODE :STATE AVALON :PARENT NIL :ACTION NIL :DEPTH 0 :G 0 :H 0 :F 0)
;;; #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; #S(NODE :STATE AVALON :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL AVALON :COST 12) :DEPTH 13 :G 22 :H 5 :F 27)
;;; #S(NODE :STATE DAVION :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL DAVION :COST 14) :DEPTH 13 :G 24 :H 1 :F 25)
;;; #S(NODE :STATE MALLORY :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 17) :DEPTH 13 :G 27 :H 7 :F 34)
;;; #S(NODE :STATE SIRTIS :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 10) :DEPTH 13 :G 20 :H 0 :F 20)
;;; #S(NODE :STATE KENTARES :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL KENTARES :COST 21) :DEPTH 13 :G 31 :H 4 :F 35)
;;; #S(NODE :STATE MALLORY :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 16) :DEPTH 13 :G 26 :H 7 :F 33)
;;; #S(NODE :STATE SIRTIS :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 7) :DEPTH 13 :G 17 :H 0 :F 17)
;;; #S(NODE :STATE KENTARES :PARENT NIL :ACTION NIL :DEPTH 2 :G 50 :H 0 :F 50))


; (print
;  (insert-nodes-strategy (list node-000 node-001 node-002)
;                         (sort (copy-list lst-nodes-00) #'<= :key #'node-g)
;                         *uniform-cost*));->
;;;
;;;(#S(NODE :STATE AVALON :PARENT NIL :ACTION NIL :DEPTH 0 :G 0 :H 0 :F 0)
;;; #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; #S(NODE :STATE SIRTIS :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 7) :DEPTH 13 :G 17 :H 0 :F 17)
;;; #S(NODE :STATE SIRTIS :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 10) :DEPTH 13 :G 20 :H 0 :F 20)
;;; #S(NODE :STATE AVALON :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL AVALON :COST 12) :DEPTH 13 :G 22 :H 5 :F 27)
;;; #S(NODE :STATE DAVION :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL DAVION :COST 14) :DEPTH 13 :G 24 :H 1 :F 25)
;;; #S(NODE :STATE MALLORY :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 16) :DEPTH 13 :G 26 :H 7 :F 33)
;;; #S(NODE :STATE MALLORY :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 17) :DEPTH 13 :G 27 :H 7 :F 34)
;;; #S(NODE :STATE KENTARES :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL KENTARES :COST 21) :DEPTH 13 :G 31 :H 4 :F 35)
;;; #S(NODE :STATE KENTARES :PARENT NIL :ACTION NIL :DEPTH 2 :G 50 :H 0 :F 50))


;;
;;    END: Exercize 6 -- Node list management
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 7 -- Definition of the A* strategy
;;
;; A strategy is, basically, a comparison function between nodes to tell
;; us which nodes should be analyzed first. In the A* strategy, the first
;; node to be analyzed is the one with the smallest value of g+h
;;

(defparameter *A-star*
  (make-strategy
    :name 'A-star-strategy
    :node-compare-p #'(lambda (node1 node2) (< (node-f node1) (node-f node2)))))

;;
;; END: Exercise 7 -- Definition of the A* strategy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(insert-nodes-strategy (list node-000 node-001 node-002)
                       (copy-list lst-nodes-00)
                       *A-star*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    BEGIN Exercise 8: Search algorithm
;;;
(defun get-mandatory-node-in-path (node planet-mandatory)
    (unless (null node)
        (if (equal (node-state node) planet-mandatory)
            (list planet-mandatory)
            (get-mandatory-node-in-path (node-parent node) planet-mandatory))))


(defun get-mandatory-nodes-in-path (node planets-mandatory)
    (unless (null planets-mandatory)
        (append (get-mandatory-node-in-path node (first planets-mandatory))
                (get-mandatory-nodes-in-path node (rest planets-mandatory)))))


(defun node-in-closed-list-p (node closed-nodes planets-mandatory)
    (if (null closed-nodes)
        t
        (if (and (equal (node-state node)
                        (node-state (first closed-nodes)))
                 (equal (get-mandatory-nodes-in-path node planets-mandatory)
                        (get-mandatory-nodes-in-path (first closed-nodes) planets-mandatory)))
            (unless (>= (node-g node) (node-g (first closed-nodes)))
                t)
            (node-in-closed-list-p node (rest closed-nodes) planets-mandatory))))

(defun graph-search-aux (problem strategy open-nodes closed-nodes planets-mandatory)
    (unless (null open-nodes)
        (let ((node (first open-nodes)))
            (if (funcall (problem-f-goal-test problem) node)
                node
                (if (node-in-closed-list-p node closed-nodes planets-mandatory)
                    (graph-search-aux problem
                                      strategy
                                      (insert-nodes-strategy (expand-node node problem) (rest open-nodes) strategy)
                                      (cons node (remove-if #'(lambda (n) (equal (node-state node) (node-state n))) closed-nodes))
                                      planets-mandatory)
                    (graph-search-aux problem
                                      strategy
                                      (rest open-nodes)
                                      closed-nodes
                                      planets-mandatory))))))

(defun graph-search (problem strategy planets-mandatory)
    (let* ((root (problem-initial-state problem))
           (root-h (funcall (problem-f-h problem) root))
           (open-nodes (list (make-node :state root :h root-h :f root-h)))
           (closed-nodes '()))
        (graph-search-aux problem strategy open-nodes closed-nodes planets-mandatory)))



;
;  Solve a problem using the A* strategy
;
(defun a-star-search (problem)...)


(graph-search *galaxy-M35* *A-star*);->
;;;#S(NODE :STATE ...
;;;        :PARENT #S(NODE :STATE ...
;;;                        :PARENT #S(NODE :STATE ...))


(print (a-star-search *galaxy-M35*));->
;;;#S(NODE :STATE ...
;;;        :PARENT #S(NODE :STATE ...
;;;                        :PARENT #S(NODE :STATE ...))


;;;
;;;    END Exercise 8: Search algorithm
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    BEGIN Exercise 9: Solution path / action sequence
;;;
(defun solution-path (node)
  )

(solution-path nil) ;;; -> NIL
(solution-path (a-star-search *galaxy-M35*))  ;;;-> (MALLORY ...)

(defun action-sequence-aux (node)
  )

(action-sequence (a-star-search *galaxy-M35*))
;;; ->
;;;(#S(ACTION :NAME ...))

;;;
;;;    END Exercise 9: Solution path / action sequence
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    BEGIN Exercise 10: depth-first / breadth-first
;;;

(defparameter *depth-first*
  (make-strategy
   :name 'depth-first
   :node-compare-p #'depth-first-node-compare-p))

(defun depth-first-node-compare-p (node-1 node-2)
  )

(solution-path (graph-search *galaxy-M35* *depth-first*))
;;; -> (MALLORY ... )

(defparameter *breadth-first*
  (make-strategy
   :name 'breadth-first
   :node-compare-p #'breadth-first-node-compare-p))

(defun breadth-first-node-compare-p (node-1 node-2)
  )

(solution-path (graph-search *galaxy-M35* *breadth-first*))
;; -> (MALLORY ... )

;;;
;;;    END Exercise 10: depth-first / breadth-first
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
