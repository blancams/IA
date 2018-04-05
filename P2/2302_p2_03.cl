;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Lab assignment 2: Search
;;;    LAB GROUP:   2302
;;;    Couple:      3
;;;    Author 1:    Blanca Martin Selgas
;;;    Author 2:    Fernando Villar Gomez
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    BEGIN: Define structures
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Problem definition
;;;
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
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Node in search tree
;;;
(defstruct node
  state           ; state label
  parent          ; parent node
  action          ; action that generated the current node from its parent
  (depth 0)       ; depth in the search tree
  (g 0)           ; cost of the path from the initial state to this node
  (h 0)           ; value of the heurstic
  (f 0))          ; g + h
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Actions
;;;
(defstruct action
  name              ; Name of the operator that generated the action
  origin            ; State on which the action is applied
  final             ; State that results from the application of the action
  cost )            ; Cost of the action
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Search strategies
;;;
(defstruct strategy
  name              ; name of the search strategy
  node-compare-p)   ; boolean comparison
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    END: Define structures
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    BEGIN: Define galaxy
;;;
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
    (Katril Davion 5) (Katril Mallory 5) (Katril Sirtis 10)
    (Kentares Avalon 4) (Kentares Proserpina 12)
    (Mallory Avalon 9) (Mallory Katril 5) (Mallory Proserpina 11)
    (Proserpina Kentares 12) (Proserpina Mallory 11) (Proserpina Sirtis 9)
    (Sirtis Davion 8) (Sirtis Katril 10) (Sirtis Proserpina 9)))

(defparameter *sensors*
  '((Avalon 15) (Davion 5) (Katril 9)
    (Kentares 14) (Mallory 12) (Proserpina 7)
    (Sirtis 0)))

(defparameter *planet-origin* 'Mallory)
(defparameter *planets-destination* '(Sirtis))
(defparameter *planets-forbidden*   '(Avalon))
(defparameter *planets-mandatory*   '(Katril Proserpina))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    END: Define galaxy
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BEGIN: Exercise 1 -- Evaluation of the heuristic
;;;
;;; Returns the value of the heuristics for a given state
;;;
;;;  Input:
;;;    state: the current state (vis. the planet we are on)
;;;    sensors: a sensor list, that is a list of pairs
;;;                (state cost)
;;;             where the first element is the name of a state and the second
;;;             a number estimating the cost to reach the goal
;;;
;;;  Returns:
;;;    The cost (a number) or NIL if the state is not in the sensor list
;;;

(defun f-h-galaxy (state sensors)
  (second (assoc state sensors)))

(f-h-galaxy 'Sirtis *sensors*) ;-> 0
(f-h-galaxy 'Avalon *sensors*) ;-> 15
(f-h-galaxy 'Earth  *sensors*) ;-> NIL

;;;
;;; END: Exercise 1 -- Evaluation of the heuristic
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BEGIN: Exercise 2 -- Navigation operators
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; navigate
;;;
;;; Returns the list of actions that can be executed from one state using
;;; one operator.
;;;
;;;  Input: name : the name of the action (vis. the operator)
;;;         state: the current state (vis. the planet we are on)
;;;         holes: a holes list, that is, a list of three of a kind
;;;                where the first element is the name of a state, 
;;;                the second the name of the final state, and the third
;;;                a number estimating the cost to reach the goal.
;;;         planets-forbidden: a list containing the forbidden planets.
;;;
;;;  Returns:
;;;    A list with the possible actions or NIL if no action can be executed.
;;;
 
(defun navigate (name state holes planets-forbidden)
  (unless (null holes)
    (let ((primero      (first holes))
          (navigate-sig (navigate name state 
                                  (rest holes) planets-forbidden)))
    (if (and (equal (first primero) state)
             (not (some #'(lambda(x) (equal x (second primero)))
                        planets-forbidden)))
      (append (list (make-action :name name
                                 :origin state
                                 :final (second primero)
                                 :cost (third primero)))
              navigate-sig)
      navigate-sig))))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; navigate-white-hole
;;;
;;; Returns the list of actions that can be executed from one state using
;;; white holes.
;;;
;;;  Input: state      : the current state (vis. the planet we are on)
;;;         white-holes: a holes list, that is, a list of three of a kind
;;;                      where the first element is the name of a state, 
;;;                      the second the name of the final state, and the third
;;;                      a number estimating the cost to reach the goal.
;;;
;;;  Returns:
;;;    A list with the possible actions or NIL if no action can be executed.
;;;

(defun navigate-white-hole (state white-holes)
  (navigate 'navigate-white-hole state white-holes NIL))

(navigate-white-hole 'Kentares *white-holes*) ;->
;;; (#S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES 
;;;                                      :FINAL AVALON :COST 3)
;;;  #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES 
;;;                                      :FINAL KATRIL :COST 10)
;;;  #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES 
;;;                                      :FINAL PROSERPINA :COST 7))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; navigate-worm-hole
;;;
;;; Returns the list of actions that can be executed from one state using
;;; worm holes.
;;;
;;;  Input: state     : the current state (vis. the planet we are on)
;;;         worm-holes: a holes list, that is, a list of three of a kind
;;;                     where the first element is the name of a state, 
;;;                     the second the name of the final state, and the third
;;;                     a number estimating the cost to reach the goal.
;;;         planets-forbidden: a list containing the forbidden planets.
;;;
;;;  Returns:
;;;    A list with the possible actions or NIL if no action can be executed.
;;;

(defun navigate-worm-hole (state worm-holes planets-forbidden)
  (navigate 'navigate-worm-hole state worm-holes planets-forbidden))

(navigate-worm-hole 'Mallory *worm-holes* *planets-forbidden*) ;->
;;; (#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY 
;;;                                     :FINAL KATRIL :COST 5)
;;;  #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY 
;;;                                     :FINAL PROSERPINA :COST 11))

(navigate-worm-hole 'Mallory *worm-holes* NIL) ;->
;;; (#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY 
;;;                                     :FINAL AVALON :COST 9)
;;;  #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY
;;;                                     :FINAL KATRIL :COST 5)
;;;  #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY
;;;                                     :FINAL PROSERPINA :COST 11))
;;;

(navigate-worm-hole 'Uranus *worm-holes* *planets-forbidden*) ;->
;;; NIL
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; END: Exercise 2 -- Navigation operators
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BEGIN: Exercise 3A -- Goal test
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; get-pending-mandatory
;;;
;;; Returns the list of mandatory states that have yet to be visited.
;;;
;;;  Input: node             : the current node (vis. the planet we are on)
;;;         planets-mandatory: a list containing the names of the mandatory
;;;                            states that have to be visited.
;;;
;;;  Returns:
;;;    A list with the mandatory states not visited or NIL if all mandatory
;;;    states have been visited.

(defun get-pending-mandatory (node planets-mandatory)
  (if (or (null node)
          (null planets-mandatory))
      planets-mandatory
    (let ((state (node-state node))
          (parent (node-parent node)))
      (if (some #'(lambda (planet) (equal planet state)) 
                planets-mandatory)
          (get-pending-mandatory parent 
                                 (remove state planets-mandatory))
        (get-pending-mandatory parent 
                               planets-mandatory)))))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; f-goal-test-galaxy
;;;
;;; Evaluates if the current node is the goal.
;;;
;;;  Input: node : the current node (vis. the planet we are on)
;;;         planets-destination: a list containing the names of the 
;;;                              destination planets.
;;;         planets-mandatory  : a list containing the names of the 
;;;                              mandatory states that has to be visited.
;;;
;;;  Returns:
;;;    T if the node is the goal, NIL if it is not.
;;;

(defun f-goal-test-galaxy (node planets-destination planets-mandatory)
  (when (member (node-state node) planets-destination)
    (null (get-pending-mandatory node planets-mandatory))))

(defparameter node-01
   (make-node :state 'Avalon) )
(defparameter node-02
   (make-node :state 'Kentares :parent node-01))
(defparameter node-03
   (make-node :state 'Katril :parent node-02))
(defparameter node-04
   (make-node :state 'Kentares :parent node-03))

(f-goal-test-galaxy node-01 '(kentares urano) '(Avalon Katril)) ;-> NIL
(f-goal-test-galaxy node-02 '(kentares urano) '(Avalon Katril)) ;-> NIL
(f-goal-test-galaxy node-03 '(kentares urano) '(Avalon Katril)) ;-> NIL
(f-goal-test-galaxy node-04 '(kentares urano) '(Avalon Katril)) ;-> T

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; END: Exercise 3A -- Goal test
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BEGIN: Exercise 3B -- Equality between search states
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; pending-mandatory-p
;;;
;;; Evaluates if two paths have the same pending mandatory planets to be
;;; visited.
;;;
;;;  Input: node-1            : first node to be evaluated
;;;         node-2            : second node to be evaluated
;;;         planets-mandatory : list of planets to be visited compulsory
;;;
;;;  Returns: T if both paths have the same set of pending mandatory planets
;;;           to be visited, NIL otherwise
;;;

(defun pending-mandatory-p (node-1 node-2 planets-mandatory)
  (let ((pending-1 (get-pending-mandatory node-1 planets-mandatory))
        (pending-2 (get-pending-mandatory node-2 planets-mandatory)))
    (when (and (subsetp pending-1 pending-2 :test 'equal)
               (subsetp pending-2 pending-1 :test 'equal))
      t)))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; f-search-state-equal-galaxy
;;;
;;; Evaluates if two nodes are the same search state. Two nodes are the
;;; same search state when they have the same state and they have visited
;;; the same mandatory planets.
;;;
;;;  Input: node-1               : first node to be evaluated
;;;         node-2               : second node to be evaluated
;;;         [planets-mandatory]  : list of planets to be visited compulsory
;;;
;;;  Returns:
;;;    T if both nodes are the same search state, NIL if they are not.
;;;

(defun f-search-state-equal-galaxy (node-1 node-2 &optional planets-mandatory)
  (when (equal (node-state node-1)
               (node-state node-2))
    (or (null planets-mandatory)
        (pending-mandatory-p node-1 node-2 planets-mandatory))))

(f-search-state-equal-galaxy node-01 node-01)                  ;-> T
(f-search-state-equal-galaxy node-01 node-02)                  ;-> NIL
(f-search-state-equal-galaxy node-02 node-04)                  ;-> T

(f-search-state-equal-galaxy node-01 node-01 '(Avalon))        ;-> T
(f-search-state-equal-galaxy node-01 node-02 '(Avalon))        ;-> NIL
(f-search-state-equal-galaxy node-02 node-04 '(Avalon))        ;-> T

(f-search-state-equal-galaxy node-01 node-01 '(Avalon Katril)) ;-> T
(f-search-state-equal-galaxy node-01 node-02 '(Avalon Katril)) ;-> NIL
(f-search-state-equal-galaxy node-02 node-04 '(Avalon Katril)) ;-> NIL

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; END: Exercise 3B -- Equality between search states
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  BEGIN: Exercise 4 -- Define the galaxy structure
;;;
;;;

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
                              (f-search-state-equal-galaxy node-1 
                                                           node-2 
                                                           *planets-mandatory*))
   :operators             (list #'(lambda (state)
                                    (navigate-white-hole state 
                                                         *white-holes*))
                                #'(lambda (state)
                                    (navigate-worm-hole state 
                                                        *worm-holes* 
                                                        *planets-forbidden*)))))

;;;
;;; END: Exercise 4 -- Define the galaxy structure
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BEGIN Exercise 5: Expand node
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; expand
;;;
;;; Returns the expansion of a given node with a heuristic and a list 
;;; of operators.
;;;
;;;  Input: node     : the current node (vis. the planet we are on)
;;;         heuristic: the heuristic of the problem.
;;;         operators: list of operators that can be executed from the node.
;;;
;;;  Returns:
;;;    A list of nodes accessible from the given node by navigating using
;;;    the given operators.
;;;

(defun expand (node heuristic operators)
  (unless (null operators)
    (append (mapcar #'(lambda(x) (let* ((state (action-final x))
                                        (g     (+ (node-g node) (action-cost x)))
                                        (h     (funcall heuristic state)))
                                   (make-node :state state
                                              :parent node
                                              :action x
                                              :depth (+ (node-depth node) 1)
                                              :g g
                                              :h h
                                              :f (+ g h))))
              (funcall (first operators) (node-state node)))
            (expand node heuristic (rest operators)))))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; expand-node
;;;
;;; Returns the expansion of a node
;;;
;;;  Input: node   : the current node (vis. the planet we are on)
;;;         problem: the problem we are solving.
;;;
;;;  Returns:
;;;    A list of nodes accessible from the given node by navigating using
;;;    the operators of the problem.
;;;

(defun expand-node (node problem)
  (expand node (problem-f-h problem) (problem-operators problem)))


(defparameter node-00
  (make-node :state 'Proserpina :depth 12 :g 10 :f 20))

(defparameter lst-nodes-00
  (expand-node node-00 *galaxy-M35*))

(print lst-nodes-00) ;->
;;; (#S(NODE :STATE AVALON
;;;          :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL 
;;;                          :DEPTH 12 :G 10 :H 0 :F 20)
;;;          :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA 
;;;                            :FINAL AVALON :COST 8.6)
;;;          :DEPTH 13 :G 18.6 :H 15 :F 33.6)
;;;  #S(NODE :STATE DAVION
;;;          :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL
;;;                          :DEPTH 12 :G 10 :H 0 :F 20)
;;;          :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA
;;;                            :FINAL DAVION :COST 5)
;;;          :DEPTH 13 :G 15 :H 5 :F 20)
;;;  #S(NODE :STATE MALLORY
;;;          :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL
;;;                          :DEPTH 12 :G 10 :H 0 :F 20)
;;;          :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA
;;;                            :FINAL MALLORY :COST 15)
;;;          :DEPTH 13 :G 25 :H 12 :F 37)
;;;  #S(NODE :STATE SIRTIS
;;;          :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL
;;;                          :DEPTH 12 :G 10 :H 0 :F 20)
;;;          :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA
;;;                            :FINAL SIRTIS :COST 12)
;;;          :DEPTH 13 :G 22 :H 0 :F 22)
;;;  #S(NODE :STATE KENTARES
;;;          :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL
;;;                          :DEPTH 12 :G 10 :H 0 :F 20)
;;;          :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA
;;;                            :FINAL KENTARES :COST 12)
;;;          :DEPTH 13 :G 22 :H 14 :F 36)
;;;  #S(NODE :STATE SIRTIS
;;;          :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL
;;;                          :DEPTH 12 :G 10 :H 0 :F 20)
;;;          :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA
;;;                            :FINAL SIRTIS :COST 9)
;;;          :DEPTH 13 :G 19 :H 0 :F 19)
;;;  #S(NODE :STATE MALLORY
;;;          :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL
;;;                          :DEPTH 12 :G 10 :H 0 :F 20)
;;;          :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA
;;;                            :FINAL MALLORY :COST 11)
;;;          :DEPTH 13 :G 21 :H 12 :F 33))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; END Exercise 5: Expand node
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  BEGIN Exercise 6 -- Node list management
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; insert-sort
;;;
;;; Inserts a node in a list of nodes according to a given comparison
;;; between nodes.
;;;
;;;  Input: node      : node to be inserted.
;;;         lst-nodes : list of ordered nodes.
;;;         comparison: comparison criteria for the insertion.
;;;
;;;  Returns:
;;;    A list of ordered nodes result of the insertion of the node in 
;;;    the list of nodes.
;;;

(defun insert-sort (node lst-nodes comparison)
  (if (null lst-nodes)
      (list node)
    (if (funcall comparison node (first lst-nodes))
        (cons node
              lst-nodes)
      (cons (first lst-nodes)
            (insert-sort node (rest lst-nodes) comparison)))))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; insert-nodes-strategy
;;;
;;; Inserts a list of nodes in an ordered list of nodes following a 
;;; given strategy.
;;;
;;;  Input: nodes    : list of nodes to be inserted.
;;;         lst-nodes: list of ordered nodes.
;;;         strategy : strategy to be followed in the insertion. lst-nodes
;;;                    is also supposed to be ordered following this strategy.
;;;
;;;  Returns:
;;;    A list of ordered nodes result of the insertion.
;;;

(defun insert-nodes-strategy (nodes lst-nodes strategy)
  (if (null nodes)
      lst-nodes
    (insert-nodes-strategy (rest nodes)
                           (insert-sort (first nodes) 
                                        lst-nodes 
                                        (strategy-node-compare-p strategy))
                           strategy)))

(defparameter node-000
  (make-node :state 'Proserpina :depth 1 :g 20 :f 20) )
(defparameter node-001
  (make-node :state 'Avalon :depth 0 :g 0 :f 0) )
(defparameter node-002
  (make-node :state 'Kentares :depth 2 :g 50 :f 50) )
(defparameter lst-nodes-00
  (list
    (make-node :state 'Davion :depth 3 :g 25 :f 30)
    (make-node :state 'Mallory :depth 3 :g 10 :f 40)
    (make-node :state 'Katril :depth 3 :g 60 :f 60)))

(defun node-g-<= (node-1 node-2)
  (<= (node-g node-1)
      (node-g node-2)))

(defparameter *uniform-cost*
  (make-strategy
   :name 'uniform-cost
   :node-compare-p #'node-g-<=))

(insert-nodes-strategy '(4 8 6 2) 
                       '(1 3 5 7) 
                       (make-strategy :name 'simple :node-compare-p #'<)) ;->
;;; (1 2 3 4 5 6 7 8)

(print (insert-nodes-strategy (list node-000 node-001 node-002)
                              lst-nodes-00
                              *uniform-cost*)) ;->
;;; (#S(NODE :STATE AVALON :PARENT NIL :ACTION NIL :DEPTH 0 :G 0 :H 0 :F 0)
;;; #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 1 :G 20 :H 0 :F 20)
;;; #S(NODE :STATE MALLORY :PARENT NIL :ACTION NIL :DEPTH 3 :G 25 :H 0 :F 25)
;;; #S(NODE :STATE DAVION :PARENT NIL :ACTION NIL :DEPTH 3 :G 10 :H 0 :F 10)
;;; #S(NODE :STATE KENTARES :PARENT NIL :ACTION NIL :DEPTH 2 :G 50 :H 0 :F 50)
;;; #S(NODE :STATE KATRIL :PARENT NIL :ACTION NIL :DEPTH 3 :G 60 :H 0 :F 60))

(print
 (insert-nodes-strategy (list node-000 node-001 node-002)
                        (sort (copy-list lst-nodes-00) #'<= :key #'node-g)
                        *uniform-cost*)) ;->
;;; (#S(NODE :STATE AVALON :PARENT NIL :ACTION NIL :DEPTH 0 :G 0 :H 0 :F 0) 
;;; #S(NODE :STATE MALLORY :PARENT NIL :ACTION NIL :DEPTH 3 :G 10 :H 0 :F 25)
;;; #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 1 :G 20 :H 0 :F 20) 
;;; #S(NODE :STATE DAVION :PARENT NIL :ACTION NIL :DEPTH 3 :G 25 :H 0 :F 10)
;;; #S(NODE :STATE KENTARES :PARENT NIL :ACTION NIL :DEPTH 2 :G 50 :H 0 :F 50) 
;;; #S(NODE :STATE KATRIL :PARENT NIL :ACTION NIL :DEPTH 3 :G 60 :H 0 :F 60))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;;    END: Exercize 6 -- Node list management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BEGIN: Exercise 7 -- Definition of the A* strategy
;;;
;;; A strategy is, basically, a comparison function between nodes to tell
;;; us which nodes should be analyzed first. In the A* strategy, the first
;;; node to be analyzed is the one with the smallest value of g+h
;;;

(defparameter *A-star*
  (make-strategy
   :name 'A-star-strategy
   :node-compare-p #'(lambda (node1 node2) (< (node-f node1) 
                                              (node-f node2)))))

(print (insert-nodes-strategy (list node-000 node-001 node-002)
                              (copy-list lst-nodes-00)
                              *A-star*)) ;->
;;; (#S(NODE :STATE AVALON :PARENT NIL :ACTION NIL :DEPTH 0 :G 0 :H 0 :F 0) 
;;; #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 1 :G 20 :H 0 :F 20)
;;; #S(NODE :STATE DAVION :PARENT NIL :ACTION NIL :DEPTH 3 :G 25 :H 0 :F 30) 
;;; #S(NODE :STATE MALLORY :PARENT NIL :ACTION NIL :DEPTH 3 :G 10 :H 0 :F 40)
;;; #S(NODE :STATE KENTARES :PARENT NIL :ACTION NIL :DEPTH 2 :G 50 :H 0 :F 50)
;;; #S(NODE :STATE KATRIL :PARENT NIL :ACTION NIL :DEPTH 3 :G 60 :H 0 :F 60))

;;;
;;; END: Exercise 7 -- Definition of the A* strategy
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    BEGIN Exercise 8: Search algorithm
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; graph-search-aux
;;;
;;; Returns the path (node) after applying a graph search of a given problem
;;; with a given strategy, using a list of open nodes and a list of closed
;;; nodes prearranged.
;;;
;;;  Input: problem:       problem to be solved
;;;         strategy:      strategy to be followed
;;;         open-nodes:    list of prearranged open nodes
;;;         closed-nodes:  list of prearranged closed nodes
;;;
;;;  Returns:
;;;         Node that contains the path of the result after applying graph
;;;         search with a certain strategy and initial list of open and
;;;         closed nodes.

(defun graph-search-aux (problem strategy open-nodes closed-nodes)
  (unless (null open-nodes)
    (let ((node (first open-nodes)))
      (if (funcall (problem-f-goal-test problem) node)
          node
        (let ((equal-closed-node 
               (find node 
                     closed-nodes 
                     :test #'(lambda (x y) 
                               (funcall (problem-f-search-state-equal problem) 
                                        x y)))))
          (if (or (null equal-closed-node) 
                  (< (node-g node) (node-g equal-closed-node))) 
              (graph-search-aux problem
                                strategy
                                (insert-nodes-strategy (expand-node node problem) 
                                                       (rest open-nodes) 
                                                       strategy)
                                (cons node closed-nodes))          
            (graph-search-aux problem
                              strategy
                              (rest open-nodes)
                              closed-nodes)))))))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun graph-search (problem strategy)
  (let* ((root (problem-initial-state problem))
         (root-h (funcall (problem-f-h problem) root))
         (open-nodes (list (make-node :state root :h root-h :f root-h)))
         (closed-nodes '()))
    (graph-search-aux problem strategy open-nodes closed-nodes)))

;;;
;;;  Solve a problem using the A* strategy
;;;
(defun a-star-search (problem)
  (graph-search problem *A-star*))

(graph-search *galaxy-M35* *A-star*) ;->
;;; #S(NODE :STATE ...
;;;         :PARENT #S(NODE :STATE ...
;;;                         :PARENT #S(NODE :STATE ...))
;;;

(print (a-star-search *galaxy-M35*)) ;->
;;; #S(NODE :STATE ...
;;;         :PARENT #S(NODE :STATE ...
;;;                         :PARENT #S(NODE :STATE ...))

;;;
;;;    END Exercise 8: Search algorithm
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    BEGIN Exercise 9: Solution path / action sequence
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; solution-path
;;;
;;; Returns the list of planets (states) of a path given by a node
;;;
;;;  Input: node: node to be evaluated
;;;
;;;  Returns: list of planets from root to leaf of the path given by the node
;;;

(defun solution-path (node)
  (unless (null node)
    (append (solution-path (node-parent node)) (list (node-state node)))))

(solution-path nil) ;-> 
;;; NIL
(solution-path (a-star-search *galaxy-M35*)) ;->
;;; (MALLORY KATRIL DAVION PROSERPINA SIRTIS)

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; action-sequence
;;;
;;; Returns the list of actions taken in a path given by a node
;;;
;;;  Input: node: node to be evaluated
;;;
;;;  Returns: list of actions from root to leaf in the path given by the node
;;;

(defun action-sequence (node)
  (unless (null (node-parent node))
    (append (action-sequence (node-parent node)) (list (node-action node)))))

(action-sequence (a-star-search *galaxy-M35*)) ;->
;;; (#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN KATRIL :FINAL DAVION :COST 5) 
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN DAVION :FINAL PROSERPINA :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 9))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;;    END Exercise 9: Solution path / action sequence
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    BEGIN Exercise 10: depth-first / breadth-first
;;;

(defun depth-first-node-compare-p (node-1 node-2)
  (> (node-depth node-1) (node-depth node-2)))

(defparameter *depth-first*
  (make-strategy
   :name 'depth-first
   :node-compare-p #'depth-first-node-compare-p))


(solution-path (graph-search *galaxy-M35* *depth-first*)) ;->
;;; (MALLORY KATRIL DAVION PROSERPINA AVALON MALLORY KATRIL DAVION SIRTIS)

(action-sequence (graph-search *galaxy-M35* *depth-first*)) ;->
;;; (#S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 10)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KATRIL :FINAL DAVION :COST 9)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN DAVION :FINAL PROSERPINA :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL AVALON :COST 8.6)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN AVALON :FINAL MALLORY :COST 6.4)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 10)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KATRIL :FINAL DAVION :COST 9)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN DAVION :FINAL SIRTIS :COST 6))

(defun breadth-first-node-compare-p (node-1 node-2)
  (< (node-depth node-1) (node-depth node-2)))

(defparameter *breadth-first*
  (make-strategy
   :name 'breadth-first
   :node-compare-p #'breadth-first-node-compare-p))

(solution-path (graph-search *galaxy-M35* *breadth-first*)) ;->
;;; (MALLORY KATRIL DAVION PROSERPINA SIRTIS)

(action-sequence (graph-search *galaxy-M35* *breadth-first*)) ;->
;;; (#S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 10)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KATRIL :FINAL DAVION :COST 9)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN DAVION :FINAL PROSERPINA :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 12))

;;;
;;;    END Exercise 10: depth-first / breadth-first
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
