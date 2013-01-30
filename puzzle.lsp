(load "search.lsp")
(load "queue.lsp")

;global variable to count all the expanded nodes
(defvar *nodes-expanded* 0)

(defvar *total-steps* 0)
(defvar *minimum* 7)

(defvar *goal-state* nil)

(defvar *puzzle-dimension* 3)
(defvar *puzzle-spaces* (* *puzzle-dimension* *puzzle-dimension*))

(defstruct puzzle-state board)

(defun 8-puzzle (start-state heuristic)
	(setf *nodes-expanded* 0)
	(append (general-search (list-to-state start-state) #'successor #'goalp heuristic) (list *nodes-expanded*))
)

(defun random-case ()
	(get-solvable-states *goal-state*)
)

;create a state representation of the board, which is an array based on the puzzle-dimension (i.e. 3)
(defun get-puzzle-state() 
	(make-puzzle-state :board (make-array (list *puzzle-dimension* *puzzle-dimension*) 
						:adjustable nil :element-type 'integer)))

;convert a list to a state
(defun list-to-state (puzzle-list)
	(let ((new-state (get-puzzle-state)))
		(dotimes (y *puzzle-dimension*)
			(dotimes (x *puzzle-dimension*)
				(let ((n (+ (* x *puzzle-dimension*) y)))
					(set-puzzle-space new-state x y (nth n puzzle-list)))))
		new-state))

;convert the state representation (3x3 array) into a list
(defun state-to-list (state)
	(let ((board nil))
	(dotimes (x *puzzle-dimension*)
		(dotimes (y *puzzle-dimension*)
				(setq board (cons (aref state x y) board))))
	(list (reverse board)))
)
;Set a certain space on the board to have a specific piece
(defun set-puzzle-space (puzzle x y piece)
	(setf (aref (puzzle-state-board puzzle) x y) piece))

(defun increment-nodes-expanded() (incf *nodes-expanded*))

;This tests to see if the current state is the goal state
(defun goalp (current-puzzle)
	(equal-states current-puzzle *goal-state*)
)

;This tests to see if two states are equal
(defun equal-states (puzzle-one puzzle-two)
	(let ((equality 0))
		(dotimes (y *puzzle-dimension*)
			(dotimes (x *puzzle-dimension*)
				(if (eql (get-position-value puzzle-one x y) (get-position-value puzzle-two x y)) 
					(setq equality (1+ equality)))))
		(eql equality 9))
)

;Get the value of the square in a specific position on the board
(defun get-position-value (puzzle x y)
	(aref (puzzle-state-board puzzle) x y)
)

;This function generates the manhattan distance heuristic
(defun manhattan (puzzle-space)			
	(let ((distance 0))
		(dotimes (y *puzzle-dimension*)
			(dotimes (x *puzzle-dimension*)
				(if(not (eql (get-position-value puzzle-space x y) 0))
					(setq distance (+ distance (+ (abs (- x (find-x-coord (get-position-value puzzle-space x y) *goal-state*))) 
									(abs (- y (find-y-coord (get-position-value puzzle-space x y) *goal-state*)))))))))
		distance)
)

;This function generates the misplaced tiles heuristic distance
(defun misplaced (puzzle-space)
	(let ((distance 0))
		(dotimes (y *puzzle-dimension*)
			(dotimes (x *puzzle-dimension*)
				(if (and (not (eql (get-position-value puzzle-space x y) (get-position-value *goal-state* x y)))
					(not (eql (get-position-value puzzle-space x y) 0)))
					(setq distance (1+ distance)))))
		distance)
)

;Linear conflict heuristic
(defun linear-conflict (puzzle-space)
	(let ((linear-cost 0)
		   (square-one (aref (puzzle-state-board puzzle-space) 0 0))
		   (square-two (aref (puzzle-state-board puzzle-space) 0 1))
		   (square-three (aref (puzzle-state-board puzzle-space) 0 2))
		   (square-four (aref (puzzle-state-board puzzle-space) 1 0))
		   (square-five (aref (puzzle-state-board puzzle-space) 1 1))
		   (square-six (aref (puzzle-state-board puzzle-space) 1 2))
		   (square-seven (aref (puzzle-state-board puzzle-space) 2 0))
		   (square-eight (aref (puzzle-state-board puzzle-space) 2 1))
		   (square-nine (aref (puzzle-state-board puzzle-space) 2 2)))
		(progn
			(if (and (equal (find-x-coord square-one puzzle-space) (find-x-coord square-one *goal-state*)) 
					 (equal (find-x-coord square-two puzzle-space) (find-x-coord square-two *goal-state*)) 
					 (< square-two square-one)
				)
				(unless (or (equal square-one 0) (equal square-two 0))
						(setf linear-cost (+ linear-cost 2)))
			)
			(if (and (equal (find-x-coord square-two puzzle-space) (find-x-coord square-two *goal-state*)) 
					 (equal (find-x-coord square-three puzzle-space) (find-x-coord square-three *goal-state*))					 
					 (< square-three square-two)
				)
				(unless (or (equal square-two 0) (equal square-three 0))
						(setf linear-cost (+ linear-cost 2)))
			)
			(if (and (equal (find-x-coord square-four puzzle-space) (find-x-coord square-four *goal-state*)) 
					 (equal (find-x-coord square-five puzzle-space) (find-x-coord square-five *goal-state*))					 
					 (< square-five square-four)
				)
				(unless (or (equal square-four 0) (equal square-five 0))
						(setf linear-cost (+ linear-cost 2)))
			)
			(if (and (equal (find-x-coord square-five puzzle-space) (find-x-coord square-five *goal-state*)) 
					 (equal (find-x-coord square-six puzzle-space) (find-x-coord square-six *goal-state*)) 
					 (< square-six square-five)
				)
				(unless (or (equal square-five 0) (equal square-six 0))
						(setf linear-cost (+ linear-cost 2)))
			)
			(if (and (equal (find-x-coord square-seven puzzle-space) (find-x-coord square-seven *goal-state*)) 
					 (equal (find-x-coord square-eight puzzle-space) (find-x-coord square-eight *goal-state*)) 
					 (< square-eight square-seven)
				)
				(unless (or (equal square-seven 0) (equal square-eight 0))
						(setf linear-cost (+ linear-cost 2)))
			)
			(if (and (equal (find-x-coord square-eight puzzle-space) (find-x-coord square-eight *goal-state*)) 
					 (equal (find-x-coord square-nine puzzle-space) (find-x-coord square-nine *goal-state*)) 
					 (< square-nine square-eight)
				)
				(unless (or (equal square-eight 0) (equal square-nine 0))
						(setf linear-cost (+ linear-cost 2)))
			)
		)
		linear-cost)
)

;This is the extra credit heuristic. 
;I used the linear conflict in conjunction with manhattan distance
(defun extracredit (puzzle-space)
	(let ((distance (+ (manhattan puzzle-space) (linear-conflict puzzle-space))))
		distance)
)

;Find the x-coordinate of a specific piece on the existing board
(defun find-x-coord (piece state)
	(let ((x-coord nil))
		(dotimes (y *puzzle-dimension*)
			(dotimes (x *puzzle-dimension*)
				(if (eql (get-position-value state x y) piece)
					(setq x-coord x))))
		x-coord)
)

;Find the y-coordinate of a specific piece on the existing board
(defun find-y-coord (piece state)
	(let ((y-coord nil))
		(dotimes (y *puzzle-dimension*)
			(dotimes (x *puzzle-dimension*)
				(if (eql (get-position-value state x y) piece)
					(setq y-coord y))))
		y-coord)
)

;Generate successors of the state passed in
(defun successor (state)
		(incf *nodes-expanded*)
		(setf x (find-x-coord 0 state)) 
		(setf y (find-y-coord 0 state))
		(setf lower-x (1- x))
		(setf upper-x (1+ x))
		(setf lower-y (1- y))
		(setf upper-y (1+ y))
		(setf successors nil)
		(if (>= lower-x 0) (setf successors (append (list (append (cons '"up" (state-to-list (puzzle-state-board (swap-spaces state x y lower-x y)) )) (list 1))) successors)))
		(if (<= upper-x 2) (setf successors (append (list (append(cons '"down" (state-to-list(puzzle-state-board(swap-spaces state x y upper-x y))))(list 1)))successors)))
		(if (>= lower-y 0) (setf successors (append (list (append(cons '"left" (state-to-list (puzzle-state-board (swap-spaces state x y x lower-y))))(list 1)))successors)))
		(if (<= upper-y 2) (setf successors (append (list (append(cons '"right" (state-to-list(puzzle-state-board (swap-spaces state x y x upper-y))))(list 1))) successors)))
		successors
)

;Use this function to generate successors by shifting the blank piece
;with one of the other squares
(defun swap-spaces (state first-x first-y second-x second-y)
	(setf temp-value (aref (puzzle-state-board state) second-x second-y))
	(setf new-state (copy-state state))
	(set-puzzle-space new-state second-x second-y 0)
	(set-puzzle-space new-state first-x first-y temp-value)
	new-state
)

;Create a random state that proves the linear+manhattan heuristic is better than
;either the misplaced or the manhattan heuristics
(defun generate-random-state()
	(setf new-state '(0 1 2 3 4 5 6 7 8))
	(let ((x (length new-state)))
    (dotimes (i x new-state)
      (rotatef (elt new-state i)(elt new-state (+ i (random (- x i)))))))
	(if (and ( > (manhattan (list-to-state new-state)) (misplaced (list-to-state new-state)))
			 ( < (manhattan (list-to-state new-state)) (extracredit (list-to-state new-state))))
		new-state
		(generate-random-state))
)

;copy the state because it needs to be cloned so the initial state is not modified
;while generating successors
(defun copy-state (state)
	(setf new-state (get-puzzle-state))
	(dotimes (y *puzzle-dimension*)
		(dotimes (x *puzzle-dimension*)
				(setf (aref (puzzle-state-board new-state) x y)(aref (puzzle-state-board state) x y))))
	new-state
)

;create a list of 5 solvable states
(defun get-solvable-states (initial-state)
	(setf *total-steps* 0)
	(setf *minimum* (+ 103 (random 9)))
	(create-solvable-list (successor initial-state))
)

;generate the solvable states by using the successor function with the goal state
(defun create-solvable-list (successors &optional solvable)
	(incf *total-steps*)
	(cond 
		((= (length solvable) 5) solvable)
		(t 
			(setf num-successors (length successors))
			(setf successor (nth (random num-successors) successors))
			(create-solvable-list (successor (list-to-state (cadr successor)))
									(if (or (or (equal-states (list-to-state(cadr successor)) *goal-state*) (contains-list (cadr successor) solvable)) 
											(and (< *total-steps* *minimum*) (oddp *total-steps*))) 
											solvable 
											(cons (cadr successor) solvable))))
	)
)

;check if the successor-list already contains this state
(defun contains-list (successor successor-list)
	(cond 
		((null successor-list) nil)
		((equal successor (car successor-list)) t)
		(t (contains-list successor (cdr successor-list)))
	)
)

(setq *goal-state* (list-to-state '(0 1 2 3 4 5 6 7 8)))
