;Modified general-search and graph-search to take the heuristic function as a parameter
;Key is now evaluation-function, enqueue function is a priority queue (heap)
(defun general-search (initial-state successor goalp heuristicp
							&key (samep #'equal-states)
								  (enqueue #'enqueue-priority)
								  (key #'evaluation-function))
	(let ((fringe (make-q :enqueue enqueue :key key)))
		(q-insert fringe (list (make-node :state initial-state :heuristic-cost (funcall heuristicp initial-state)))) ;added heuristic here
		(graph-search fringe nil successor goalp samep heuristicp)))

(defun graph-search (fringe closed successor goalp samep heuristicp)
	(unless (q-emptyp fringe)
		(let ((node (q-remove fringe)))
			(cond ((funcall goalp (node-state node))
					(cons (action-sequence node) (list (node-depth node))))
					((member (node-state node) closed
						:test samep :key #'node-state)
					(graph-search fringe closed successor goalp samep heuristicp))
					(t (let ((successors (expand successor node heuristicp)))
						(graph-search (q-insert fringe successors)
										(cons node closed)
										successor goalp samep heuristicp)))))))

(defstruct node (state nil) (parent nil) (action nil) (path-cost 0) (heuristic-cost 0) (depth 0))
										
(defun make-node (&key (state nil) (parent nil)
							 (action nil)
							 (path-cost 0) (heuristic-cost 0) (depth 0))
		(list state parent action path-cost heuristic-cost depth))

(defun node-state (node) (car node))
(defun node-parent (node) (cadr node))
(defun node-action (node) (caddr node))
(defun node-path-cost (node) (cadddr node))
(defun node-heuristic-cost (node) (car (cddddr node))) ;modified node to include heuristic cost
(defun node-depth (node) (car (cdr (cddddr node))))

(defun expand (successor node heuristicp)
	(let ((triples (funcall successor (node-state node))))
		(mapcar (lambda (action-state-cost)
				(let ((action (car action-state-cost))
					  (state (cadr action-state-cost))
					  (cost (caddr action-state-cost)))
					  (make-node :state (list-to-state state) 
								 :parent node
								 :action action 
								 :path-cost (+ (node-path-cost node)
													1)
								 :heuristic-cost (funcall heuristicp (list-to-state state)) ;node now includes heuristic cost
								 :depth (1+ (node-depth node)))))
				triples)))

(defun action-sequence (node &optional (actions nil))
	(if (node-parent node)
		(action-sequence (node-parent node)
						 (cons (node-action node) actions))
		actions))

;Evaluation function for the heap anad picking the lowest cost node. 
;Total cost is heuristic cost plus the actual cost from the root node to this node.
(defun evaluation-function (node)
	(+ (node-path-cost node)(node-heuristic-cost node)))