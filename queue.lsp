(defstruct q
	(enqueue #'enqueue-priority)
	(key #'evaluation-function)
	(last-element nil)
	(elements nil))

(defun q-emptyp (q)
	"Returns T if queue is empty."
	(= (length (q-elements q)) 0))
	
(defun q-front (q)
	(elt (q-elements q) 0))
	
(defun  q-remove (q)
	"Removes and returns the element at the front of the queue."
	(if (listp (q-elements q))
		(pop (q-elements q))
		(heap-pop (q-elements q) (q-key q))))
		
(defun q-insert (q items)
	"Inserts the items into the queue, according to the queue's
	enqueuing function. Returns the altered queue."
	(funcall (q-enqueue q) q items)
	q)
		
(defun enqueue-priority (q items)
	"Inserts the items by priority of key values."
	(when (null (q-elements q))
		(setf (q-elements q) (make-heap)))
	(mapc (lambda (item)
			(heap-insert (q-elements q) item (q-key q)))
			items)
	items)
	
(defun heap-val (heap i key) (funcall key (elt heap i)))
(defun heap-parent (i) (floor (1- i) 2))
(defun heap-left (i) (+ 1 i i))
(defun heap-right (i) (+ 2 i i))
(defun heap-leafp (heap i)(> i (1- (floor (length heap) 2))))

(defun heapify (heap i key)
	(unless (heap-leafp heap i)
		(let ((left-index (heap-left i))
			(right-index (heap-right i)))
			(let ((smaller-index 
				(if (and (< right-index (length heap))
						(< (heap-val heap right-index key)
							(heap-val heap left-index key)))
					right-index
					left-index)))
				(when (> (heap-val heap i key)
							(heap-val heap smaller-index key))
					(rotatef (elt heap i)
								(elt heap smaller-index))
						(heapify heap smaller-index key))))))
						
(defun heap-pop (heap key)
	"Pops the lowest item off the heap"
	(let ((min (elt heap 0)))
		(setf (elt heap 0)(elt heap (1- (length heap))))
		(decf (fill-pointer heap))
		(heapify heap 0 key)
		min))
		
(defun heap-insert (heap item key)
	"Puts an item into a heap"
	(vector-push-extend nil heap)
	(setf (elt heap (heap-find-pos heap (1- (length heap))
										(funcall key item) key))
										item))
										
(defun heap-find-pos (heap i val key)
	"Bubbles up from i to find position for val, moving items down in the process."
	(cond ((or (zerop i)
				(< (heap-val heap (heap-parent i) key) val))
				i)
				(t (setf (elt heap i)(elt heap (heap-parent i)))
					(heap-find-pos heap (heap-parent i) val key))))
					
(defun make-heap (&optional (size 100))
	(make-array size :fill-pointer 0 :adjustable t))
	
	