
(defclass heap ()
  ((store :initform (make-array 1000 :adjustable T :fill-pointer 0)
	  :accessor store)
   (comparison :initform #'>
	       :initarg :comparison
	       :accessor comparison))
  (:documentation "a class representation of the heap data structure"))


(defgeneric shove (obj value)
  (:documentation "add one value to the provided heap, maintining the heap property"))

(defmethod shove ((obj heap) value)
  (with-slots (store comparison) obj
    (let ((new-position (fill-pointer store))) ;; this points to the new value
      (vector-push-extend value store) ;; increments fill-pointer
      (when (= new-position 0) (return-from shove T))
      (loop
	(let ((parent-position (- (floor (+ 1 new-position) 2) 1)))
	  (if (funcall comparison
		       (aref store new-position)
		       (aref store parent-position))
	      (progn
		(swap-vector-values store new-position parent-position)
		(setq new-position parent-position)
		(when (= new-position 0) (return-from shove T)))
	      (return T)))))))

(defgeneric yank (obj)
  (:documentation "remove the next in order element from the heap and rebalance"))

(defmethod yank ((obj heap))
  (with-slots (store comparison) obj
    (when (= (fill-pointer store) 0) (return-from yank NIL)) ;; empty
    (let ((new-fill-pointer (- (fill-pointer store) 1))
	  (parent-pointer 0)
	  (popped-element (aref store 0)))
      (setf (fill-pointer store) new-fill-pointer)
      (when (= new-fill-pointer 0) (return-from yank popped-element))
      (setf (aref store parent-pointer)
	    (aref store new-fill-pointer)) ;; last element to top
      (loop
	(let* ((first-child-pointer (- (* 2 (+ 1 parent-pointer)) 1))
	       (second-child-pointer (+ 1 first-child-pointer))
	       (single-child-p (= second-child-pointer new-fill-pointer)))
	  (when (>= first-child-pointer new-fill-pointer)
	    (return-from yank popped-element))
	  (when (and single-child-p (funcall comparison
					     (aref store first-child-pointer)
					     (aref store parent-pointer)))
	    (progn
	      (swap-vector-values store parent-pointer first-child-pointer)
	      (return-from yank popped-element)))
	  (let* ((parent-element (aref store parent-pointer))
		 (first-child-element (aref store first-child-pointer))
		 (second-child-element (aref store second-child-pointer))
		 (first>p (funcall comparison first-child-element second-child-element)))
	    (if first>p
		(if (funcall comparison first-child-element parent-element)
		    (progn
		      (swap-vector-values store parent-pointer first-child-pointer)
		      (setq parent-pointer first-child-pointer))
		    (return-from yank popped-element))
		(if (funcall comparison second-child-element parent-element)
		    (progn
		      (swap-vector-values store parent-pointer second-child-pointer)
		      (setq parent-pointer second-child-pointer))
		    (return-from yank popped-element)))))))))

(defmacro swap-vector-values (v pos1 pos2)
  "swap the values at the two positions in vector v"
  `(rotatef (aref ,v ,pos1) (aref ,v ,pos2)))




  




