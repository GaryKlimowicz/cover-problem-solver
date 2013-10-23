;;;; ____________________________________________________________
;;;; Knuth's Dancing Links

;;; Technique is described in Knuth's Dancing Links paper,
;;; http://www-cs-faculty.stanford.edu/~knuth/papers/dancing-color.ps.gz

(in-package "COVER-PROBLEM")

(declaim (optimize (debug 3) (safety 3)))

(defstruct (toroid-node (:type vector) (:conc-name nil))
  l r u d)

(defstruct (node (:type vector) (:include toroid-node) (:conc-name nil))
  c)

(defstruct (col-header (:type vector) (:include toroid-node) (:conc-name nil))
  n (s 0 :type fixnum) (secondary nil))

(defclass cover-problem ()
  ((mh :accessor mh :initform (make-col-header))
   (o :accessor o :initform (make-array 500 :adjustable t))
   (column-names :accessor column-names :initargs :column-names :initform nil)
   (search-tries :accessor search-tries :initform 0)
   (max-search-depth :accessor max-search-depth :initform 0)
   (solutions-found :accessor solutions-found :initform 0)))

(defmethod initialize-instance :after ((cover-problem cover-problem) &key &allow-other-keys)
  (with-slots (mh) cover-problem
    (setf (l mh) mh
	  (r mh) mh
	  (u mh) nil
	  (d mh) nil)))

(defun find-column (cover-problem col-name)
  "Locate the name of a column in the matrix. Assumes that all columns are represented from the header. That is, we haven't started solving the cover problem, and so no columns are covered."
  (loop for col-head = (r (mh cover-problem)) then (r col-head)
       until (eq col-head (mh cover-problem))
       when (equal (n col-head) col-name)
       do (return-from find-column col-head))
  (error "Can't find column name ~A in the cover-problem" col-name))

(defun set-column-headers (dl)
  (with-slots (mh column-names) dl
      (loop for col-name in column-names
	as col-head = (make-col-header)
	do (setf (u col-head) col-head
		 (d col-head) col-head
		 (r col-head) mh
		 (l col-head) (l mh)
		 (r (l col-head)) col-head
		 (l mh) col-head
		 (n col-head) col-name))))

(defun set-secondaries (dl secondaries)
  "Unlink the secondary columns from the other column headers. Should be called after the rows are set, as this makes the column headers unfindable."
  (loop for col in secondaries
     as col-head = (find-column dl col)
     do (setf (secondary col-head) t)
       (setf (r (l col-head)) (r col-head)
	     (l (r col-head)) (l col-head)
	     (r col-head) col-head
	     (l col-head) col-head)))

(defun set-row (dl row)
  (loop with row-head = nil
	     for col-name in row
	     as col-head = (find-column dl col-name)
	     as node = (make-node)
	     do (if (null row-head)
		    (setf row-head node
			  (l node) node
			  (r node) node)
		  (setf (r node) row-head
			(l node) (l row-head)
			(r (l node)) node
			(l row-head) node))
	     (setf (d node) col-head
		   (u node) (u col-head)
		   (d (u node)) node
		   (u col-head) node
		   (c node) col-head)
	     (incf (s col-head))))

(defun create-cover-problem (columns secondaries rows)
  "Create a cover problem from a set of all column names, a set of secondary column namess, and a set of rows that each list the columns present in the row."
  (let ((dl (make-instance 'cover-problem)))
    (setf (column-names dl) columns)
    (set-column-headers dl)
    (loop for row in rows do (set-row dl row))
    (set-secondaries dl secondaries)
    dl))

(defun walk-matrix-rows (cover-problem row-visitor)
  "Walk the matrix by rows. The row-visitor is called with a node on the row; there's no guarantee about which node in the row."
  (let ((nodes-visited (make-hash-table :test #'eq)))
    (loop for col-head = (r (mh cover-problem)) then (r col-head)
       until (eq col-head (mh cover-problem))
       do (loop for node = (d col-head) then (d node)
	     until (eq node col-head)
	     unless (gethash node nodes-visited)
	     do (setf (gethash node nodes-visited) t)
	       (loop for first = t then nil
		  for rnode = node then (r rnode)
		  until (and (not first) (eq rnode node))
		  do (setf (gethash rnode nodes-visited) t))
	       (funcall row-visitor node)))))

(defun collect-row (node)
  "Collect the nodes in a row as a list of column names."
  (cons (n (c node))
	(loop for rnode = (r node) then (r rnode)
	   until (eq rnode node)
	   collect (n (c rnode)))))

(defun collect-solution (cover-problem k)
  (with-slots (o) cover-problem
    (loop for i below k
       as obj = (aref o i)
       collect (collect-row obj))))

(defun print-solution (rows depth tries max-depth)
  "Print the solution to a cover problem. For each node in the solution matrix 'o', print the names of the columns in that row."
  (format t "Solution at depth ~D: ~D rows, ~D tries, ~D max depth:~%" depth (length rows) tries max-depth)
  (loop for row in rows
     do (format t "  ~A~%" row)))

(defun print-row (node)
  "Prints the nodes in a row as a list of column names."
  (format t "  (~A" (n (c node)))
  (loop for rnode = (r node) then (r rnode)
     until (eq rnode node)
     do (format t " ~A" (n (c rnode))))
  (format t ")~%"))

(defun print-matrix (cover-problem)
  "Print an entire dancing-links matrix. This is primarily used for debug purposes."
  (format t "columns: ~%  (")
  (loop for col-head = (r (mh cover-problem)) then (r col-head)
     until (eq col-head (mh cover-problem))
     do (format t "~A:~D " (n col-head) (s col-head)))
  (format t ")~%rows:~%")
  (walk-matrix-rows cover-problem #'(lambda (node) (print-row node))))

(defun choose-column (cover-problem)
  "Choose a column to proceed with the solution. We choose a column with the least number of 1-s set. We don't look at secondary columns. We can stop when we've found a column with just one entry."
  (loop with s = most-positive-fixnum
     with c = nil
     for col-head = (r (mh cover-problem)) then (r col-head)
     until (or (eql s 1) (eq col-head (mh cover-problem)))
     do (when (and (not (secondary col-head))
		   (< (s col-head) s))
	  (setf s (s col-head)
		c col-head))
     finally (return c)))

(defun cover-column (c)
  "Cover a column by removing the column from the list of column headers. For each row that is covered, remove all the columns with their bits set. That is, we remove the column, and all columns that are associated with any row in the column."
  (setf (l (r c)) (l c)
	(r (l c)) (r c))
  (loop for i = (d c) then (d i) until (eq i c)
       do (loop for j = (r i) then (r j) until (eq j i)
	       do (setf (u (d j)) (u j)
			(d (u j)) (d j))
	       (decf (s (c j))))))

(defun uncover-column (c)
  "Undo a column covering."
  (loop for i = (u c) then (u i) until (eq i c)
       do (loop for j = (l i) then (l j) until (eq j i)
	       do (incf (s (c j)))
	       (setf (u (d j)) j
		     (d (u j)) j)))
  (setf (l (r c)) c
	(r (l c)) c))

(defun check-solution (dl k)
  "Check a solution to the dancing-links instance. The solution, in the array 'o', is considered valid if no column is covered twice, and all non-secondary columns in the problem are covered."
  (let ((solution-valid t)
	(cols-covered nil)
	(col-names-covered nil))
    (loop for i below k
       as node = (aref (o dl) i)
       do (loop for first = t then nil
	     for rnode = node then (r rnode)
	     for chead = (c rnode)
	     until (and (not first) (eq rnode node))
	     do (if (member chead cols-covered :test #'eq)
		    (progn (setf solution-valid nil)
			   (format t "Column ~A covered more than once." (n rnode)))
		    (progn (push chead cols-covered)
			   (push (n chead) col-names-covered)))))
    #-(AND)(when (set-difference (column-names dl) col-names-covered :test #'equal)
      (format t "Columns not covered: ~A~%" (set-difference (column-names dl) col-names-covered :test #'equal))
      (setq solution-valid nil))
    solution-valid))

(defun solve (cover-problem &key (solution-callback #'print-solution) (max-solutions most-positive-fixnum) (debug nil))
  "Solve the cover problem using Knuth's algorithm DLX. (solution-callback  rows search-tries max-search-depth) is called when a solution is found, passed a list of rows that make up the solution. Each row contains the names of the columns set in that row. Returns the number of solutions found."
  (when debug (format t "Solve: Initial matrix: ~%") (print-matrix cover-problem))
  (with-slots (mh o solutions-found max-search-depth search-tries) cover-problem
    (labels ((dlx-search (k)
	       (when (> k max-search-depth)
		 (setf max-search-depth k))
	       (incf search-tries)
	       (when (eq (r mh) mh)
		 (incf solutions-found)
		 (assert (check-solution cover-problem k))
		 (funcall solution-callback (collect-solution cover-problem k) k search-tries max-search-depth)
		 (if (= solutions-found max-solutions)
		     (throw nil solutions-found)
		     (return-from dlx-search)))
	       (let ((c (choose-column cover-problem)))
		 (cover-column c)
		 (when debug (format t "Level ~D: after initial cover ~A:~%" k (n c)) (print-matrix cover-problem))
		 (loop for r = (d c) then (d r)
		      until (eq r c)
		      do (setf (aref o k) r)
		      (loop for j = (r r) then (r j) until (eq j r)
			   do (cover-column (c j)))
		      (when debug (format t "Level ~D: after total cover ~A:~%" k (n c)) (print-matrix cover-problem))
		      (dlx-search (1+ k))
		      #-(AND)(setf r (aref o k)
				   c (c r))
		      (loop for j = (l r) then (l j) until (eq j r)
			   do (uncover-column (c j)))
		      (when debug (format t "Level ~D: after partial uncover ~A:~%" k (n c)) (print-matrix cover-problem)))
		 (uncover-column c)
		 (when debug (format t "Level ~D: after full uncover ~A:~%" k (n c)) (print-matrix cover-problem))
		 (values solutions-found search-tries max-search-depth))))
      (setf solutions-found 0
	    max-search-depth 0
	    search-tries 0)
      (unwind-protect (dlx-search 0)
	(values solutions-found search-tries max-search-depth)))))


;;;; ____________________________________________________________
;;;; Unit tests

(defmacro doesnt-throw-error (&body body)
  `(unwind-protect (progn ,@body t)
    nil))

(defconstant +standard-test-matrix-1+
  '((c0 c1 c2 c3 c4 c5 c6 c7) () ((c0) (c1) (c2) (c3) (c4) (c5) (c6) (c7))))

(defconstant +standard-test-matrix-2+
  (list (first +standard-test-matrix-1+)
	(second +standard-test-matrix-1+)
	(append (third +standard-test-matrix-1+) '((c0 c7)))))

(defun make-test-matrix-1 ()
  (apply #'create-cover-problem +standard-test-matrix-1+))

(defun make-test-matrix-2 ()
  (apply #'create-cover-problem +standard-test-matrix-2+))

(defun make-test-matrix-knuth-3 ()
  (create-cover-problem  '(c0 c1 c2 c3 c4 c5 c6)
			 nil
			 '((c2 c4 c5)
			   (c0 c3 c6)
			   (c1 c2 c5)
			   (c0 c3)
			   (c1 c6)
			   (c3 c4 c6))))

(com.gigamonkeys.test:deftest test-create-1 ()
    (doesnt-throw-error (make-test-matrix-1)))

(com.gigamonkeys.test:deftest test-create-2 ()
    (doesnt-throw-error (make-test-matrix-2)))

(com.gigamonkeys.test:deftest test-create ()
  (com.gigamonkeys.test:check
    (test-create-1)
    (test-create-2)))

(com.gigamonkeys.test:deftest test-print-1 ()
  (com.gigamonkeys.test:check
    (doesnt-throw-error (print-matrix (make-test-matrix-1)))))

(com.gigamonkeys.test:deftest test-print-2 ()
  (com.gigamonkeys.test:check
    (doesnt-throw-error (print-matrix (make-test-matrix-2)))))

(com.gigamonkeys.test:deftest test-print-knuth-3 ()
  (com.gigamonkeys.test:check
    (doesnt-throw-error (print-matrix (make-test-matrix-knuth-3)))))

(com.gigamonkeys.test:deftest test-print ()
  (com.gigamonkeys.test:check
    (test-print-1)
    (test-print-2)
    (test-print-knuth-3)))

(com.gigamonkeys.test:deftest test-choose-column-1 ()
  (let* ((dl (make-test-matrix-1))
	 (all-cols (loop for col-head = (r (mh dl)) then (r col-head)
		      until (eq col-head (mh dl))
		      collect col-head)))
    (member (choose-column dl) all-cols)))

(com.gigamonkeys.test:deftest test-choose-column-2 ()
  (let* ((dl (make-test-matrix-2))
	 (1-cols (loop for col-head = (r (mh dl)) then (r col-head)
		    until (eq col-head (mh dl))
		    when (eql 1 (s col-head)) collect col-head)))
    (member (choose-column dl) 1-cols)))

(com.gigamonkeys.test:deftest test-choose-column-knuth-3 ()
  (let* ((dl (make-test-matrix-knuth-3))
	 (2-cols (loop for col-head = (r (mh dl)) then (r col-head)
		    until (eq col-head (mh dl))
		    when (eql 2 (s col-head))
		    collect col-head)))
    (member (choose-column dl) 2-cols)))

(com.gigamonkeys.test:deftest test-choose-column ()
  (com.gigamonkeys.test:check
    (test-choose-column-1)
    (test-choose-column-2)
    (test-choose-column-knuth-3)))

(com.gigamonkeys.test:deftest test-cover-column-knuth-3 ()
  (let* ((dl (make-test-matrix-knuth-3))
	 (c (choose-column dl)))
    (cover-column c)))

(com.gigamonkeys.test:deftest test-cover-column ()
  (com.gigamonkeys.test:check
    (doesnt-throw-error (test-cover-column-knuth-3))))

(com.gigamonkeys.test:deftest test-cover-uncover-column-knuth-3 ()
  "Cover a column and then uncover it and make sure that all columns and column sizes are the same."
  (let* ((dl (make-test-matrix-knuth-3))
	 (col-sizes (loop for col-head = (r (mh dl)) then (r col-head)
		       until (eq col-head (mh dl))
		       collect (list (n col-head) (s col-head))))
	 (c (choose-column dl)))
    (cover-column c)
    (uncover-column c)
    (let ((new-col-sizes (loop for col-head = (r (mh dl)) then (r col-head)
		       until (eq col-head (mh dl))
		       collect (list (n col-head) (s col-head)))))
      (if (not (equalp col-sizes new-col-sizes))
	  (progn (format t "Before ~A didn't match after ~A~%" col-sizes new-col-sizes)
		 nil)
	  t))))

(com.gigamonkeys.test:deftest test-cover-uncover-column-knuth-3x2 ()
  "Cover a column and then uncover it and make sure that all columns and column sizes are the same."
  (let* ((dl (make-test-matrix-knuth-3))
	 (col-sizes (loop for col-head = (r (mh dl)) then (r col-head)
		       until (eq col-head (mh dl))
		       collect (list (n col-head) (s col-head))))
	 (c (choose-column dl)))
    (cover-column c)
    (let ((c2 (choose-column dl)))
      (cover-column c2)
      (uncover-column c2))
    (uncover-column c)
    (let ((new-col-sizes (loop for col-head = (r (mh dl)) then (r col-head)
		       until (eq col-head (mh dl))
		       collect (list (n col-head) (s col-head)))))
      (if (not (equalp col-sizes new-col-sizes))
	  (progn (format t "Before ~A didn't match after ~A~%" col-sizes new-col-sizes)
		 nil)
	  t))))

(com.gigamonkeys.test:deftest test-uncover-column ()
  (com.gigamonkeys.test:check
    (test-cover-uncover-column-knuth-3)
    (test-cover-uncover-column-knuth-3x2)))

(com.gigamonkeys.test:deftest test-solve-knuth-3 ()
  (let ((dl (make-test-matrix-knuth-3))
	(callbacks 0)
	(solutions-count 0))
    (labels ((got-solution (rows depth tries max-depth)
	       (declare (ignore rows depth tries max-depth))
	       (incf callbacks)))
      (setf solutions-count (solve dl :solution-callback #'got-solution)))
    (= callbacks solutions-count)))

(defun make-8-queens-matrix ()
  (let ((ranks-and-files (loop for i from 0 to 7 append (list (format nil "R~D" i) (format nil "F~D" i))))
	(a-diagonals (loop for i from 0 to 14 collect (format nil "A~D" i)))
	(b-diagonals (loop for i from 0 to 14 collect (format nil "B~D" i)))
	(rows (loop for r from 0 to 7 append
		(loop for f from 0 to 7 collect (list (format nil "R~D" r)
						     (format nil "F~D" f)
						     (format nil "A~D" (+ r f))
						     (format nil "B~D" (+ 7 (- r) f)))))))
    (create-cover-problem (append ranks-and-files a-diagonals b-diagonals)
			  (append a-diagonals b-diagonals)
			  rows)))

(com.gigamonkeys.test:deftest test-solve-8-queens ()
  (let ((dl (make-8-queens-matrix))
	(solutions-count 0)
	(solutions-returned))
    (labels ((callback (rows depth tries max-depth)
	       (declare (ignore depth tries max-depth))
	       (assert (eql 8 (length rows) ))
	       (incf solutions-count)))
      (setf solutions-returned (solve dl :solution-callback #'callback)))
    (if (= solutions-count solutions-returned 92)
	t
	(progn (format t "Found ~D solutions; expected ~D~%" solutions-count solutions-returned)
	       nil))))

(com.gigamonkeys.test:deftest test-solve ()
  (com.gigamonkeys.test:check
    (test-solve-knuth-3)
    (test-solve-8-queens)))

(eval-when (:load-toplevel :execute)
  (com.gigamonkeys.test::combine-results
    (test-create)
    (test-print)
    (test-choose-column)
    (test-cover-column)
    (test-uncover-column)
    (test-solve)))
