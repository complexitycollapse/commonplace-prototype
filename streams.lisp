; An attempt at a codata approach

(defun list-generator (list)
  (lambda () (let ((end (endp list))) (values (if (not end) (pop list)) end))))

(defun stream-generator (stream readfn eof)
  (lambda () (let ((v (funcall readfn stream))) (values v (eq v eof)))))

(defun characters (stream) (stream-generator stream (lambda (s) (read-char s nil :eof)) :eof))

(defstruct (queue (:constructor %make-queue (start end)) (:copier nil)) start end)
(defun make-queue (&rest items) (let ((it (cons nil items))) (%make-queue it (last it))))
(defun has-items (queue) (cdr (queue-start queue)))
(defun enqueue (item queue)
   (setf (queue-end queue) (cdr (rplacd (queue-end queue) (list item)))))
(defun dequeue (queue)
  (prog1
      (if (has-items queue) (pop (cdr (queue-start queue))))
    (if (not (has-items queue)) (setf (queue-end queue) (queue-start queue)))))
(defun copy-queue (queue) (apply #'make-queue (copy-list (cdr (queue-start queue)))))
(defun queue-to-list (queue) (copy-list (cdr (queue-start queue))))

(defun iterable (generator)
  (let ((cache (make-array '(0) :adjustable T :fill-pointer T))
	(complete nil))
    (lambda ()
      (let ((n 0))
	(lambda ()
	  (cond (complete (values nil T))
		((= (fill-pointer cache) n)
		 (multiple-value-bind (v c) (funcall generator)
		   (setf complete c)
		   (if (not complete) (vector-push-extend v cache))
		   (values v complete)))
		(T (prog1 (aref cache n) (incf n)))))))))

#|
Parsers take a sequence and return a result plus an updated sequence. Therefore they take
a sequence argument (the stream).

The sequence is itself a function (but not a parser). It takes no arguments. Therefore simple
parsers take a function of no arguments.

Parser combinators take parsers (functions of one argument) and return other parsers (also
functions of one argument).
|#

(defun seq (&rest children)
  (labels ((recur (source)
	     (if (endp children) (values nil T)
		 (multiple-value-bind (v c) (funcall (car children) source)
		   (if c (progn (pop children) (recur source))
		       (values v nil))))))
    #'recur))

(defun call-once (fn)
  (let ((called nil))
    (lambda (source)
      (if called (values nil T) (progn (setf called T) (values (funcall fn source) nil))))))

(defun transform (parser fn)
  (lambda (source) (multiple-value-bind (v c) (funcall parser source)
	       (if c (values nil T) (values (funcall fn v) nil)))))

(defun listify (generator &optional (stop-predicate (identity nil)))
  (call-once (lambda (source)
	       (with-collectors (items)
		 (block nil
		   (loop :do
			(multiple-value-bind (v c) (funcall generator source)
			  (if (or c (funcall stop-predicate v)) (return) (items v)))))))))

(defun stringify (generator &optional (stop-predicate (identity nil)))
  (transform (listify generator stop-predicate) (lambda (list) (coerce list 'string))))

(defun drop (anything) (lambda (source) (funcall anything source) (values nil T)))

(defun pull-line () (stringify (lambda (x) (eql #\newline x))))

(defun parsed-name () (transform (pull-line) #'parse-name))

(defun parsed-leaf ()
  (seq (parsed-name)
       (pull-line)
       (pull-line)
       (drop (pull-line))))
