;;;; refactor.lisp

(defun lift (list &optional others)
  (cond ((endp list) nil)
	((endp (cdr list)) (cons (car list) (reverse others)))
	(T (lift (cdr list) (cons (car list) others)))))

(defparameter local-scroll-name+ '(:scroll "local"))
(defparameter scratch-name+ '(0))
(defparameter editable-signature+ "EDITABLE")

(defstruct span origin start length)

(defun span (origin start length) (make-span :origin origin :start start :length length))

(defun edit-span (span &key (origin (span-origin span)) (start (span-start span))
			 (length (span-length span)))
  (span origin start length))

(defun span-end (s) (+ (span-start s) (span-length s) -1))

(defun span-contains (span point &optional (adjustment 0))
  (let ((offset (- point (span-start span) adjustment)))
    (and (>= offset 0) (< offset (span-length span)))))

(defun overlapping-p (s1 s2 &key (adjust1 0) (adjust2 0))
  (not (or (< (+ (span-end s1) adjust1) (+ (span-start s2) adjust2))
	   (< (+ (span-end s2) adjust2) (+ (span-start s1) adjust1)))))

(defun abutting-p (s1 s2)
  (eq 1 (- (span-start s2) (span-end s1))))

(defun mergeable-p (s1 s2)
  (and (equal (span-origin s1) (span-origin s2))
       (or (overlapping-p s1 s2) (abutting-p s1 s2))))

(defun merge-spans (s1 s2)
  (if (mergeable-p s1 s2)
      (let ((start (min (span-start s1) (span-start s2))))
	(list (span (span-origin s1) start (1+ (- (max (span-end s1) (span-end s2)) start)))))
      (list s1 s2)))

(defun divide-span-at-point (s1 point)
  (with-slots (start length) s1
    (if (and (span-contains s1 point) (> point (span-start s1)))
	(list (edit-span s1 :length (- point start))
	      (edit-span s1 :start (+ start point -1) :length (1+ (- length point))))
	(list s1))))

(defun divide-span (s length)
  (if (or (zerop length) (>= length (span-length s)))
      (list s)
      (list (edit-span s :length length)
	    (edit-span s :start (+ (span-start s) length) :length (- (span-length s) length)))))

(defun merge-span-lists (list1 list2)
  (let ((lifted (lift list1)))
    (append (cdr lifted) (merge-spans (car lifted) (car list2)) (cdr list2))))

(defun merge-all (list)
  (if (endp (cdr list)) list
      (let ((merged (merge-spans (car list) (cadr list))))
	(if (cadr merged) (cons (car list) (merge-all (cdr list)))
	    (merge-all (cons (car merged) (cddr list)))))))

(defun divide-list (list point &optional collected)
  "Point is included in the second list"
  (let ((s (car list)))
    (cond ((endp list) (list (nreverse collected) nil))
	  ((zerop point) (list (nreverse collected) list))
	  ((> (span-length s) point)
	   (let ((split (divide-span s point)))
	     (list (nreverse (cons (car split) collected)) (cons (cadr split) (cdr list)))))
	  (T (divide-list (cdr list) (- point (span-length s)) (cons s collected))))))

(defun divide-twice (list start length)
  (let ((div (divide-list list start)))
    (cons (car div) (divide-list (cadr div) length))))

(defun extract-range (spans start length)
  (second (divide-twice spans start length)))

(defun insert-spans (spans new-spans point)
  (let ((div (divide-spans spans point)))
    (merge-span-lists (car div) (merge-span-lists new-spans (cadr div)))))

(defun delete-spans (spans start length)
  (let ((div (divide-twice spans start length)))
    (merge-span-lists (first div) (third div))))

(defun transclude (source-spans start length target-spans insert-point)
  "Transclude content from one set of spans into another."
  (insert-spans target-spans (extract-range source-spans start length) insert-point))
