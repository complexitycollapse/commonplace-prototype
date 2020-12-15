;;;; refactor.lisp

(defparameter local-scroll-name+ '(:scroll "local"))
(defparameter scratch-name+ '(0))
(defparameter editable-signature+ "EDITABLE")

(defstruct span origin start length)

(defun span (origin start length) (make-span :origin origin :start start :length length))

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
	(span (span-origin s1) start (1+ (- (max (span-end s1) (span-end s2)) start))))))

(defun divide-span (s1 point)
  (with-slots (origin start length) s1
    (if (span-contains s1 point)
	(list (span origin start (- point start))
	      (span origin (+ start point -1) (1+ (- length point))))
	(list s1))))
