;;;; refactor.lisp

(defmacro awhen (test &body body)
  `(let ((,(intern "IT") ,test))
     (when ,(intern "IT") ,@body)))

(defun lift (list &optional others)
  (cond ((endp list) nil)
	((endp (cdr list)) (cons (car list) (reverse others)))
	(T (lift (cdr list) (cons (car list) others)))))

(defmacro recur (args vals &body body)
  `(labels ((recur ,args ,@body))
     (recur ,@vals)))

(defun flatten (list)
  (cond ((endp list) nil)
	((listp (car list)) (append (car list) (flatten (cdr list))))
	(T (cons (car list) (flatten (cdr list))))))

(defparameter local-scroll-name+ '(:scroll "local"))
(defparameter scratch-name+ '(0))
(defparameter editable-signature+ "EDITABLE")


(defstruct span origin start length)
(defstruct leaf name owner type sig)
(defstruct (content-leaf (:include leaf)) contents)
(defstruct (doc (:include leaf)) spans links)
(defstruct link type endsets)
(defstruct endset name)
(defstruct (span-endset (:include endset)) spans)
(defstruct (doc-endset (:include endset)) doc-name)

;; Span operations

(defun span (origin start length) (make-span :origin origin :start start :length length))

(defun edit-span (span &key (origin (span-origin span)) (start (span-start span))
			 (length (span-length span)))
  (span origin start length))

(defun span-end (s) (+ (span-start s) (span-length s) -1))

(defun same-origin (s1 s2) (equal (span-origin s1) (span-origin s2)))

(defun span-contains (span point &optional (adjustment 0))
  (let ((offset (- point (span-start span) adjustment)))
    (and (>= offset 0) (< offset (span-length span)))))

(defun overlapping-p (s1 s2 &key (adjust1 0) (adjust2 0))
  (not (or (< (+ (span-end s1) adjust1) (+ (span-start s2) adjust2))
	   (< (+ (span-end s2) adjust2) (+ (span-start s1) adjust1)))))

(defun abutting-p (s1 s2)
  (eq 1 (- (span-start s2) (span-end s1))))

(defun mergeable-p (s1 s2)
  (and (same-origin s1 s2) (or (overlapping-p s1 s2) (abutting-p s1 s2))))

(defun duplicating-p (s1 s2)
  (and (same-origin s1 s2) (overlapping-p s1 s2)))

(defun merge-spans (s1 s2 &optional only-overlaps)
  (if (if only-overlaps (duplicating-p s1 s2) (mergeable-p s1 s2))
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

(defun deduplicate (list)
  (if (endp (cdr list)) list
      (let ((merged (merge-spans (car list) (cadr list) T)))
	(if (cadr merged) (cons (car list) (deduplicate (cdr list)))
	    (deduplicate (cons (car merged) (cddr list)))))))

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

(defun find-span (spans point)
  (recur (spans pos) (spans 0)
    (cond ((endp spans) nil)
	  ((span-contains (car spans) point pos) (values (car spans) pos))
	  (T (recur (cdr spans) (+ pos (span-length (car spans))))))))

(defun transform-intersection (s i fn)
  "Breaks s into a list of spans according to which parts overlap with i, calling fn on the
parts that do"
  (if (not (and (same-origin s i) (overlapping-p s i))) (list s)
      (collecting
       (let ((length (- (span-start i) (span-start s))))
	 (if (> length 0) (collect (edit-span s :length length)))
	 (let ((start (max (span-start s) (span-start i))))
	   (collect
	       (funcall fn (edit-span s :start start
				      :length (1+ (- (min (span-end s) (span-end i)) start)))
			length))))
       (let ((length (- (span-end s) (span-end i))))
	 (if (> length 0)
	     (collect (edit-span s :start (1+ (span-end i)) :length length)))))))

;;;; Leaf operations

(defun save-leaf (leaf)
  (cond ((content-leaf-p leaf) (save-by-name (leaf-name leaf) (serialize-content-leaf leaf)))
	((doc-p leaf) (save-by-name (leaf-name leaf) (serialize-doc leaf)))
	(T (error "Should be a leaf: ~A" leaf))))

(defun load-all-contents (spans &optional (index (make-hash-table :test 'equal)))
  (dolist (a (mapcar #'span-origin spans))
    (when (not (nth-value 1 (gethash a index)))
      (setf (gethash a index) (load-and-parse a))
      (if (scroll-name-p a) (load-all-contents (doc-spans (gethash a index)) index))))
  index)

(defun generate-concatatext (spans &optional (contents-hash (load-all-contents spans)))
  (apply #'concatenate 'string
	 (mapcar (lambda (s) (apply-span s contents-hash)) spans)))

(defun generate-concatatext-clip (spans start length
				  &optional (contents-hash (load-all-contents spans)))
    (apply #'concatenate 'string
	 (mapcar (lambda (s) (apply-span s contents-hash))
		 (extract-range-from-spans spans start length))))

(defun apply-span (span contents-hash)
  "Extract the text of a span from a collection of contents leaves."
  (let ((contents (gethash (span-origin span) contents-hash)))
    (if (scroll-name-p (span-origin span))
	(generate-concatatext-clip (doc-spans contents) (span-start span) (span-length span)
				   contents-hash)
	(subseq (content-leaf-contents contents) (span-start span) (1+ (span-end span))))))

(defun get-concatatext-position (spans point-origin point &optional (pos 0))
  (with-slots (origin start length) (car spans)
    (cond ((endp spans) nil)
	  ((and (equal origin point-origin) (span-contains (car spans) point))
	   (values (+ pos (- point start)) (car spans)))
	  (T (get-concatatext-position (cdr spans) point-origin point (+ pos length))))))

(defun iterate-doc (doc on-clip on-link)
  (mapc on-clip (doc-spans doc))
  (mapc on-link (doc-links doc)))

(defun replace-spans (doc new-span-fn)
  (multiple-value-bind (clips links)
      (with-collectors (clips links)
	(iterate-doc
	 doc
	 (compose #'clips new-span-fn)
	 (lambda (l)
	   (let ((new (copy-link l))
		 (endsets (mapcar (lambda (e)
				    (if (span-endset-p e)
					(let ((newe (copy-span-endset e)))
					  (setf (span-endset-spans newe)
						(flatten
						 (mapcar new-span-fn (span-endset-spans e))))
					  newe)
					e))
				  (link-endsets l))))
	     (setf (link-endsets new) endsets)
	     (links new)))))
    (new-doc-leaf (doc-name doc) (flatten clips) links)))


(defun migrate-scroll-spans-to-scroll-targets (doc scroll-spans)
  (replace-spans
   doc
   (lambda (s) (if (scroll-span-p s)
		   (extract-range scroll-spans (span-start s) (span-length s))
		   s))))

(defun get-scratch-spans (doc)
  (collecting (iterate-spans doc (lambda (s) (if (scratch-span-p s) (collect s))))))

(defun build-map-from-scratch-spans (spans)
  (let ((deduped (deduplicate (sort spans #'< :key #'span-start))))
    (collecting
      (dolist (s spans)
	(awhen (find (span-start s) deduped :test (lambda (p s) (span-contains s p)))
	  (collect it)
	  (setf deduped (remove it deduped)))))))

(defun create-leaf-from-map (map name)
  (new-content-leaf name "text" (generate-concatatext map)))

(defun migrate-scratch-spans-to-leaf (doc map leaf-name)
  (replace-spans
   doc
   (lambda (s)
     (if (scratch-span-p s)
	 (span leaf-name
	       (get-concatatext-position map scratch-name+ (span-start s))
	       (span-length s))
	 s))))

(defun rewrite-scratch-span (span map pos leaf-name)
  (cond ((endp map) (list span))
	(T (mapcan (lambda (s)
		     (rewrite-scratch-span
		      s (cdr map) (+ pos (span-length (car map))) leaf-name))
		   (transform-intersection
		    span
		    (car map)
		    (lambda (x p) (span leaf-name (+ pos p) (span-length x))))))))

(defun migrate-scroll-spans-to-leaf (scroll-spans map leaf-name)
  (mapcan (lambda (s) (rewrite-scratch-span s map 0 leaf-name)) scroll-spans))

;; TODO passing a name is a workaround until it is calculated
(defun publish (doc leaf-name)
  (let* ((scroll (load-and-parse local-scroll-name+))
	 (migrated-to-scratch (migrate-scroll-spans-to-scroll-targets doc (doc-spans scroll)))
	 (map (build-map-from-scratch-spans (get-scratch-spans migrated-to-scratch)))
	 (new-leaf (create-leaf-from-map map leaf-name))
	 (fully-migrated
	  (migrate-scratch-spans-to-leaf migrated-to-scratch map (leaf-name new-leaf)))
	 (migrated-scroll-spans
	  (migrate-scroll-spans-to-leaf (doc-spans scroll) map (leaf-name new-leaf))))
    (save-leaf new-leaf)
    (save-leaf fully-migrated)
    (save-leaf (new-doc-leaf local-scroll-name+ migrated-scroll-spans nil))))
