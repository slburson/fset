
(in-package :fset)

(defgeneric sort-and-group (seq pred &key key)
  (:documentation
    "Like 'stable-sort', but additionally groups the result, returning a seq of seqs,
where the elements of each inner seq are equal according to `pred' and, optionally,
`key'."))

(defmethod sort-and-group ((s seq) pred &key key)
  (if (empty? s) s
    (let ((sorted (stable-sort s pred :key key))
	  (result (seq))
	  (group (seq)))
      (do-seq (x sorted)
	(if (or (empty? group)
		(not (if key (funcall pred (funcall key (last group))
				      (funcall key x))
		       (funcall pred (last group) x))))
	    (push-last group x)
	  (progn
	    (push-last result group)
	    (setq group (with-first (empty-seq) x)))))
      ;; 'group' can't be empty if 's' was nonempty.
      (with-last result group))))

