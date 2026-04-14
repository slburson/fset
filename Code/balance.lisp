(in-package :fset)



(defmethod mean-depth ((s wb-set))
  (let ((depth size (wb-set-tree-total-depth (wb-set-contents s))))
    (/ (float depth) size)))

(defun wb-set-tree-total-depth (tree)
  (cond ((null tree) (values 0 0))
	((simple-vector-p tree)
	 (let ((len (length tree)))
	   (values 0 len)))
	(t
	 (let ((left-depth left-size (wb-set-tree-total-depth (wb-set-tree-node-left tree)))
	       (right-depth right-size (wb-set-tree-total-depth (wb-set-tree-node-right tree))))
	   (values (+ left-depth left-size right-depth right-size)
		   (+ 1 left-size right-size))))))

(defmethod leaf-sizes ((s wb-set))
  (wb-set-tree-leaf-sizes (wb-set-contents s)))

(defun wb-set-tree-leaf-sizes (tree)
  (cond ((null tree) (wb-bag))
	((simple-vector-p tree)
	 (wb-bag (length tree)))
	(t
	 (bag-sum (wb-set-tree-leaf-sizes (wb-set-tree-node-left tree))
		  (wb-set-tree-leaf-sizes (wb-set-tree-node-right tree))))))


(defmethod mean-depth ((s wb-bag))
  (let ((depth size (wb-bag-tree-total-depth (wb-bag-contents s))))
    (/ (float depth) size)))

(defun wb-bag-tree-total-depth (tree)
  (cond ((null tree) (values 0 0))
	((consp tree)
	 (values 0 (length (car tree))))
	(t
	 (let ((left-depth left-size (wb-bag-tree-total-depth (wb-bag-tree-node-left tree)))
	       (right-depth right-size (wb-bag-tree-total-depth (wb-bag-tree-node-right tree))))
	   (values (+ left-depth left-size right-depth right-size)
		   (+ 1 left-size right-size))))))


(defmethod mean-depth ((s wb-map))
  (let ((depth size (wb-map-tree-total-depth (wb-map-contents s))))
    (/ (float depth) size)))

(defun wb-map-tree-total-depth (tree)
  (cond ((null tree) (values 0 0))
	((consp tree)
	 (values 0 (length (car tree))))
	(t
	 (let ((left-depth left-size (wb-map-tree-total-depth (wb-map-tree-node-left tree)))
	       (right-depth right-size (wb-map-tree-total-depth (wb-map-tree-node-right tree))))
	   (values (+ left-depth left-size right-depth right-size)
		   (+ 1 left-size right-size))))))


(defmethod mean-depth ((s wb-seq))
  (let ((depth size (wb-seq-tree-total-depth (wb-seq-contents s))))
    (/ (float depth) size)))

(defun wb-seq-tree-total-depth (tree)
  (cond ((null tree) (values 0 0))
	((simple-vector-p tree)
	 (let ((len (length tree)))
	   (values 0 len)))
	((wb-ht-seq-tree? tree)
	 (let ((body-depth body-size (wb-seq-tree-total-depth (wb-ht-seq-tree-body tree)))
	       (head-size (length (wb-ht-seq-tree-head tree)))
	       (tail-size (length (wb-ht-seq-tree-tail tree))))
	   (values (+ body-depth body-size head-size tail-size)
		   (+ body-size head-size tail-size))))
	(t
	 (let ((left-depth left-size (wb-seq-tree-total-depth (wb-seq-tree-node-left tree)))
	       (right-depth right-size (wb-seq-tree-total-depth (wb-seq-tree-node-right tree))))
	   (values (+ left-depth left-size right-depth right-size)
		   (+ left-size right-size))))))

(defmethod leaf-sizes ((s wb-seq))
  (wb-seq-tree-leaf-sizes (wb-seq-contents s)))

(defun wb-seq-tree-leaf-sizes (tree)
  (cond ((null tree) (wb-bag))
	((simple-vector-p tree)
	 (wb-bag (length tree)))
	((wb-ht-seq-tree? tree)
	 (bag-sum (wb-seq-tree-leaf-sizes (wb-ht-seq-tree-body tree))
		  (bag (length (wb-ht-seq-tree-head tree))
		       (length (wb-ht-seq-tree-tail tree)))))
	(t
	 (bag-sum (wb-seq-tree-leaf-sizes (wb-seq-tree-node-left tree))
		  (wb-seq-tree-leaf-sizes (wb-seq-tree-node-right tree))))))
