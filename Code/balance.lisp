(in-package :fset)



(defmethod mean-depth ((s wb-set))
  (let ((depth size (WB-Set-Tree-Total-Depth (wb-set-contents s))))
    (/ (float depth) size)))

(defun WB-Set-Tree-Total-Depth (tree)
  (cond ((null tree) (values 0 0))
	((simple-vector-p tree)
	 (let ((len (length tree)))
	   (values 0 len)))
	(t
	 (let ((left-depth left-size (WB-Set-Tree-Total-Depth (WB-Set-Tree-Node-Left tree)))
	       (right-depth right-size (WB-Set-Tree-Total-Depth (WB-Set-Tree-Node-Right tree))))
	   (values (+ left-depth left-size right-depth right-size)
		   (+ 1 left-size right-size))))))

(defmethod leaf-sizes ((s wb-set))
  (WB-Set-Tree-Leaf-Sizes (wb-set-contents s)))

(defun WB-Set-Tree-Leaf-Sizes (tree)
  (cond ((null tree) (wb-bag))
	((simple-vector-p tree)
	 (wb-bag (length tree)))
	(t
	 (bag-sum (WB-Set-Tree-Leaf-Sizes (WB-Set-Tree-Node-Left tree))
		  (WB-Set-Tree-Leaf-Sizes (WB-Set-Tree-Node-Right tree))))))


(defmethod mean-depth ((s wb-bag))
  (let ((depth size (WB-Bag-Tree-Total-Depth (wb-bag-contents s))))
    (/ (float depth) size)))

(defun WB-Bag-Tree-Total-Depth (tree)
  (cond ((null tree) (values 0 0))
	((consp tree)
	 (values 0 (length (car tree))))
	(t
	 (let ((left-depth left-size (WB-Bag-Tree-Total-Depth (WB-Bag-Tree-Node-Left tree)))
	       (right-depth right-size (WB-Bag-Tree-Total-Depth (WB-Bag-Tree-Node-Right tree))))
	   (values (+ left-depth left-size right-depth right-size)
		   (+ 1 left-size right-size))))))


(defmethod mean-depth ((s wb-map))
  (let ((depth size (WB-Map-Tree-Total-Depth (wb-map-contents s))))
    (/ (float depth) size)))

(defun WB-Map-Tree-Total-Depth (tree)
  (cond ((null tree) (values 0 0))
	((consp tree)
	 (values 0 (length (car tree))))
	(t
	 (let ((left-depth left-size (WB-Map-Tree-Total-Depth (WB-Map-Tree-Node-Left tree)))
	       (right-depth right-size (WB-Map-Tree-Total-Depth (WB-Map-Tree-Node-Right tree))))
	   (values (+ left-depth left-size right-depth right-size)
		   (+ 1 left-size right-size))))))


(defmethod mean-depth ((s wb-seq))
  (let ((depth size (WB-Seq-Tree-Total-Depth (wb-seq-contents s))))
    (/ (float depth) size)))

(defun WB-Seq-Tree-Total-Depth (tree)
  (cond ((null tree) (values 0 0))
	((simple-vector-p tree)
	 (let ((len (length tree)))
	   (values 0 len)))
	((WB-HT-Seq-Tree? tree)
	 (let ((body-depth body-size (WB-Seq-Tree-Total-Depth (WB-HT-Seq-Tree-Body tree)))
	       (head-size (length (WB-HT-Seq-Tree-Head tree)))
	       (tail-size (length (WB-HT-Seq-Tree-Tail tree))))
	   (values (+ body-depth body-size head-size tail-size)
		   (+ body-size head-size tail-size))))
	(t
	 (let ((left-depth left-size (WB-Seq-Tree-Total-Depth (WB-Seq-Tree-Node-Left tree)))
	       (right-depth right-size (WB-Seq-Tree-Total-Depth (WB-Seq-Tree-Node-Right tree))))
	   (values (+ left-depth left-size right-depth right-size)
		   (+ left-size right-size))))))

(defmethod leaf-sizes ((s wb-seq))
  (WB-Seq-Tree-Leaf-Sizes (wb-seq-contents s)))

(defun WB-Seq-Tree-Leaf-Sizes (tree)
  (cond ((null tree) (wb-bag))
	((simple-vector-p tree)
	 (wb-bag (length tree)))
	((WB-HT-Seq-Tree? tree)
	 (bag-sum (WB-Seq-Tree-Leaf-Sizes (WB-HT-Seq-Tree-Body tree))
		  (bag (length (WB-HT-Seq-Tree-Head tree))
		       (length (WB-HT-Seq-Tree-Tail tree)))))
	(t
	 (bag-sum (WB-Seq-Tree-Leaf-Sizes (WB-Seq-Tree-Node-Left tree))
		  (WB-Seq-Tree-Leaf-Sizes (WB-Seq-Tree-Node-Right tree))))))
