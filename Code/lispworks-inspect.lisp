(in-package :fset)

(defmethod system:sort-inspector-p ((object collection) mode)
  nil)

(defmethod lw:get-inspector-values ((object set) mode)
  (values
   (loop for count from 0 below (size object)
         collecting count)
   (convert 'list object)
   nil
   nil
   "SET"))

(defmethod lw:get-inspector-values ((object bag) mode)
  (values
   (let (names)
     (do-bag-pairs (value mult object names)
       (lw:appendf names (list value))))
   (let (values)
     (do-bag-pairs (value mult object values)
       (lw:appendf values (list mult))))
   nil
   nil
   "BAG"))

(defmethod lw:get-inspector-values ((object map) mode)
  (values
   (convert 'list (domain object))
   (convert 'list (range object))
   nil
   nil
   "MAP"))

(defmethod lw:get-inspector-values ((object map) (mode (eql 'domain)))
  (values
   (loop for count from 0 below (size object)
         collecting count)
   (convert 'list (domain object))
   nil
   nil
   "MAP-DOMAIN"))

(defmethod lw:get-inspector-values ((object seq) mode)
  (values
   (loop for count from 0 below (size object)
         collecting count)
   (convert 'list object)
   nil
   nil
   "SEQ"))
