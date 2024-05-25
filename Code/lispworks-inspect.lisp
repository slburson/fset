(in-package :fset)

(defmethod system:sort-inspector-p ((object collection) mode)
  nil)

(defmethod lw:get-inspector-values ((object set) mode)
  (values (gmap (:result list) #'identity (:arg index 0 (size object)))
	  (convert 'list object)
	  nil
	  nil
	  "SET"))

(defmethod lw:get-inspector-values ((object bag) mode)
  (values (gmap (:result list) (fn (x _y) x) (:arg bag-pairs object))
	  (gmap (:result list) (fn (_x y) y) (:arg bag-pairs object))
	  nil
	  nil
	  "BAG"))

(defmethod lw:get-inspector-values ((object map) mode)
  (values (convert 'list (domain object))
	  (gmap (:result list) (fn (_x y) y) (:arg map object))
	  nil
	  nil
	  "MAP"))

(defmethod lw:get-inspector-values ((object map) (mode (eql 'domain)))
  (values (gmap (:result list) #'identity (:arg index 0 (size object)))
	  (convert 'list (domain object))
	  nil
	  nil
	  "MAP-DOMAIN"))

(defmethod lw:get-inspector-values ((object seq) mode)
  (values (gmap (:result list) #'identity (:arg index 0 (size object)))
	  (convert 'list object)
	  nil
	  nil
	  "SEQ"))
