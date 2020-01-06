;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

(in-package :fset)

;;; File: fset.lisp
;;; Contents: Top level of FSet, the fast functional set-theoretic datatypes package.
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2012 Scott L. Burson.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; See: http://opensource.franz.com/preamble.html
;;; This license provides NO WARRANTY.


;;; ================================================================================
;;; Generic functions

;;; We make almost all the interface operations generic to support the addition of
;;; new implementations.

(defgeneric empty? (collection)
  (:documentation "Returns true iff the collection is empty."))

;;; Wish I could think of a shorter name that would still be easy to remember.
(declaim (inline nonempty?))
(defun nonempty? (collection)
  "Returns true iff the collection is not empty."
  (not (empty? collection)))

(defgeneric size (collection)
  (:documentation
    "Returns the number of members in a set, seq, or bag, or the number of
pairs in a map.  The size of a bag is the sum of the multiplicities."))

(defgeneric set-size (bag)
  (:documentation
    "Returns the number of unique members in the bag."))

(defgeneric arb (collection)
  (:documentation
    "Returns an arbitrary member or pair of a set, bag, or map.  Specifically,
on a nonempty set, returns two values, an arbitrary member of the set and
true; on a nonempty bag, returns an arbitrary member, its multiplicity,
and true; on a nonempty map, returns an arbitrary domain member, its
associated value, and true.  On an empty set, bag, or map, returns false for
all values.   Please note that \"arbitrary\" does not mean \"randomly selected\";
it simply means that the sole postcondition is that the returned value or pair
is a member of the collection."))

;;; I've decided I prefer `contains?' because its argument order is more
;;; consistent -- I think all the other operations that take a collection and
;;; a value which might be a member of the collection or its domain, take the
;;; collection as the first argument.  (Well, except for those we inherit from
;;; CL, like `find'.)
(defun member? (x collection)
  "Returns true iff `x' is a member of the set or bag.  Stylistically, `contains?'
is preferred over `member?'."
  (contains? collection x))

(defgeneric contains? (collection x &optional y)
  (:documentation
    "Returns true iff the set or bag contains `x', or the map or relation contains
the pair <x, y>."))

(defgeneric domain-contains? (collection x)
  (:documentation
    "Returns true iff the domain of the map or seq contains `x'.  (The domain
of a seq is the set of valid indices.)"))

;;; This is a common operation on seqs, making me wonder if the name should
;;; be shorter, but I like the clarity of this name.  Simply defining `contains?'
;;; on maps and seqs to do this is not entirely out of the question, but (a) I
;;; previously had `contains?' on a map meaning `domain-contains?', and (b) I
;;; prefer a single generic function to have a single time complexity.
(defgeneric range-contains? (collection x)
  (:documentation
    "Returns true iff the range of the map or seq contains `x'.  (The range
of a seq is the set of members.)  Note that this requires a linear search."))

;;; This used to take its arguments in the other order.
(defgeneric multiplicity (bag x)
  (:documentation "Returns the multiplicity of `x' in the bag."))

(defgeneric least (collection)
  (:documentation
    "On a set, returns two values: the smallest member of the set and
true; on a bag, returns three values: the smallest member of the bag, its
multiplicity, and true; on a map, also returns three values: the smallest key
of the map, its value, and true.  If there is not a unique smallest member,
chooses one of the smallest members arbitrarily.  Returns `nil' if the
collection is empty."))

(defgeneric greatest (collection)
  (:documentation
    "On a set, returns two values: the greatest member of the set and
true; on a bag, returns three values: the greatest member of the bag, its
multiplicity, and true; on a map, also returns three values: the greatest key
of the map, its value, and true.  If there is not a unique greatest member,
chooses one of the greatest members arbitrarily.  Returns `nil' if the
collection is empty."))

(defgeneric lookup (collection key)
  (:documentation
    "If `collection' is a map, returns the value to which `key' is mapped.
If `collection' is a seq, takes `key' as an index and returns the
corresponding member (0-origin, of course).  If `collection' is a set or
bag that contains a member equal to `key', returns true and the member
as two values, else false and `nil'; this is useful for canonicalization."))

(defgeneric rank (collection value)
  (:documentation
    "If `collection' is a set or bag that contains `value', returns the rank
of `value' in the ordering defined by `compare', and a true second value.
If `collection' is a map whose domain contains `value', returns the rank of
`value' in the domain of the map, and a true second value.  If `value' is
not in the collection, the second value is false, and the first value is the
rank of the greatest member of the collection less than `value' (if any;
otherwise -1).  Note that if there are values/keys that are unequal but
equivalent to `value', an arbitrary order will be imposed on them for this
purpose; but another collection that is `equal?' but not `eq' to this one
will in general order them differently.  Also, on a bag, multiplicities are
ignored for this purpose."))

(defgeneric at-rank (collection rank)
  (:documentation
    "On a set, returns the element with rank `rank'; on a bag, returns
that element with its multiplicity as a second value; on a map, returns
the pair with that rank as two values.  Note that if there are values/keys
that are unequal but equivalent in the collection, an arbitrary order will be
imposed on them for this purpose; but another collection that is `equal?'
but not `eq' to this one will in general order them differently."))

(defgeneric with (collection value1 &optional value2)
  (:documentation
    "On a set, adds `value1' to it, returning the updated set.  On a bag, adds
`value2' occurrences of `value1', returning the updated bag; `value2' defaults
to 1.  On a map, adds a mapping from `value1' (the key) to `value2', returning
the updated map.  On a seq, replaces the element at index `value1' with
`value2', returning the updated seq (the seq is extended in either direction
if needed; previously uninitialized indices are filled with the seq's default)."))

(defgeneric less (collection value1 &optional value2)
  (:documentation
    "On a set, removes `value1' from it if present, returning the updated set.
On a bag, removes `value2' occurrences of `value1' if present, returning the
updated bag; `value2' defaults to 1.  On a map, removes the pair whose key is
`value1', if present, returning the updated map.  On a seq, removes the element
at index `value1', if that index is in bounds, and shifts subsequent elements
down, returning the updated seq."))

(defgeneric union (set-or-bag1 set-or-bag2 &key)
  (:documentation
    "Returns the union of the two sets/bags.  The result is a set if both
arguments are sets; otherwise a bag.  The union of two bags is a bag whose
multiplicity, for any value, is the maximum of its multiplicities in the
two argument bags."))

(defgeneric bag-sum (bag1 bag2)
  (:documentation
    "Returns a bag whose multiplicity, for any value, is the sum of its
multiplicities in the two argument bags."))

(defgeneric intersection (set-or-bag1 set-or-bag2 &key)
  (:documentation
    "Returns the intersection of the two sets/bags.  The result is a bag
if both arguments are bags; otherwise a set.  The intersection of two bags
is the bag whose multiplicity, for any value, is the minimum of its
multiplicities in the two argument bags."))

(defgeneric bag-product (bag1 bag2)
  (:documentation
    "Returns a bag whose multiplicity, for any value, is the product of
its multiplicities in the two argument bags."))

(defgeneric set-difference (set1 set2 &key)
  (:documentation
    "Returns the set difference of set1 and set2, i.e., the set containing
every member of `set1' that is not in `set2'."))

(defgeneric set-difference-2 (set1 set2)
  (:documentation
    "Returns `set1 - set2' and `set2 - set1' as two values."))

(defgeneric bag-difference (bag1 bag2)
  (:documentation
    "Returns a bag whose multiplicity, for any value, is its multiplicity
in `bag1' less that in `bag2', but of course not less than zero."))

(defgeneric subset? (sub super)
  (:documentation "Returns true iff `sub' is a subset of `super'."))

(defgeneric disjoint? (set1 set2)
  (:documentation
    "Returns true iff `set1' and `set2' have a null intersection (without
actually constructing said intersection)."))

(defgeneric subbag? (sub super)
  (:documentation
    "Returns true iff `sub' is a subbag of `super', that is, for every
member of `sub', `super' contains the same value with at least the same
multiplicity."))

(defgeneric filter (fn collection)
  (:documentation
    "Returns a new collection containing those members or pairs of `collection'
for which `fn' returns true.  If `collection' is a set, bag, or seq, `fn' is
called with one argument; if a map, `fn' is called with two arguments, the key
and the value (the map-default of the result is that of `collection').  As well
as a Lisp function, `fn' can be a map, or a set (which is treated as mapping
its members to true and everything else to false)."))

(defgeneric partition (pred collection)
  (:documentation
    "Returns two values, (filter fn collection) and
\(filter (cl:complement fn) collection)."))

(defun split (pred collection)
  "Deprecated; use `partition'."
  (partition pred collection))

(defgeneric filter-pairs (fn collection)
  (:documentation
    "Just like `filter' except that if invoked on a bag, `fn' (which must be a
Lisp function) is called with two arguments for each pair, the member and the
multiplicity."))

(defmethod filter-pairs (fn (collection t))
  (filter fn collection))

(defgeneric image (fn collection)
  (:documentation
    "Returns a new collection containing the result of applying `fn' to each
member of `collection', which may be a set, bag, or seq.  In the bag case,
the multiplicity of each member of the result is the sum of the multiplicities
of the values that `fn' maps to it.  As well as a Lisp function, `fn' can be a
map, or a set (which is treated as mapping its members to true and everything
else to false).  `collection' can also be a map, in which case `fn' must be a
Lisp function of two arguments that returns two values (the map-default of the
result is that of `collection'); also see `compose'."))

;;; Convenience methods.
(defmethod image ((fn function) (l list))
  (mapcar fn l))

(defmethod image ((fn symbol) (l list))
  (mapcar (coerce fn 'function) l))

(defmethod image ((fn map) (l list))
  (mapcar (lambda (x) (@ fn x)) l))

(defmethod image ((fn set) (l list))
  (mapcar (lambda (x) (@ fn x)) l))

(defgeneric reduce (fn collection &key key initial-value)
  (:documentation
    "If `collection' is a Lisp sequence, this simply calls `cl:reduce' (q.v.).
On an FSet collection, the `:start', `:end', and `:from-end' keywords are
accepted only if `collection' is a seq."))

(defmethod reduce (fn (s sequence) &rest keyword-args
		   &key key initial-value start end from-end)
  (declare (dynamic-extent keyword-args)
	   (ignore key initial-value start end from-end))
  (apply #'cl:reduce fn s keyword-args))

(defgeneric domain (map)
  (:documentation
    "Returns the domain of the map, that is, the set of keys mapped by the map."))

;;; &&& Actually I think this should return a bag.  You can then convert it
;;; to a set if you want.
(defgeneric range (map)
  (:documentation
    "Returns the range of the map, that is, the set of all values to which keys
are mapped by the map."))

(defgeneric default (collection)
  (:documentation
    "Returns the default for the map or seq, i.e., the value returned by `lookup'
when the supplied key or index is not in the domain."))

(defgeneric with-default (collection new-default)
  (:documentation
    "Returns a new map or seq with the same contents as `collection' but whose
default is now `new-default'."))

(defgeneric map-union (map1 map2 &optional val-fn)
  (:documentation
    "Returns a map containing all the keys of `map1' and `map2', where the
value for each key contained in only one map is the value from that map, and
the value for each key contained in both maps is the result of calling
`val-fn' on the value from `map1' and the value from `map2'.  `val-fn'
defaults to simply returning its second argument, so the entries in `map2'
simply shadow those in `map1'.  The default for the new map is the result of
calling `val-fn' on the defaults for the two maps (so be sure it can take
these values)."))

(defgeneric map-intersection (map1 map2 &optional val-fn)
  (:documentation
    "Returns a map containing all the keys that are in the domains of both
`map1' and `map2', where the value for each key is the result of calling
`val-fn' on the value from `map1' and the value from `map2'.  `val-fn'
defaults to simply returning its second argument, so the entries in `map2'
simply shadow those in `map1'.  The default for the new map is the result
of calling `val-fn' on the defaults for the two maps (so be sure it can
take these values)."))

(defgeneric map-difference-2 (map1 map2)
  (:documentation
    "Returns, as two values: a map containing all the pairs that are in `map1'
but not `map2', with the same default as `map1'; and one containing all the
pairs that are in `map2' but not `map1', with the same default as `map2'."))

;;; Possible operation: `map-update' (better name??), which would be like
;;; `map-union' except the keys would be exactly the keys of `map1'.  This
;;; would be useful for removing items from chained maps:
;;;
;;; (map-update chained-map
;;;		(map (key1 (map (key2 (set val)))))
;;;		(fn (x y) (map-update x y #'set-difference)))
;;;
;;; If key1->key2->val is not already present, this returns `chained-map'.
;;;
;;; But another operation with a legitimate claim on the name would simply
;;; apply a function to the range value for a specified key:
;;;
;;; (map-update chained-map key1
;;;		(fn (m) (map-update m key2
;;;				    (fn (s) (less s val)))))

(defgeneric restrict (map set)
  (:documentation
    "Returns a map containing only those pairs of `map' whose keys are
also in `set'."))

;;; &&& Better name?
(defgeneric restrict-not (map set)
  (:documentation
    "Returns a map containing only those pairs of `map' whose keys are
not in `set'."))

(defgeneric compose (map1 map2-or-fn)
  (:documentation
    "Returns a new map with the same domain as `map1', which maps each member
of that domain to the result of applying first `map1' to it, then applying
`map2-or-fn' to the result.  `map2-or-fn' can also be a sequence, which is
treated as a map from indices to members."))

(defgeneric first (seq)
  (:documentation
    "Returns the first element of `seq', i.e., element 0.  This has a back-
compatibility method for lists, and adds one for CL sequences generally."))

(defmethod first ((s list))
  (cl:first s))

(defmethod first ((s sequence))
  (elt s 0))

(defgeneric last (seq)
  (:documentation
    "Returns the last element of `seq', i.e., element `(1- (size seq))'.  This
has methods for CL lists and sequences that are NOT COMPATIBLE with `cl:last'.
FSet exports `lastcons' as an arguably better name for the functionality of
`cl:last'."))

(defmethod last ((s list))
  (car (lastcons s)))

(defmethod last ((s sequence))
  (elt s (1- (length s))))

(defgeneric with-first (seq val)
  (:documentation
    "Returns `seq' with `val' prepended, that is, `val' is element 0 of the
result, and the elements of `seq' appear starting at index 1."))

(defgeneric with-last (seq val)
  (:documentation
    "Returns `seq' with `val' appended, that is, `val' is element `(size seq)'
of the result."))

(defgeneric less-first (seq)
  (:documentation
    "Returns the subsequence of `seq' from element 1 through the end."))

(defgeneric less-last (seq)
  (:documentation
    "Returns the subsequence of `seq' from element 0 through the next-to-last
element."))

(defgeneric insert (seq idx val)
  (:documentation
    "Returns a new sequence like `seq' but with `val' inserted at `idx' (the seq
is extended in either direction if needed prior to the insertion; previously
uninitialized indices are filled with the seq's default)."))

(defgeneric splice (seq idx subseq)
  (:documentation
    "Returns a new sequence like `seq' but with the elements of `subseq' inserted
at `idx' (the seq is extended in either direction if needed prior to the insertion;
previously uninitialized indices are filled with the seq's default)."))

;;; &&& Maybe we should shadow `concatenate' instead, so you can specify a
;;; result type.
(defgeneric concat (seq1 &rest seqs)
  (:documentation
    "Returns the concatenation of `seq1' with each of `seqs'."))


;;; This is the opposite order from `cl:coerce', but I like it better, because I
;;; think the calls are easier to read with the type first.  It's also consistent
;;; with `cl:concatenate' -- the inconsistency between `coerce' and `concatenate'
;;; has long bugged me.
(defgeneric convert (to-type collection &key)
  (:documentation "Converts the collection to the specified type.  Some methods may
take additional keyword arguments to further specify the kind of conversion."))

;;; The `&allow-other-keys' is to persuade SBCL not to issue warnings about keywords
;;; that are accepted by some methods of `convert'.
(declaim (ftype (function (t t &key &allow-other-keys) t) convert))


;;; ================================================================================
;;; Iterators

;;; Rationale:
;;; () The use of a closure allows implementation genericity without requiring
;;; a CLOS dispatch on each iteration.
;;; () There are several ways to use this iterator.  You can explicitly call
;;; either sense of the termination predicate -- both senses are provided as
;;; a stylistic convenience -- and then use the `:get' method separately.  Or,
;;; if you are going for maximum speed, you can just use `:get'; if you know
;;; your collection doesn't contain `nil', you can just look at the first value
;;; to check termination; if it might contain `nil', you can use the extra value.
(defgeneric iterator (collection &key)
  (:documentation
    "Returns an iterator for the collection.  (These are stateful iterators and
are not thread-safe; if you want a pure iterator, your best bet is to `convert'
the collection to a list.)  The iterator is a closure of one argument; given
`:done?', it returns true iff the iterator is exhausted; given `:more?', it
returns true iff the iterator is _not_ exhausted.  Given `:get', if the iterator
is not exhausted, it returns the next element (or pair, for a map, as two values),
with the second value (third, for a map) being true, and advances one element; if
it is exhausted, it returns two `nil' values (three, for a map)."))

;;; The `&allow-other-keys' is to persuade SBCL not to issue warnings about keywords
;;; that are accepted by some methods of `iterator'.
(declaim (ftype (function (t &key &allow-other-keys) function) iterator))

;;; Iterators for the Lisp sequence types are useful for some generic operations
;;; (e.g. `some' and friends).
(defmethod iterator ((ls list) &key)
  (lambda (op)
    (ecase op
      (:get (if ls (values (pop ls) t)
	      (values nil nil)))
      (:done? (null ls))
      (:more? ls))))

(defmethod iterator ((vec vector) &key)
  (let ((idx 0)
	(len (length vec)))
    ;; You might think this could be more elegantly done by defining a method on
    ;; `simple-vector', but the CL standard does not require `simple-vector' to
    ;; be a class (and it isn't in Allegro).
    (if (simple-vector-p vec)
	(lambda (op)
	  (ecase op
	    (:get (if (< idx len) (values (prog1 (svref vec idx) (incf idx)) t)
		    (values nil nil)))
	    (:done? (>= idx len))
	    (:more? (< idx len))))
      (lambda (op)
	(ecase op
	  (:get (if (< idx len) (values (prog1 (aref vec idx) (incf idx)) t)
		  (values nil nil)))
	  (:done? (>= idx len))
	  (:more? (< idx len)))))))

(defmethod iterator ((str string) &key)
  (let ((idx 0)
	(len (length str)))
    ;; You might think this could be more elegantly done by defining a method on
    ;; `simple-string', but the CL standard does not require `simple-string' to
    ;; be a class (and it isn't in Allegro).
    (if (simple-string-p str)
	(lambda (op)
	  (ecase op
	    (:get (if (< idx len) (values (prog1 (schar str idx) (incf idx)) t)
		    (values nil nil)))
	    (:done? (>= idx len))
	    (:more? (< idx len))))
      (lambda (op)
	(ecase op
	  (:get (if (< idx len) (values (prog1 (char str idx) (incf idx)) t)
		  (values nil nil)))
	  (:done? (>= idx len))
	  (:more? (< idx len)))))))

;;; If an implementation has any more concrete subtypes of `sequence' besides
;;; those above, this method will cover them.  Note, this is `cl:sequence' we're
;;; talking about here.
(defmethod iterator ((seq sequence) &key)
  (let ((idx 0)
	(len (length seq)))
    (lambda (op)
      (ecase op
	(:get (if (< idx len) (values (prog1 (elt seq idx) (incf idx)) t)
		(values nil nil)))
	(:done? (>= idx len))
	(:more? (< idx len))))))

(def-gmap-arg-type :sequence (seq)
  "Yields the elements of `seq', which can be of any CL sequence type as well
as an FSet seq, or a set or bag as well."
  `((iterator ,seq)
    #'(lambda (it) (declare (type function it)) (funcall it ':done?))
    #'(lambda (it) (declare (type function it)) (funcall it ':get))))

;;; ================================================================================
;;; Generic versions of Common Lisp sequence functions

(defgeneric subseq (seq start &optional end)
  (:documentation
    "Returns the subsequence of `seq' from `start' (inclusive) to `end' (exclusive),
where `end' defaults to `(size seq)'."))

(defmethod subseq ((s sequence) start &optional end)
  (cl:subseq s start end))

(defgeneric reverse (seq)
  (:documentation
    "Returns `seq' in reverse order."))

(defmethod reverse ((s sequence))
  (cl:reverse s))

(defgeneric sort (seq pred &key key)
  (:documentation
    "Returns `seq' sorted by `pred', a function of two arguments; if `key' is
supplied, it is a function of one argument that is applied to the elements of
`seq' before they are passed to `pred'.  The sort is not guaranteed to be
stable.  The method for CL sequences copies the sequence first, unlike
`cl:sort'."))

(defmethod sort ((s sequence) pred &key key)
  (cl:sort (cl:copy-seq s) pred :key key))

(defgeneric stable-sort (seq pred &key key)
  (:documentation
    "Returns `seq' sorted by `pred', a function of two arguments; if `key' is
supplied, it is a function of one argument that is applied to the elements of
`seq' before they are passed to `pred'.  The sort is guaranteed to be stable.
The method for CL sequences copies the sequence first, unlike `cl:stable-sort'."))

(defmethod stable-sort ((s sequence) pred &key key)
  (cl:stable-sort (cl:copy-seq s) pred :key key))

(defgeneric sort-and-group (seq pred &key key)
  (:documentation
    "Like 'stable-sort', but additionally groups the result, returning a seq of seqs,
where the elements of each inner seq are equal according to `pred' and, optionally,
`key'."))

(defgeneric find (item collection &key key test)
  (:documentation
    "If `collection' is a Lisp sequence, this simply calls `cl:find'.  On an FSet
collection, the default for `test' is `equal?'; the `:test-not' keyword is not
accepted; and the `:start', `:end', and `:from-end' keywords are accepted only
if `collection' is a seq.  Also, on a map, this scans the domain; on success,
it returns the corresponding range element as the second value."))

(defmethod find (item (s sequence) &rest keyword-args
		      &key key test test-not start end from-end)
  (declare (dynamic-extent keyword-args)
	   (ignore key test test-not start end from-end))
  (apply #'cl:find item s keyword-args))

(defgeneric find-if (pred collection &key key)
  (:documentation
    "If `collection' is a Lisp sequence, this simply calls `cl:find-if'.  On an
FSet collection, the `:start', `:end', and `:from-end' keywords are accepted
only if `collection' is a seq.  Also, on a map, this scans the domain; on
success, it returns the corresponding range element as the second value."))

(defmethod find-if (pred (s sequence) &rest keyword-args &key key start end from-end)
  (declare (dynamic-extent keyword-args)
	   (ignore key start end from-end))
  (apply #'cl:find-if pred s keyword-args))

(defgeneric find-if-not (pred collection &key key)
  (:documentation
    "If `collection' is a Lisp sequence, this simply calls `cl:find-if-not'.
On an FSet collection, the `:start', `:end', and `:from-end' keywords are
accepted only if `collection' is a seq.  Also, on a map, this scans the domain;
on success, it returns the corresponding range element as the second value."))

(defmethod find-if-not (pred (s sequence) &rest keyword-args &key key start end from-end)
  (declare (dynamic-extent keyword-args)
	   (ignore key start end from-end))
  (apply #'cl:find-if-not pred s keyword-args))

(defgeneric count (item collection &key key test)
  (:documentation
    "If `collection' is a Lisp sequence, this simply calls `cl:count'.  On an FSet
collection, the default for `test' is `equal?'; the `:test-not' keyword is not
accepted; and the `:start', `:end', and `:from-end' keywords are accepted only
if `collection' is a seq.  Also, on a map, this scans the domain."))

(defmethod count (item (s sequence) &rest keyword-args
		       &key key test test-not start end from-end)
  (declare (dynamic-extent keyword-args)
	   (ignore key test test-not start end from-end))
  (apply #'cl:count item s keyword-args))

(defgeneric count-if (pred collection &key key)
  (:documentation
    "If `collection' is a Lisp sequence, this simply calls `cl:count-if'.  On an
FSet collection, the `:start', `:end', and `:from-end' keywords are accepted
only if `collection' is a seq.  Also, on a map, this scans the domain."))

(defmethod count-if (pred (s sequence) &rest keyword-args &key key start end from-end)
  (declare (dynamic-extent keyword-args)
	   (ignore key start end from-end))
  (apply #'cl:count-if pred s keyword-args))

(defgeneric count-if-not (pred collection &key key)
  (:documentation
    "If `collection' is a Lisp sequence, this simply calls `cl:count-if-not'.
On an FSet collection, the `:start', `:end', and `:from-end' keywords are
accepted only if `collection' is a seq.  Also, on a map, this scans the domain."))

(defmethod count-if-not (pred (s sequence) &rest keyword-args &key key start end from-end)
  (declare (dynamic-extent keyword-args)
	   (ignore key start end from-end))
  (apply #'cl:count-if-not pred s keyword-args))

(defgeneric position (item collection &key key test start end from-end)
  (:documentation
    "If `collection' is a Lisp sequence, this simply calls `cl:position'.  On an
FSet seq, the default for `test' is `equal?', and the `:test-not' keyword is
not accepted."))

(defmethod position (item (s sequence) &rest keyword-args
			  &key key test test-not start end from-end)
  (declare (dynamic-extent keyword-args)
	   (ignore key test test-not start end from-end))
  (apply #'cl:position item s keyword-args))

(defgeneric position-if (pred collection &key key start end from-end)
  (:documentation
    "If `collection' is a Lisp sequence, this simply calls `cl:position-if'.
Also works on an FSet seq."))

(defmethod position-if (pred (s sequence) &rest keyword-args &key key start end from-end)
  (declare (dynamic-extent keyword-args)
	   (ignore key start end from-end))
  (apply #'cl:position-if pred s keyword-args))

(defgeneric position-if-not (pred collection &key key start end from-end)
  (:documentation
    "If `collection' is a Lisp sequence, this simply calls `cl:position-if-not'.
Also works on an FSet seq."))

(defmethod position-if-not (pred (s sequence) &rest keyword-args
				 &key key start end from-end)
  (declare (dynamic-extent keyword-args)
	   (ignore key start end from-end))
  (apply #'cl:position-if-not pred s keyword-args))

(defgeneric remove (item collection &key key test)
  (:documentation
    "If `collection' is a Lisp sequence, this simply calls `cl:remove'.  On an
FSet seq, the default for `test' is `equal?', and the `:test-not' keyword is
not accepted."))

(defmethod remove (item (s sequence) &rest keyword-args
			&key key test start end from-end count)
  (declare (dynamic-extent keyword-args)
	   (ignore key test start end from-end count))
  (apply #'cl:remove item s keyword-args))

(defgeneric remove-if (pred collection &key key)
  (:documentation
    "If `collection' is a Lisp sequence, this simply calls `cl:remove-if'.
Also works on an FSet seq; but see `filter'."))

(defmethod remove-if (pred (s sequence) &rest keyword-args
			   &key key start end from-end count)
  (declare (dynamic-extent keyword-args)
	   (ignore key start end from-end count))
  (apply #'cl:remove-if pred s keyword-args))

(defgeneric remove-if-not (pred collection &key key)
  (:documentation
    "If `collection' is a Lisp sequence, this simply calls `cl:remove-if-not'.
Also works on an FSet seq; but see `filter'."))

(defmethod remove-if-not (pred (s sequence) &rest keyword-args
			       &key key start end from-end count)
  (declare (dynamic-extent keyword-args)
	   (ignore key start end from-end count))
  (apply #'cl:remove-if-not pred s keyword-args))

(defgeneric substitute (newitem olditem collection &key key)
  (:documentation
    "If `collection' is a Lisp sequence, this simply calls `cl:substitute'.  On
an FSet seq, the default for `test' is `equal?', and the `:test-not' keyword
is not accepted."))

(defmethod substitute (newitem olditem (s sequence) &rest keyword-args
			       &key key test start end from-end count)
  (declare (dynamic-extent keyword-args)
	   (ignore key test start end from-end count))
  (apply #'cl:substitute newitem olditem s keyword-args))

(defgeneric substitute-if (newitem pred collection &key key)
  (:documentation
    "If `collection' is a Lisp sequence, this simply calls `cl:substitute-if'.
Also works on an FSet seq."))

(defmethod substitute-if (newitem pred (s sequence) &rest keyword-args
				  &key key start end from-end count)
  (declare (dynamic-extent keyword-args)
	   (ignore key start end from-end count))
  (apply #'cl:substitute-if newitem pred s keyword-args))

(defgeneric substitute-if-not (newitem pred collection &key key)
  (:documentation
    "If `collection' is a Lisp sequence, this simply calls `cl:substitute-if-not'.
Also works on an FSet seq."))

(defmethod substitute-if-not (newitem pred (s sequence) &rest keyword-args
				      &key key start end from-end count)
  (declare (dynamic-extent keyword-args)
	   (ignore key start end from-end count))
  (apply #'cl:substitute-if-not newitem pred s keyword-args))

;;; `(gmap :or ...)' is a bit faster.
(defun some (pred sequence0 &rest more-sequences)
  "FSet generic version of `cl:some'."
  (let ((it0 (iterator sequence0))
	(more-its (mapcar #'iterator more-sequences))
	(pred (coerce pred 'function)))
    (do ()
	((or (funcall it0 ':done?)
	     (gmap :or (lambda (it) (funcall it ':done?))
		   (:list more-its)))
	 nil)
      (let ((val (apply pred (funcall it0 ':get) (mapcar (lambda (it) (funcall it ':get))
							 more-its))))
	(when val
	  (return val))))))

;;; `(gmap :and ...)' is a bit faster.
(defun every (pred sequence0 &rest more-sequences)
  "FSet generic version of `cl:every'."
  (let ((it0 (iterator sequence0))
	(more-its (mapcar #'iterator more-sequences))
	(pred (coerce pred 'function)))
    (do ()
	((or (funcall it0 ':done?)
	     (gmap :or (lambda (it) (funcall it ':done?))
		   (:list more-its)))
	 t)
      (let ((val (apply pred (funcall it0 ':get) (mapcar (lambda (it) (funcall it ':get))
							 more-its))))
	(when (not val)
	  (return nil))))))

(defun notany (pred sequence0 &rest more-sequences)
  "FSet generic version of `cl:notany'."
  (not (apply #'some pred sequence0 more-sequences)))

(defun notevery (pred sequence0 &rest more-sequences)
  "FSet generic version of `cl:notevery'."
  (not (apply #'every pred sequence0 more-sequences)))


(defmethod union ((ls1 list) (ls2 list) &rest keyword-args &key test test-not)
  (declare (dynamic-extent keyword-args)
	   (ignore test test-not))
  (apply #'cl:union ls1 ls2 keyword-args))

(defmethod intersection ((ls1 list) (ls2 list) &rest keyword-args &key test test-not)
  (declare (dynamic-extent keyword-args)
	   (ignore test test-not))
  (apply #'cl:intersection ls1 ls2 keyword-args))

(defmethod set-difference ((ls1 list) (ls2 list) &rest keyword-args &key test test-not)
  (declare (dynamic-extent keyword-args)
	   (ignore test test-not))
  (apply #'cl:set-difference ls1 ls2 keyword-args))


;;; ================================================================================
;;; FSet methods for CL sequences

(defmethod concat ((a list) &rest seqs)
  (append a (reduce #'append seqs :key (fn (x) (convert 'list x)) :from-end t)))

(defmethod convert ((to-type (eql 'list)) (ls list) &key)
  ls)

(defmethod convert ((to-type (eql 'vector)) (v vector) &key)
  v)

(defmethod partition ((pred symbol) (ls list))
  (list-partition (coerce pred 'function) ls))

(defmethod partition ((pred function) (ls list))
  (list-partition pred ls))

(defun list-partition (pred ls)
  (declare (optimize (speed 3) (safety 0))
	   (type function pred))
  (let ((res1 nil)
	(res2 nil))
    (dolist (x ls)
      (if (funcall pred x)
	  (push x res1)
	(push x res2)))
    (values (nreverse res1) (nreverse res2))))


;;; ================================================================================
;;; New names for a few existing CL functions

(declaim (inline lastcons head tail))

;;; The CL function is poorly (albeit traditionally) named, and we shadow the name.
(defun lastcons (list)
  "Returns the last cons of `list'.  This is a renaming of the CL function `last'."
  (cl:last list))

(defun head (list)
  "Another name for the `car' operation on lists."
  (car list))

(defun tail (list)
  "Another name for the `cdr' operation on lists."
  (cdr list))

(declaim (inline lastcons head tail))


;;; ================================================================================
;;; SETF expanders, modify methods, etc.

(define-setf-expander lookup (collection key &environment env)
  "Adds a pair to a map or updates an existing pair, or adds an element to a
sequence or updates an existing element.  This does NOT modify the map or
sequence; it modifies the place (generalized variable) HOLDING the map or
sequence (just like `(setf (ldb ...) ...)').  That is, the `collection' subform
must be `setf'able itself."
  (let ((temps vals stores store-form access-form
	  (get-setf-expansion collection env))
	(key-temp (gensym))
	(val-temp (gensym))
	((coll-temp (car stores))))
    (when (cdr stores)
      (error "Too many values required in `setf' of `lookup'"))
    (values (cons key-temp temps)
	    (cons key vals)
	    (list val-temp)
	    `(let ((,coll-temp (with ,access-form ,key-temp ,val-temp)))
	       ,store-form
	       ,val-temp)
	    `(lookup ,access-form ,key-temp))))

;;; Have to do the same thing for `@' since `setf' would not know what to
;;; do with `@'s normal expansion.
(define-setf-expander @ (collection key &environment env)
  "Adds a pair to a map or updates an existing pair, or adds an element to a
sequence or updates an existing element.  This does NOT modify the map or
sequence; it modifies the place (generalized variable) HOLDING the map or
sequence (just like `(setf (ldb ...) ...)').  That is, the `collection' subform
must be `setf'able itself."
  (let ((temps vals stores store-form access-form
	  (get-setf-expansion collection env))
	(key-temp (gensym))
	(val-temp (gensym))
	((coll-temp (car stores))))
    (when (cdr stores)
      (error "Too many values required in `setf' of `@'"))
    (values (cons key-temp temps)
	    (cons key vals)
	    (list val-temp)
	    `(let ((,coll-temp (with ,access-form ,key-temp ,val-temp)))
	       ,store-form
	       ,val-temp)
	    `(lookup ,access-form ,key-temp))))

;;; ================================================================================
;;; Functional deep update

(defun update (fn coll &rest keys)
  "Returns a new version of `coll' in which the element reached by doing chained
`lookup's on `keys' is updated by `fn'.  An example will help a lot here:
instead of writing

  (incf (@ (@ (@ foo 'a) 3) 7))

you can write, equivalently

  (setq foo (update #'1+ foo 'a 3 7))

This is perhaps most useful in contexts where you don't want to do the `setq'
anyway."
  (labels ((rec (fn coll keys)
	     (if (null keys) (@ fn coll)
	       (with coll (car keys) (rec fn (lookup coll (car keys)) (cdr keys))))))
    (rec fn coll keys)))

;;; If the `fn' is nontrivial, binds a variable to it with a `dynamic-extent' declaration.
;;; (Really, should do this for `image', `filter', etc. etc.)
(define-compiler-macro update (&whole form fn coll &rest keys)
  (if (not (or (symbolp fn)
	       (and (listp fn)
		    (eq (car fn) 'function)
		    (symbolp (cadr fn)))))
      (let ((fn-var (gensym "FN-")))
	`(let ((,fn-var ,fn))
	   (declare (dynamic-extent ,fn-var))
	   ; (expansion terminates because `fn-var' is a symbol)
	   (update ,fn-var ,coll . ,keys)))
    form))


;;; ================================================================================
;;; Sets

;;; Note that while many of these methods are defined on `wb-set', some of them are
;;; written generically; I have left these defined on `set'.  Also, the assumption
;;; that `wb-set' is the default implementation is hard-coded at the moment.


(defstruct (wb-set
	    (:include set)
	    (:constructor make-wb-set (contents))
	    (:predicate wb-set?)
	    (:print-function print-wb-set)
	    (:copier nil))
  "A class of functional sets represented as weight-balanced binary trees.  This is
the default implementation of sets in FSet."
  contents)


(defparameter *empty-wb-set* (make-wb-set nil))

(declaim (inline empty-set))
(defun empty-set ()
  "Returns an empty set of the default implementation."
  *empty-wb-set*)

(declaim (inline empty-wb-set))
(defun empty-wb-set ()
  "Returns an empty wb-set."
  *empty-wb-set*)

(defmethod empty? ((s wb-set))
  (null (wb-set-contents s)))

(defmethod size ((s wb-set))
  (WB-Set-Tree-Size (wb-set-contents s)))

(defmethod set-size ((s wb-set))
  (WB-Set-Tree-Size (wb-set-contents s)))

(defmethod arb ((s wb-set))
  (let ((tree (wb-set-contents s)))
    (if tree (values (WB-Set-Tree-Arb tree) t)
      (values nil nil))))

(defmethod contains? ((s wb-set) x &optional (y nil y?))
  (declare (ignore y))
  (check-two-arguments y? 'contains? 'wb-set)
  (WB-Set-Tree-Member? (wb-set-contents s) x))

;;; Note, first value is `t' or `nil'.
(defmethod lookup ((s wb-set) key)
  (WB-Set-Tree-Find-Equal (wb-set-contents s) key))

(defmethod rank ((s wb-set) x)
  (let ((found? rank (WB-Set-Tree-Rank (wb-set-contents s) x)))
    (values (if found? rank (1- rank)) found?)))

(defmethod at-rank ((s wb-set) rank)
  (let ((contents (wb-set-contents s))
	((size (WB-Set-Tree-Size contents))))
    (unless (and (>= rank 0) (< rank size))
      (error 'simple-type-error :datum rank :expected-type `(integer 0 (,size))
	     :format-control "Rank ~D out of bounds on ~A"
	     :format-arguments (list rank s)))
    (WB-Set-Tree-Rank-Element contents rank)))

(defmethod least ((s wb-set))
  (let ((tree (wb-set-contents s)))
    (if tree (values (WB-Set-Tree-Least tree) t)
      (values nil nil))))

(defmethod greatest ((s wb-set))
  (let ((tree (wb-set-contents s)))
    (and tree (values (WB-Set-Tree-Greatest tree) t))))

(defmethod with ((s wb-set) value &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'with 'wb-set)
  (let ((contents (wb-set-contents s))
	((new-contents (WB-Set-Tree-With contents value))))
    (if (eq new-contents contents)
	s
      (make-wb-set new-contents))))

(defmethod less ((s wb-set) value &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'wb-set)
  (let ((contents (wb-set-contents s))
	((new-contents (WB-Set-Tree-Less contents value))))
    (if (eq new-contents contents)
	s
      (make-wb-set new-contents))))

(defmethod split-from ((s wb-set) value)
  (let ((new-contents (WB-Set-Tree-Split-Above (wb-set-contents s) value)))
    (make-wb-set (if (WB-Set-Tree-Member? (wb-set-contents s) value)
		     (WB-Set-Tree-With new-contents value)
		   new-contents))))

(defmethod split-above ((s wb-set) value)
  (make-wb-set (WB-Set-Tree-Split-Above (wb-set-contents s) value)))

(defmethod split-through ((s wb-set) value)
  (let ((new-contents (WB-Set-Tree-Split-Below (wb-set-contents s) value)))
    (make-wb-set (if (WB-Set-Tree-Member? (wb-set-contents s) value)
		     (WB-Set-Tree-With new-contents value)
		   new-contents))))

(defmethod split-below ((s wb-set) value)
  (make-wb-set (WB-Set-Tree-Split-Below (wb-set-contents s) value)))

(defmethod union ((s1 wb-set) (s2 wb-set) &key)
  (make-wb-set (WB-Set-Tree-Union (wb-set-contents s1) (wb-set-contents s2))))

(defmethod intersection ((s1 wb-set) (s2 wb-set) &key)
  (make-wb-set (WB-Set-Tree-Intersect (wb-set-contents s1) (wb-set-contents s2))))

(defmethod set-difference ((s1 wb-set) (s2 wb-set) &key)
  (make-wb-set (WB-Set-Tree-Diff (wb-set-contents s1) (wb-set-contents s2))))

(defmethod set-difference-2 ((s1 wb-set) (s2 wb-set))
  (let ((newc1 newc2 (WB-Set-Tree-Diff-2 (wb-set-contents s1) (wb-set-contents s2))))
    (values (make-wb-set newc1) (make-wb-set newc2))))

(defmethod subset? ((s1 wb-set) (s2 wb-set))
  (WB-Set-Tree-Subset? (wb-set-contents s1) (wb-set-contents s2)))

(defmethod disjoint? ((s1 wb-set) (s2 wb-set))
  (WB-Set-Tree-Disjoint? (wb-set-contents s1) (wb-set-contents s2)))

(defmethod compare ((s1 wb-set) (s2 wb-set))
  (WB-Set-Tree-Compare (wb-set-contents s1) (wb-set-contents s2)))

(defgeneric internal-do-set (set elt-fn value-fn)
  (:documentation
    "Calls `elt-fn' on successive elements of the set; when done, calls `value-fn'
on no arguments and returns the result(s).  This is called by `do-set' to provide
for the possibility of different set implementations; it is not for public use.
`elt-fn' and `value-fn' must be function objects, not symbols."))

(defmethod internal-do-set ((s wb-set) elt-fn value-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  ;; Expect Python note about "can't use known return convention"
  (Do-WB-Set-Tree-Members (x (wb-set-contents s) (funcall value-fn))
    (funcall elt-fn x)))

(defmethod iterator ((s wb-set) &key)
  (Make-WB-Set-Tree-Iterator (wb-set-contents s)))

(defmethod filter ((pred function) (s set))
  (set-filter pred s))

(defmethod filter ((pred symbol) (s set))
  (set-filter (coerce pred 'function) s))

(defmethod filter ((pred map) (s set))
  (set-filter #'(lambda (x) (lookup pred x)) s))

(defun set-filter (pred s)
  (declare (optimize (speed 3) (safety 0))
	   (type function pred))
  (let ((result nil))
    (do-set (x s)
      (when (funcall pred x)
	(setq result (WB-Set-Tree-With result x))))
    (make-wb-set result)))

(defmethod partition ((pred function) (s set))
  (set-partition pred s))

(defmethod partition ((pred symbol) (s set))
  (set-partition (coerce pred 'function) s))

(defmethod partition ((pred map) (s set))
  (set-partition #'(lambda (x) (lookup pred x)) s))

(defun set-partition (pred s)
  (declare (optimize (speed 3) (safety 0))
	   (type function pred))
  (let ((result-1 nil)
	(result-2 nil))
    (do-set (x s)
      (if (funcall pred x)
	  (setq result-1 (WB-Set-Tree-With result-1 x))
	(setq result-2 (WB-Set-Tree-With result-2 x))))
    (values (make-wb-set result-1)
	    (make-wb-set result-2))))

;;; A set is another kind of boolean-valued map.
(defmethod filter ((pred set) (s set))
  (intersection pred s))

;;; A bag is yet another kind of boolean-valued map.
(defmethod filter ((pred bag) (s set))
  (intersection pred s))

(defmethod image ((fn function) (s set))
  (set-image fn s))

(defmethod image ((fn symbol) (s set))
  (set-image (coerce fn 'function) s))

(defmethod image ((fn map) (s set))
  (set-image fn s))

(defmethod image ((fn set) (s set))
  (set-image fn s))

(defmethod image ((fn bag) (s set))
  (set-image fn s))

(defun set-image (fn s)
  (let ((result nil))
    (do-set (x s)
      (setq result (WB-Set-Tree-With result (@ fn x))))
    (make-wb-set result)))

(defmethod reduce ((fn function) (s set) &key key (initial-value nil init?))
  (set-reduce fn s initial-value (and key (coerce key 'function)) init?))

(defmethod reduce ((fn symbol) (s set) &key key (initial-value nil init?))
  (set-reduce (coerce fn 'function) s initial-value (and key (coerce key 'function))
	      init?))

(defun set-reduce (fn s initial-value key init?)
  (declare (optimize (speed 3) (safety 0))
	   (type function fn)
	   (type (or function null) key))
  (let ((result initial-value)
	(call-fn? init?))
    (if (and (not init?) (empty? s))
	(setq result (funcall fn))
      (do-set (x s)
	(if call-fn?
	    (setq result (funcall fn result (if key (funcall key x) x)))
	  (setq result (if key (funcall key x) x)
		call-fn? t))))
    result))

;;; For convenience.  Note that it always returns a seq.
(defmethod sort ((s set) pred &key key)
  (convert 'seq (cl:sort (convert 'vector s) pred :key key)))

;;; For convenience.  Note that it always returns a seq.
(defmethod stable-sort ((s set) pred &key key)
  (convert 'seq (cl:stable-sort (convert 'vector s) pred :key key)))

(defmethod convert ((to-type (eql 'set)) (s set) &key)
  s)

(defmethod convert ((to-type (eql 'wb-set)) (s wb-set) &key)
  s)

(defmethod convert ((to-type (eql 'bag)) (s wb-set) &key)
  (make-wb-bag (WB-Set-Tree-To-Bag-Tree (wb-set-contents s))))

(defmethod convert ((to-type (eql 'wb-bag)) (s wb-set) &key)
  (make-wb-bag (WB-Set-Tree-To-Bag-Tree (wb-set-contents s))))

(defmethod convert ((to-type (eql 'list)) (s set) &key)
  (declare (optimize (speed 3)))
  (let ((result nil))
    (do-set (x s)
      (push x result))
    (nreverse result)))

(defmethod convert ((to-type (eql 'vector)) (s set) &key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((result (make-array (the fixnum (size s))))
	(i 0))
    (declare (type fixnum i))
    (do-set (x s)
      (setf (svref result i) x)
      (incf i))
    result))

(defmethod convert ((to-type (eql 'set)) (l list) &key)
  (make-wb-set (WB-Set-Tree-From-List l)))

(defmethod convert ((to-type (eql 'wb-set)) (l list) &key)
  (make-wb-set (WB-Set-Tree-From-List l)))

(defmethod convert ((to-type (eql 'set)) (s sequence) &key)
  (make-wb-set (WB-Set-Tree-From-CL-Sequence s)))

(defmethod convert ((to-type (eql 'wb-set)) (s sequence) &key)
  (make-wb-set (WB-Set-Tree-From-CL-Sequence s)))

(defmethod find (item (s set) &key key test)
  (declare (optimize (speed 3) (safety 0)))
  (if key
      (let ((key (coerce key 'function)))
	(if test
	    (let ((test (coerce test 'function)))
	      (do-set (x s)
		(when (funcall test item (funcall key x))
		  (return x))))
	  (do-set (x s)
	    (when (equal? item (funcall key x))
	      (return x)))))
    (if (and test (not (or (eq test 'equal?) (eq test #'equal?))))
	(let ((test (coerce test 'function)))
	  (do-set (x s)
	    (when (funcall test item x)
	      (return x))))
      (let ((val? val (lookup s item)))
	(declare (ignore val?))
	val))))

(defmethod find-if (pred (s set) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function)))
    (if key
	(let ((key (coerce key 'function)))
	  (do-set (x s)
	    (when (funcall pred (funcall key x))
	      (return x))))
      (do-set (x s)
	(when (funcall pred x)
	  (return x))))))

(defmethod find-if-not (pred (s set) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function)))
    (find-if #'(lambda (x) (not (funcall pred x))) s :key key)))

(defmethod count (item (s set) &key key test)
  (declare (optimize (speed 3) (safety 0)))
  (let ((total 0))
    (declare (fixnum total))
    (if key
	(let ((key (coerce key 'function)))
	  (if test
	      (let ((test (coerce test 'function)))
		(do-set (x s total)
		  (when (funcall test item (funcall key x))
		    (incf total))))
	    (do-set (x s total)
	      (when (equal? item (funcall key x))
		(incf total)))))
      (if (and test (not (or (eq test 'equal?) (eq test #'equal?))))
	  (let ((test (coerce test 'function)))
	    (do-set (x s total)
	      (when (funcall test item x)
		(incf total))))
	(let ((val? val (lookup s item)))
	  (declare (ignore val))
	  (if val? 1 0))))))

(defmethod count-if (pred (s set) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function))
	(n 0))
    (declare (fixnum n))
    (if key
	(let ((key (coerce key 'function)))
	  (do-set (x s n)
	    (when (funcall pred (funcall key x))
	      (incf n))))
      (do-set (x s n)
	(when (funcall pred x)
	  (incf n))))))

(defmethod count-if-not (pred (s set) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function)))
    (count-if #'(lambda (x) (not (funcall pred x))) s :key key)))

(defun print-wb-set (set stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "#{" :suffix " }")
    (do-set (x set)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (write x :stream stream))))

(def-gmap-arg-type :set (set)
  "Yields the elements of `set'."
  `((iterator ,set)
    #'(lambda (it) (declare (type function it)) (funcall it ':done?))
    #'(lambda (it) (declare (type function it)) (funcall it ':get))))

(def-gmap-res-type :set (&key filterp)
  "Returns a set of the values, optionally filtered by `filterp'."
  `(nil #'WB-Set-Tree-With #'make-wb-set ,filterp))


;;; A bit faster than `:set', if you know it's a `wb-set'.
(def-gmap-arg-type :wb-set (set)
  "Yields the elements of `set'."
  `((Make-WB-Set-Tree-Iterator-Internal (wb-set-contents ,set))
    #'WB-Set-Tree-Iterator-Done?
    #'WB-Set-Tree-Iterator-Get))

(def-gmap-res-type :wb-set (&key filterp)
  "Returns a set of the values, optionally filtered by `filterp'."
  `(nil #'WB-Set-Tree-With #'make-wb-set ,filterp))


(def-gmap-res-type :union (&key filterp)
  "Returns the union of the values, optionally filtered by `filterp'."
  `((set) #'union nil ,filterp))

(def-gmap-res-type :intersection (&key filterp)
  "Returns the intersection of the values, optionally filtered by `filterp'."
  `((complement (set)) #'intersection nil ,filterp))


;;; ================================================================================
;;; Bags

(defstruct (wb-bag
	    (:include bag)
	    (:constructor make-wb-bag (contents))
	    (:predicate wb-bag?)
	    (:print-function print-wb-bag)
	    (:copier nil))
  "A class of functional bags (multisets) represented as weight-balanced binary
trees.  This is the default implementation of bags in FSet."
  contents)


(defparameter *empty-wb-bag* (make-wb-bag nil))

(declaim (inline empty-bag))
(defun empty-bag ()
  "Returns an empty bag of the default implementation."
  *empty-wb-bag*)

(declaim (inline empty-wb-bag))
(defun empty-wb-bag ()
  "Returns an empty wb-bag."
  *empty-wb-bag*)

(defmethod empty? ((b wb-bag))
  (null (wb-bag-contents b)))

(defmethod arb ((m wb-bag))
  (let ((tree (wb-bag-contents m)))
    (if tree
	(let ((val mult (WB-Bag-Tree-Arb-Pair tree)))
	  (values val mult t))
      (values nil nil nil))))

(defmethod contains? ((b wb-bag) x &optional (y nil y?))
  (declare (ignore y))
  (check-two-arguments y? 'contains? 'wb-bag)
  (plusp (WB-Bag-Tree-Multiplicity (wb-bag-contents b) x)))

(defmethod lookup ((b wb-bag) x)
  (let ((mult value-found (WB-Bag-Tree-Multiplicity (wb-bag-contents b) x)))
    (if (plusp mult)
	(values t value-found)
      (values nil nil))))

(defmethod rank ((s wb-bag) x)
  (let ((found? rank (WB-Bag-Tree-Rank (wb-bag-contents s) x)))
    (values (if found? rank (1- rank)) found?)))

(defmethod at-rank ((s wb-bag) rank)
  (let ((contents (wb-bag-contents s))
	((size (WB-Bag-Tree-Size contents))))
    (unless (and (>= rank 0) (< rank size))
      (error 'simple-type-error :datum rank :expected-type `(integer 0 (,size))
	     :format-control "Rank ~D out of bounds on ~A"
	     :format-arguments (list rank s)))
    (WB-Bag-Tree-Rank-Pair contents rank)))

(defmethod least ((b wb-bag))
  (let ((tree (wb-bag-contents b)))
    (if tree
	(let ((val mult (WB-Bag-Tree-Least-Pair tree)))
	  (values val mult t))
      (values nil nil nil))))

(defmethod greatest ((m wb-bag))
  (let ((tree (wb-bag-contents m)))
    (if tree
	(let ((val mult (WB-Bag-Tree-Greatest-Pair tree)))
	  (values val mult t))
      (values nil nil nil))))

(defmethod size ((b wb-bag))
  (WB-Bag-Tree-Total-Count (wb-bag-contents b)))

(defmethod set-size ((b wb-bag))
  (WB-Bag-Tree-Size (wb-bag-contents b)))

(defmethod multiplicity ((b wb-bag) x)
  (WB-Bag-Tree-Multiplicity (wb-bag-contents b) x))

(defmethod multiplicity ((s set) x)
  (if (contains? s x) 1 0))

(defmethod with ((b wb-bag) value &optional (multiplicity 1))
  (assert (and (integerp multiplicity) (not (minusp multiplicity))))
  (if (zerop multiplicity) b
    (make-wb-bag (WB-Bag-Tree-With (wb-bag-contents b) value multiplicity))))

(defmethod less ((b wb-bag) value &optional (multiplicity 1))
  (assert (and (integerp multiplicity) (not (minusp multiplicity))))
  (if (zerop multiplicity) b
    (make-wb-bag (WB-Bag-Tree-Less (wb-bag-contents b) value multiplicity))))

(defmethod union ((b1 wb-bag) (b2 wb-bag) &key)
  (make-wb-bag (WB-Bag-Tree-Union (wb-bag-contents b1) (wb-bag-contents b2))))

(defmethod union ((s wb-set) (b wb-bag) &key)
  (make-wb-bag (WB-Bag-Tree-Union (WB-Set-Tree-To-Bag-Tree (wb-set-contents s))
				  (wb-bag-contents b))))

(defmethod union ((b wb-bag) (s wb-set) &key)
  (make-wb-bag (WB-Bag-Tree-Union (wb-bag-contents b)
				  (WB-Set-Tree-To-Bag-Tree (wb-set-contents s)))))

(defmethod bag-sum ((b1 wb-bag) (b2 wb-bag))
  (make-wb-bag (WB-Bag-Tree-Sum (wb-bag-contents b1) (wb-bag-contents b2))))

(defmethod bag-sum ((s wb-set) (b wb-bag))
  (make-wb-bag (WB-Bag-Tree-Sum (WB-Set-Tree-To-Bag-Tree (wb-set-contents s))
				(wb-bag-contents b))))

(defmethod bag-sum ((b wb-bag) (s wb-set))
  (make-wb-bag (WB-Bag-Tree-Sum (wb-bag-contents b)
				(WB-Set-Tree-To-Bag-Tree (wb-set-contents s)))))

(defmethod intersection ((s1 wb-bag) (s2 wb-bag) &key)
  (make-wb-bag (WB-Bag-Tree-Intersect (wb-bag-contents s1) (wb-bag-contents s2))))

(defmethod intersection ((s wb-set) (b wb-bag) &key)
  (make-wb-set (WB-Set-Tree-Intersect (wb-set-contents s)
				      (WB-Bag-Tree-To-Set-Tree (wb-bag-contents b)))))

(defmethod intersection ((b wb-bag) (s wb-set) &key)
  (make-wb-set (WB-Set-Tree-Intersect (WB-Bag-Tree-To-Set-Tree (wb-bag-contents b))
				      (wb-set-contents s))))

(defmethod bag-product ((b1 wb-bag) (b2 wb-bag))
  (make-wb-bag (WB-Bag-Tree-Product (wb-bag-contents b1) (wb-bag-contents b2))))

(defmethod bag-product ((s wb-set) (b wb-bag))
  (make-wb-bag (WB-Bag-Tree-Product (WB-Set-Tree-To-Bag-Tree (wb-set-contents s))
				    (wb-bag-contents b))))

(defmethod bag-product ((b wb-bag) (s wb-set))
  (make-wb-bag (WB-Bag-Tree-Product (wb-bag-contents b)
				    (WB-Set-Tree-To-Bag-Tree (wb-set-contents s)))))

(defmethod bag-difference ((b1 wb-bag) (b2 wb-bag))
  (make-wb-bag (WB-Bag-Tree-Diff (wb-bag-contents b1) (wb-bag-contents b2))))

(defmethod bag-difference ((s wb-set) (b wb-bag))
  (make-wb-bag (WB-Bag-Tree-Diff (WB-Set-Tree-To-Bag-Tree (wb-set-contents s))
				 (wb-bag-contents b))))

(defmethod bag-difference ((b wb-bag) (s wb-set))
  (make-wb-bag (WB-Bag-Tree-Diff (wb-bag-contents b)
				 (WB-Set-Tree-To-Bag-Tree (wb-set-contents s)))))

(defmethod subbag? ((b1 wb-bag) (b2 wb-bag))
  (WB-Bag-Tree-Subbag? (wb-bag-contents b1) (wb-bag-contents b2)))

(defmethod subbag? ((s wb-set) (b wb-bag))
  (WB-Bag-Tree-Subbag? (WB-Set-Tree-To-Bag-Tree (wb-set-contents s)) (wb-bag-contents b)))

(defmethod subbag? ((b wb-bag) (s wb-set))
  (WB-Bag-Tree-Subbag? (wb-bag-contents b) (WB-Set-Tree-To-Bag-Tree (wb-set-contents s))))

(defmethod compare ((b1 wb-bag) (b2 wb-bag))
  (WB-Bag-Tree-Compare (wb-bag-contents b1) (wb-bag-contents b2)))

(defgeneric internal-do-bag-pairs (bag elt-fn value-fn)
  (:documentation
    "Calls `elt-fn' on successive pairs of the bag (the second argument is
the multiplicity); when done, calls `value-fn' on no arguments and returns the
result(s).  This is called by `do-bag' to provide for the possibility of
different bag implementations; it is not for public use.  `elt-fn' and
`value-fn' must be function objects, not symbols."))


(defmethod internal-do-bag-pairs ((b wb-bag) elt-fn value-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  ;; Expect Python note about "can't use known return convention"
  (Do-WB-Bag-Tree-Pairs (x n (wb-bag-contents b) (funcall value-fn))
    (funcall elt-fn x n)))

(defmethod iterator ((b wb-bag) &key pairs?)
  (if pairs?
      (Make-WB-Bag-Tree-Pair-Iterator (wb-bag-contents b))
    (Make-WB-Bag-Tree-Iterator (wb-bag-contents b))))

(defmethod filter ((pred function) (b bag))
  (bag-filter pred b))

(defmethod filter ((pred symbol) (b bag))
  (bag-filter (coerce pred 'function) b))

(defmethod filter ((pred map) (b bag))
  (bag-filter pred b))

(defun bag-filter (pred b)
  (let ((result nil))
    (do-bag-pairs (x n b)
      (when (@ pred x)
	(setq result (WB-Bag-Tree-With result x n))))
    (make-wb-bag result)))

(defmethod filter ((pred set) (b bag))
  (bag-product (convert 'bag pred) b))

(defmethod filter ((pred bag) (b bag))
  (bag-filter pred b))

(defun bag-filter-pairs (pred b)
  (let ((result nil))
    (do-bag-pairs (x n b)
      (when (funcall pred x n)
	(setq result (WB-Bag-Tree-With result x n))))
    (make-wb-bag result)))

(defmethod filter-pairs ((pred function) (b bag))
  (bag-filter-pairs pred b))

(defmethod filter-pairs ((pred symbol) (b bag))
  (bag-filter-pairs (coerce pred 'function) b))

(defmethod image ((fn function) (b bag))
  (bag-image fn b))

(defmethod image ((fn symbol) (b bag))
  (bag-image (coerce fn 'function) b))

(defmethod image ((fn map) (b bag))
  (bag-image fn b))

(defmethod image ((fn set) (b bag))
  (bag-image fn b))

(defmethod image ((fn bag) (b bag))
  (bag-image fn b))

(defun bag-image (fn b)
  (let ((result nil))
    (do-bag-pairs (x n b)
      (setq result (WB-Bag-Tree-With result (@ fn x) n)))
    (make-wb-bag result)))

(defmethod reduce ((fn function) (b bag) &key key (initial-value nil init?))
  (bag-reduce fn b initial-value (and key (coerce key 'function)) init?))

(defmethod reduce ((fn symbol) (b bag) &key key (initial-value nil init?))
  (bag-reduce (coerce fn 'function) b initial-value (and key (coerce key 'function))
	      init?))

(defun bag-reduce (fn b initial-value key init?)
  (declare (optimize (speed 3) (safety 0))
	   (type function fn)
	   (type (or function null) key))
  (let ((result initial-value)
	(call-fn? init?))
    (if (and (not init?) (empty? b))
	(setq result (funcall fn))
      (do-bag (x b)
	(if call-fn?
	    (setq result (funcall fn result (if key (funcall key x) x)))
	  (setq result (if key (funcall key x) x)
		call-fn? t))))
    result))

(defmethod convert ((to-type (eql 'bag)) (b bag) &key)
  b)

(defmethod convert ((to-type (eql 'wb-bag)) (b wb-bag) &key)
  b)

(defmethod convert ((to-type (eql 'set)) (b wb-bag) &key)
  (make-wb-set (WB-Bag-Tree-To-Set-Tree (wb-bag-contents b))))

(defmethod convert ((to-type (eql 'wb-set)) (b wb-bag) &key)
  (make-wb-set (WB-Bag-Tree-To-Set-Tree (wb-bag-contents b))))

(defmethod convert ((to-type (eql 'list)) (b bag) &key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((result nil))
    (do-bag (value b)
      (push value result))
    (nreverse result)))

(defmethod convert ((to-type (eql 'seq)) (b bag) &key)
  (convert 'seq (convert 'list b)))

(defmethod convert ((to-type (eql 'vector)) (b bag) &key)
  (coerce (convert 'list b) 'vector))

(defmethod convert ((to-type (eql 'alist)) (b bag) &key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((result nil))
    (do-bag-pairs (value count b)
      (push (cons value count) result))
    (nreverse result)))

(defmethod convert ((to-type (eql 'bag)) (l list) &key from-type)
  "If `from-type' is the symbol `alist', treats the operand as an alist where the
cdr of each pair (which must be a positive integer) is the member count for
the car.  Otherwise the operand is treated as a simple list of members, some
of which may be repeated."
  (bag-from-list l from-type))

(defmethod convert ((to-type (eql 'wb-bag)) (l list) &key from-type)
  "If `from-type' is the symbol `alist', treats the operand as an alist where the
cdr of each pair (which must be a positive integer) is the member count for
the car.  Otherwise the operand is treated as a simple list of members, some
of which may be repeated."
  (bag-from-list l from-type))

(defun bag-from-list (l from-type)
  (if (eq from-type 'alist)
      (let ((contents nil))
	(dolist (pr l)
	  (unless (and (integerp (cdr pr)) (< 0 (cdr pr)))
	    (error 'simple-type-error :datum (cdr pr) :expected-type '(integer 0 *)
		   :format-control "Cdr of pair is not a positive integer: ~S"
		   :format-arguments (list (cdr pr))))
	  (setq contents (WB-Bag-Tree-With contents (car pr) (cdr pr))))
	(make-wb-bag contents))
    ;; &&& Improve me someday
    (let ((contents nil))
      (dolist (x l)
	(setq contents (WB-Bag-Tree-With contents x)))
      (make-wb-bag contents))))

(defmethod convert ((to-type (eql 'bag)) (s sequence) &key)
  ;; &&& Improve me someday
  (let ((contents nil))
    (dotimes (i (length s))
      (setq contents (WB-Bag-Tree-With contents (elt s i))))
    (make-wb-bag contents)))

(defmethod convert ((to-type (eql 'wb-bag)) (s sequence) &key)
  ;; &&& Improve me someday
  (let ((contents nil))
    (dotimes (i (length s))
      (setq contents (WB-Bag-Tree-With contents (elt s i))))
    (make-wb-bag contents)))

(defmethod find (item (b bag) &key key test)
  (declare (optimize (speed 3) (safety 0)))
  (if key
      (let ((key (coerce key 'function)))
	(if test
	    (let ((test (coerce test 'function)))
	      (do-bag-pairs (x n b nil)
		(declare (ignore n))
		(when (funcall test item (funcall key x))
		  (return x))))
	  (do-bag-pairs (x n b nil)
	    (declare (ignore n))
	    (when (equal? item (funcall key x))
	      (return x)))))
    (if (and test (not (or (eq test 'equal?) (eq test #'equal?))))
	(let ((test (coerce test 'function)))
	  (do-bag-pairs (x n b nil)
	    (declare (ignore n))
	    (when (funcall test item x)
	      (return x))))
      (let ((val? val (lookup b item)))
	(declare (ignore val?))
	val))))

(defmethod find-if (pred (b bag) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function)))
    (if key
	(let ((key (coerce key 'function)))
	  (do-bag-pairs (x n b nil)
	    (declare (ignore n))
	    (when (funcall pred (funcall key x))
	      (return x))))
      (do-bag-pairs (x n b nil)
	(declare (ignore n))
	(when (funcall pred x)
	  (return x))))))

(defmethod find-if-not (pred (b bag) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function)))
    (find-if #'(lambda (x) (not (funcall pred x))) b :key key)))

(defmethod count (item (b bag) &key key test)
  (declare (optimize (speed 3) (safety 0)))
  (let ((total 0))
    (if key
	(let ((key (coerce key 'function)))
	  (if test
	      (let ((test (coerce test 'function)))
		(do-bag-pairs (x n b total)
		  (when (funcall test item (funcall key x))
		    (setq total (gen + total n)))))
	    (do-bag-pairs (x n b total)
	      (when (equal? item (funcall key x))
		(setq total (gen + total n))))))
      (if (and test (not (or (eq test 'equal?) (eq test #'equal?))))
	  (let ((test (coerce test 'function)))
	    (do-bag-pairs (x n b total)
	      (when (funcall test item x)
		(setq total (gen + total n)))))
	(multiplicity b item)))))

(defmethod count-if (pred (b bag) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function))
	(total 0))
    (if key
	(let ((key (coerce key 'function)))
	  (do-bag-pairs (x n b total)
	    (when (funcall pred (funcall key x))
	      (setq total (gen + total n)))))
      (do-bag-pairs (x n b total)
	(when (funcall pred x)
	  (setq total (gen + total n)))))))

(defmethod count-if-not (pred (s bag) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function)))
    (count-if #'(lambda (x) (not (funcall pred x))) s :key key)))

(defun print-wb-bag (bag stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "#{%" :suffix " %}")
    (let ((i 0))
      (do-bag-pairs (x n bag)
        (pprint-pop)
        (write-char #\Space stream)
        (pprint-newline :linear stream)
        (incf i)
        (if (> n 1)
	    (write (if *print-readably* `(% ,x ,n) `(,x ,n)) :stream stream)
            (write x :stream stream))))))

(def-gmap-arg-type :bag (bag)
  "Yields each element of `bag', as many times as its multiplicity."
  `((iterator ,bag)
    #'(lambda (it) (declare (type function it)) (funcall it ':done?))
    #'(lambda (it) (declare (type function it)) (funcall it ':get))))

(def-gmap-arg-type :bag-pairs (bag)
  "Yields each element of `bag' and its multiplicity as two values."
  `((iterator ,bag :pairs? t)
    #'(lambda (it) (declare (type function it)) (funcall it ':done?))
    (:values 2 #'(lambda (it) (declare (type function it)) (funcall it ':get)))))

(def-gmap-arg-type :wb-bag (bag)
  "Yields each element of `bag', as many times as its multiplicity."
  `((Make-WB-Bag-Tree-Iterator-Internal (wb-bag-contents ,bag))
    #'WB-Bag-Tree-Iterator-Done?
    #'WB-Bag-Tree-Iterator-Get))

(def-gmap-arg-type :wb-bag-pairs (bag)
  "Yields each element of `bag' and its multiplicity as two values."
  `((Make-WB-Bag-Tree-Pair-Iterator-Internal (wb-bag-contents ,bag))
    #'WB-Bag-Tree-Pair-Iterator-Done?
    (:values 2 #'WB-Bag-Tree-Pair-Iterator-Get)))

(def-gmap-res-type :bag (&key filterp)
  "Returns a bag of the values, optionally filtered by `filterp'."
  `(nil #'WB-Bag-Tree-With #'make-wb-bag ,filterp))

(def-gmap-res-type :bag-pairs (&key filterp)
  "Consumes two values from the mapped function; returns a bag of the pairs.
Note that `filterp', if supplied, must take two arguments."
  `(nil (:consume 2 #'WB-Bag-Tree-With) #'make-wb-bag ,filterp))

(def-gmap-res-type :wb-bag (&key filterp)
  "Returns a wb-bag of the values, optionally filtered by `filterp'."
  `(nil #'WB-Bag-Tree-With #'make-wb-bag ,filterp))

(def-gmap-res-type :wb-bag-pairs (&key filterp)
  "Consumes two values from the mapped function; returns a wb-bag of the pairs.
Note that `filterp', if supplied, must take two arguments."
  `(nil (:consume 2 #'WB-Bag-Tree-With) #'make-wb-bag ,filterp))


;;; ================================================================================
;;; Maps

(defstruct (wb-map
	    (:include map)
	    (:constructor make-wb-map (contents &optional default))
	    (:predicate wb-map?)
	    (:print-function print-wb-map)
	    (:copier nil))
  "A class of functional maps represented as weight-balanced binary trees.  This is
the default implementation of maps in FSet."
  contents)


(defparameter *empty-wb-map* (make-wb-map nil))

(declaim (inline empty-map))
(defun empty-map (&optional default)
  "Returns an empty map of the default implementation."
  (if default (make-wb-map nil default)
    *empty-wb-map*))

(declaim (inline empty-wb-map))
(defun empty-wb-map (&optional default)
  "Returns an empty wb-map."
  (if default (make-wb-map nil default)
    *empty-wb-map*))

(defmethod default ((m map))
  (map-default m))

(defmethod with-default ((m wb-map) new-default)
  (make-wb-map (wb-map-contents m) new-default))

(defmethod empty? ((m wb-map))
  (null (wb-map-contents m)))

(defmethod arb ((m wb-map))
  (let ((tree (wb-map-contents m)))
    (if tree
	(let ((key val (WB-Map-Tree-Arb-Pair tree)))
	  (values key val t))
      (values nil nil nil))))

(defmethod least ((m wb-map))
  (let ((tree (wb-map-contents m)))
    (if tree
	(let ((key val (WB-Map-Tree-Least-Pair tree)))
	  (values key val t))
      (values nil nil nil))))

(defmethod greatest ((m wb-map))
  (let ((tree (wb-map-contents m)))
    (if tree
	(let ((key val (WB-Map-Tree-Greatest-Pair tree)))
	  (values key val t))
      (values nil nil nil))))

(defmethod size ((m wb-map))
  (WB-Map-Tree-Size (wb-map-contents m)))

(defmethod contains? ((m wb-map) x &optional (y nil y?))
  (check-three-arguments y? 'contains? 'wb-map)
  (let ((val? val (WB-Map-Tree-Lookup (wb-map-contents m) x)))
    (and val? (equal? val y))))

(defmethod lookup ((m wb-map) key)
  (let ((val? val (WB-Map-Tree-Lookup (wb-map-contents m) key)))
    ;; Our internal convention is the reverse of the external one.
    (values (if val? val (map-default m)) val?)))

(defmethod rank ((s wb-map) x)
  (let ((found? rank (WB-Map-Tree-Rank (wb-map-contents s) x)))
    (values (if found? rank (1- rank)) found?)))

(defmethod at-rank ((s wb-map) rank)
  (let ((contents (wb-map-contents s))
	((size (WB-Map-Tree-Size contents))))
    (unless (and (>= rank 0) (< rank size))
      (error 'simple-type-error :datum rank :expected-type `(integer 0 (,size))
	     :format-control "Rank ~D out of bounds on ~A"
	     :format-arguments (list rank s)))
    (WB-Map-Tree-Rank-Pair contents rank)))

(defmethod with ((m wb-map) key &optional (value nil value?))
  (check-three-arguments value? 'with 'wb-map)
  (make-wb-map (WB-Map-Tree-With (wb-map-contents m) key value)
	       (map-default m)))

(defmethod less ((m wb-map) key &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'wb-map)
  (make-wb-map (WB-Map-Tree-Less (wb-map-contents m) key)
	       (map-default m)))

(defmethod domain ((m wb-map))
  (make-wb-set (WB-Map-Tree-Domain (wb-map-contents m))))

(defmethod compare ((map1 wb-map) (map2 wb-map))
  (WB-Map-Tree-Compare (wb-map-contents map1) (wb-map-contents map2)))

(defgeneric internal-do-map (map elt-fn value-fn)
  (:documentation
    "Calls `elt-fn' on successive pairs of the map (as two arguments); when done,
calls `value-fn' on no arguments and returns the result(s).  This is called by
`do-map' to provide for the possibility of different map implementations; it
is not for public use.  `elt-fn' and `value-fn' must be function objects, not
symbols."))

(defmethod internal-do-map ((m wb-map) elt-fn value-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  ;; Expect Python note about "can't use known return convention"
  (Do-WB-Map-Tree-Pairs (x y (wb-map-contents m) (funcall value-fn))
    (funcall elt-fn x y)))

(defmethod iterator ((m wb-map) &key)
  (Make-WB-Map-Tree-Iterator (wb-map-contents m)))

(defmethod filter ((pred function) (m map))
  (map-filter pred m))

(defmethod filter ((pred symbol) (m map))
  (map-filter (coerce pred 'function) m))

(defun map-filter (pred m)
  (let ((result nil))
    (do-map (x y m)
      (when (funcall pred x y)
	(setq result (WB-Map-Tree-With result x y))))
    (make-wb-map result (map-default m))))

(defmethod image ((fn function) (m map))
  (map-image fn m))

(defmethod image ((fn symbol) (m map))
  (map-image (coerce fn 'function) m))

(defun map-image (fn m)
  (let ((result nil))
    (do-map (x y m)
      (let ((new-x new-y (funcall fn x y)))
	(setq result (WB-Map-Tree-With result new-x new-y))))
    (make-wb-map result (map-default m))))

(defmethod reduce ((fn function) (m map) &key key (initial-value nil init?))
  (map-reduce fn m initial-value (and key (coerce key 'function)) init?))

(defmethod reduce ((fn symbol) (m map) &key key (initial-value nil init?))
  (map-reduce (coerce fn 'function) m initial-value (and key (coerce key 'function))
	      init?))

(defun map-reduce (fn m initial-value key init?)
  (declare (optimize (speed 3) (safety 0))
	   (type function fn)
	   (type (or function null) key))
  (unless init?
    (error 'simple-program-error
	   :format-control "~A on a map requires an initial value"
	   :format-arguments '(reduce)))
  (let ((result initial-value))
    (do-map (x y m)
      (let ((x y (if key (funcall key x y) (values x y))))
	(setq result (funcall fn result x y))))
    result))

(defmethod range ((m map))
  (let ((s nil))
    (do-map (key val m)
      (declare (ignore key))
      (setq s (WB-Set-Tree-With s val)))
    (make-wb-set s)))

(defmethod domain-contains? ((m wb-map) x)
  (WB-Map-Tree-Lookup (wb-map-contents m) x))

(defmethod range-contains? ((m wb-map) x)
  (do-map (k v m)
    (declare (ignore k))
    (when (equal? v x)
      (return t))))

(defmethod map-union ((map1 wb-map) (map2 wb-map)
		      &optional (val-fn #'(lambda (v1 v2)
					    (declare (ignore v1))
					    v2)))
  (make-wb-map (WB-Map-Tree-Union (wb-map-contents map1) (wb-map-contents map2)
				  (coerce val-fn 'function))
	       (funcall val-fn (map-default map1) (map-default map2))))

(defmethod map-intersection ((map1 wb-map) (map2 wb-map)
			     &optional (val-fn #'(lambda (v1 v2)
						   (declare (ignore v1))
						   v2)))
  (make-wb-map (WB-Map-Tree-Intersect (wb-map-contents map1) (wb-map-contents map2)
				      (coerce val-fn 'function))
	       (funcall val-fn (map-default map1) (map-default map2))))

(defmethod map-difference-2 ((map1 wb-map) (map2 wb-map))
  (let ((newc1 newc2 (WB-Map-Tree-Diff-2 (wb-map-contents map1) (wb-map-contents map2))))
    (values (make-wb-map newc1 (map-default map1))
	    (make-wb-map newc2 (map-default map2)))))

(defmethod restrict ((m wb-map) (s wb-set))
  (make-wb-map (WB-Map-Tree-Restrict (wb-map-contents m) (wb-set-contents s))
	       (map-default m)))

(defmethod restrict-not ((m wb-map) (s wb-set))
  (make-wb-map (WB-Map-Tree-Restrict-Not (wb-map-contents m) (wb-set-contents s))
	       (map-default m)))

(defmethod compose ((map1 map) (map2 wb-map))
  (let ((tree2 (wb-map-contents map2)))
    (make-wb-map (WB-Map-Tree-Compose (wb-map-contents map1)
				      #'(lambda (x)
					  (let ((val2? val2
						  (WB-Map-Tree-Lookup tree2 x)))
					    (if val2? val2 (map-default map2)))))
		 (let ((new-default? new-default
			 (WB-Map-Tree-Lookup tree2 (map-default map1))))
		   (if new-default? new-default (map-default map2))))))

(defmethod compose ((m wb-map) (fn function))
  (map-fn-compose m fn))

(defmethod compose ((m wb-map) (fn symbol))
  (map-fn-compose m (coerce fn 'function)))

(defmethod compose ((m wb-map) (s seq))
  (map-fn-compose m (fn (x) (@ s x))))

(defun map-fn-compose (m fn)
  (make-wb-map (WB-Map-Tree-Compose (wb-map-contents m) fn)
	       (funcall fn (map-default m))))

(defmethod convert ((to-type (eql 'map)) (m map) &key)
  m)

(defmethod convert ((to-type (eql 'wb-map)) (m wb-map) &key)
  m)

(defmethod convert ((to-type (eql 'list)) (m map) &key (pair-fn #'cons))
  (let ((result nil)
	(pair-fn (coerce pair-fn 'function)))
    (do-map (key val m)
      (push (funcall pair-fn key val) result))
    (nreverse result)))

(defmethod convert ((to-type (eql 'seq)) (m map) &key (pair-fn #'cons))
  (convert 'seq (convert 'list m :pair-fn pair-fn)))

(defmethod convert ((to-type (eql 'vector)) (m map) &key (pair-fn #'cons))
  (coerce (convert 'list m :pair-fn pair-fn) 'vector))

(defmethod convert ((to-type (eql 'set)) (m map) &key (pair-fn #'cons))
  (let ((result nil)
	(pair-fn (coerce pair-fn 'function)))
    (do-map (key val m)
      (setq result (WB-Set-Tree-With result (funcall pair-fn key val))))
    (make-wb-set result)))

;;; &&& Plist support?  The `key-fn' / `value-fn' thing is not very useful.
(defmethod convert ((to-type (eql 'map)) (list list)
		    &key (key-fn #'car) (value-fn #'cdr))
  (wb-map-from-list list key-fn value-fn))

(defmethod convert ((to-type (eql 'wb-map)) (list list)
		    &key (key-fn #'car) (value-fn #'cdr))
  (wb-map-from-list list key-fn value-fn))

(defun wb-map-from-list (list key-fn value-fn)
  (let ((m nil)
	(key-fn (coerce key-fn 'function))
	(value-fn (coerce value-fn 'function)))
    (dolist (pr list)
      (setq m (WB-Map-Tree-With m (funcall key-fn pr) (funcall value-fn pr))))
    (make-wb-map m)))

(defmethod convert ((to-type (eql 'map)) (s sequence)
		    &key (key-fn #'car) (value-fn #'cdr))
  (wb-map-from-cl-sequence s key-fn value-fn))

(defmethod convert ((to-type (eql 'wb-map)) (s sequence)
		    &key (key-fn #'car) (value-fn #'cdr))
  (wb-map-from-cl-sequence s key-fn value-fn))

(defun wb-map-from-cl-sequence (s key-fn value-fn)
  (let ((m nil))
    (dotimes (i (length s))
      (let ((pr (elt s i)))
	(setq m (WB-Map-Tree-With m (funcall key-fn pr) (funcall value-fn pr)))))
    (make-wb-map m)))

(defmethod convert ((to-type (eql 'map)) (b bag) &key)
  (convert 'wb-map b))

(defmethod convert ((to-type (eql 'wb-map)) (b bag) &key)
  ;; &&& If desired, we can easily make a very fast version of this -- all it has
  ;; to do is build new interior nodes, reusing the leaf vectors.
  (let ((m nil))
    (do-bag-pairs (x n b)
      (setq m (WB-Map-Tree-With m x n)))
    (make-wb-map m)))

(defmethod convert ((to-type (eql 'map)) (ht hash-table) &key)
  (convert 'wb-map ht))

(defmethod convert ((to-type (eql 'wb-map)) (ht hash-table) &key)
  (let ((m nil))
    (maphash (lambda (k v) (setq m (WB-Map-Tree-With m k v))) ht)
    (make-wb-map m)))

(defmethod convert ((to-type (eql 'hash-table)) (m map)
		    &rest make-hash-table-args &key &allow-other-keys)
  (let ((ht (apply #'make-hash-table make-hash-table-args)))
    (do-map (x y m)
      (setf (gethash x ht) y))
    ht))

(defmethod find (item (m map) &key key test)
  (declare (optimize (speed 3) (safety 0)))
  (if key
      (let ((key (coerce key 'function)))
	(if test
	    (let ((test (coerce test 'function)))
	      (do-map (x y m nil)
		(when (funcall test item (funcall key x))
		  (return (values x y)))))
	  (do-map (x y m nil)
	    (when (equal? item (funcall key x))
	      (return (values x y))))))
    (if test
	(let ((test (coerce test 'function)))
	  (do-map (x y m nil)
	    (when (funcall test item x)
	      (return (values x y)))))
      (let ((val val? (lookup m item)))
	(if val? (values item val)
	  (values nil nil))))))

(defmethod find-if (pred (m map) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function)))
    (if key
	(let ((key (coerce key 'function)))
	  (do-map (x y m nil)
	    (when (funcall pred (funcall key x))
	      (return (values x y)))))
      (do-map (x y m nil)
	(when (funcall pred x)
	  (return (values x y)))))))

(defmethod find-if-not (pred (m map) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function)))
    (find-if #'(lambda (x) (not (funcall pred x))) m :key key)))

(defmethod count (item (m map) &key key test)
  (declare (optimize (speed 3) (safety 0)))
  (let ((total 0))
    (declare (fixnum total))
    (if key
	(let ((key (coerce key 'function)))
	  (if test
	      (let ((test (coerce test 'function)))
		(do-map (x y m total)
		  (declare (ignore y))
		  (when (funcall test item (funcall key x))
		    (incf total))))
	    (progn
	      (do-map (x y m total)
		(declare (ignore y))
		(when (equal? item (funcall key x))
		  (incf total))))))
      (if (and test (not (or (eq test 'equal?) (eq test #'equal?))))
	  (let ((test (coerce test 'function)))
	    (do-map (x y m total)
	      (declare (ignore y))
	      (when (funcall test item x)
		(incf total))))
	(let ((val? val (lookup m item)))
	  (declare (ignore val))
	  (if val? 1 0))))))

(defmethod count-if (pred (m map) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function))
	(n 0))
    (declare (fixnum n))
    (if key
	(let ((key (coerce key 'function)))
	  (do-map (x y m n)
	    (declare (ignore y))
	    (when (funcall pred (funcall key x))
	      (incf n))))
      (do-map (x y m n)
	(declare (ignore y))
	(when (funcall pred x)
	  (incf n))))))

(defmethod count-if-not (pred (m map) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function)))
    (count-if #'(lambda (x) (not (funcall pred x))) m :key key)))

(defun print-wb-map (map stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "#{|")
    (do-map (x y map)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (write (list x y) :stream stream))
    (format stream " |}~:[~;/~:*~S~]" (map-default map))))

(def-gmap-arg-type :map (map)
  "Yields each pair of `map', as two values."
  `((iterator ,map)
    #'(lambda (it) (declare (type function it)) (funcall it ':done?))
    (:values 2 #'(lambda (it) (declare (type function it)) (funcall it ':get)))))

(def-gmap-arg-type :wb-map (map)
  "Yields each pair of `map', as two values."
  `((Make-WB-Map-Tree-Iterator-Internal (wb-map-contents ,map))
    #'WB-Map-Tree-Iterator-Done?
    (:values 2 #'WB-Map-Tree-Iterator-Get)))

(def-gmap-res-type :map (&key filterp default)
  "Consumes two values from the mapped function; returns a map of the pairs.
Note that `filterp', if supplied, must take two arguments."
  `(nil (:consume 2 #'WB-Map-Tree-With) #'(lambda (tree) (make-wb-map tree ,default))
    ,filterp))

(def-gmap-res-type :wb-map (&key filterp default)
  "Consumes two values from the mapped function; returns a wb-map of the pairs.
Note that `filterp', if supplied, must take two arguments."
  `(nil (:consume 2 #'WB-Map-Tree-With) #'(lambda (tree) (make-wb-map tree ,default))
    ,filterp))


;;; ================================================================================
;;; Seqs

(defstruct (wb-seq
	    (:include seq)
	    (:constructor make-wb-seq (contents &optional default))
	    (:predicate wb-seq?)
	    (:print-function print-wb-seq)
	    (:copier nil))
  "A class of functional seqs (sequences, but we use the short name to avoid
confusion with `cl:sequence') represented as weight-balanced binary trees.
This is the default implementation of seqs in FSet."
  contents)


(defparameter *empty-wb-seq* (make-wb-seq nil))

(declaim (inline empty-seq))
(defun empty-seq (&optional default)
  "Returns an empty seq of the default implementation."
  (if default (make-wb-seq nil default)
    *empty-wb-seq*))

(declaim (inline empty-wb-seq))
(defun empty-wb-seq ()
  "Returns an empty wb-seq."
  *empty-wb-seq*)

(defmethod empty? ((s wb-seq))
  (null (wb-seq-contents s)))

(defmethod default ((s seq))
  (seq-default s))

(defmethod with-default ((s wb-seq) new-default)
  (make-wb-seq (wb-seq-contents s) new-default))

(defmethod size ((s wb-seq))
  (WB-Seq-Tree-Size (wb-seq-contents s)))

(defmethod lookup ((s wb-seq) key)
  (if (typep key 'fixnum)
      (let ((val? val (WB-Seq-Tree-Subscript (wb-seq-contents s) key)))
	(values (if val? val (seq-default s)) val?))
    (values nil nil)))

(defmethod first ((s wb-seq))
  (let ((val? val (WB-Seq-Tree-Subscript (wb-seq-contents s) 0)))
    (values (if val? val (seq-default s)) val?)))

(defmethod last ((s wb-seq))
  (let ((tree (wb-seq-contents s))
	((val? val (WB-Seq-Tree-Subscript tree (1- (WB-Seq-Tree-Size tree))))))
    (values (if val? val (seq-default s)) val?)))

(defmethod with-first ((s wb-seq) val)
  (make-wb-seq (WB-Seq-Tree-Insert (wb-seq-contents s) 0 val)
	       (seq-default s)))

(defmethod with-last ((s wb-seq) val)
  (let ((tree (wb-seq-contents s)))
    (make-wb-seq (WB-Seq-Tree-Insert tree (WB-Seq-Tree-Size tree) val)
		 (seq-default s))))

(defmethod less-first ((s wb-seq))
  (let ((tree (wb-seq-contents s)))
    (make-wb-seq (WB-Seq-Tree-Subseq tree 1 (WB-Seq-Tree-Size tree))
		 (seq-default s))))

(defmethod less-last ((s wb-seq))
  (let ((tree (wb-seq-contents s)))
    (make-wb-seq (WB-Seq-Tree-Subseq tree 0 (1- (WB-Seq-Tree-Size tree)))
		 (seq-default s))))

(defmethod with ((s wb-seq) idx &optional (val nil val?))
  (check-three-arguments val? 'with 'wb-seq)
  (let ((tree (wb-seq-contents s))
	((size (WB-Seq-Tree-Size tree))))
    (when (< idx -1)
      (setq tree (WB-Seq-Tree-Concat
		   (WB-Seq-Tree-From-Vector
		     (make-array (- -1 idx) :initial-element (seq-default s)))
		   tree))
      (setq idx -1))
    (when (> idx size)
      (setq tree (WB-Seq-Tree-Concat
		   tree (WB-Seq-Tree-From-Vector
			  (make-array (- idx size) :initial-element (seq-default s)))))
      (setq size idx))
    (make-wb-seq (if (= idx -1)
		     (WB-Seq-Tree-Insert tree 0 val)
		   (if (= idx size)
		       (WB-Seq-Tree-Insert tree idx val)
		     (WB-Seq-Tree-With tree idx val)))
		 (seq-default s))))

(defmethod insert ((s wb-seq) idx val)
  (let ((tree (wb-seq-contents s))
	((size (WB-Seq-Tree-Size tree))))
    (when (< idx 0)
      (setq tree (WB-Seq-Tree-Concat
		   (WB-Seq-Tree-From-Vector
		     (make-array (- idx) :initial-element (seq-default s)))
		   tree))
      (setq idx 0))
    (when (> idx size)
      (setq tree (WB-Seq-Tree-Concat
		   tree (WB-Seq-Tree-From-Vector
			  (make-array (- idx size) :initial-element (seq-default s)))))
      (setq size idx))
    (make-wb-seq (WB-Seq-Tree-Insert tree idx val)
		 (seq-default s))))

(defmethod splice ((s wb-seq) idx subseq)
  (let ((tree (wb-seq-contents s))
	((size (WB-Seq-Tree-Size tree)))
	(subseq-tree (wb-seq-contents (convert 'wb-seq subseq))))
    (when (< idx 0)
      (setq tree (WB-Seq-Tree-Concat
		   (WB-Seq-Tree-From-Vector
		     (make-array (- idx) :initial-element (seq-default s)))
		   tree))
      (setq idx 0))
    (when (> idx size)
      (setq tree (WB-Seq-Tree-Concat
		   tree (WB-Seq-Tree-From-Vector
			  (make-array (- idx size) :initial-element (seq-default s)))))
      (setq size idx))
    (make-wb-seq (WB-Seq-Tree-Concat (WB-Seq-Tree-Concat (WB-Seq-Tree-Subseq tree 0 idx)
							 subseq-tree)
				     (WB-Seq-Tree-Subseq tree idx (WB-Seq-Tree-Size tree)))
		 (seq-default s))))

(defmethod less ((s wb-seq) idx &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'wb-seq)
  (let ((tree (wb-seq-contents s))
	((size (WB-Seq-Tree-Size tree))))
    (if (and (>= idx 0) (< idx size))
	(make-wb-seq (WB-Seq-Tree-Remove tree idx) (seq-default s))
      s)))

(defmethod concat ((s1 seq) &rest seqs)
  (let ((tree (wb-seq-contents s1)))
    (dolist (seq seqs)
      (setq tree (WB-Seq-Tree-Concat tree (wb-seq-contents (convert 'seq seq)))))
    (make-wb-seq tree (seq-default s1))))

(defmethod subseq ((s wb-seq) start &optional end)
  (let ((tree (wb-seq-contents s))
	((size (WB-Seq-Tree-Size tree))
	 ((start (max 0 start))
	  (end (if end (min end size) size)))))
    (if (and (= start 0) (= end size))
	s
      (make-wb-seq (WB-Seq-Tree-Subseq tree start end)
		   (seq-default s)))))

(defmethod reverse ((s wb-seq))
  (make-wb-seq (WB-Seq-Tree-Reverse (wb-seq-contents s))
	       (seq-default s)))

(defmethod sort ((s wb-seq) pred &key key)
  (with-default (convert 'seq (cl:sort (convert 'vector s) pred :key key))
		(seq-default s)))

(defmethod stable-sort ((s wb-seq) pred &key key)
  (with-default (convert 'seq (cl:stable-sort (convert 'vector s) pred :key key))
		(seq-default s)))

(defmethod domain ((s wb-seq))
  (let ((result nil))
    (dotimes (i (size s))
      (setq result (WB-Set-Tree-With result i)))
    (make-wb-set result)))

(defmethod range ((s wb-seq))
  (convert 'set s))

(defmethod convert ((to-type (eql 'seq)) (s seq) &key)
  s)

(defmethod convert ((to-type (eql 'wb-seq)) (s wb-seq) &key)
  s)

(defmethod convert ((to-type (eql 'seq)) (vec vector) &key)
  (make-wb-seq (WB-Seq-Tree-From-Vector vec)))

(defmethod convert ((to-type (eql 'wb-seq)) (vec vector) &key)
  (make-wb-seq (WB-Seq-Tree-From-Vector vec)))

(defmethod convert ((to-type (eql 'vector)) (s wb-seq) &key)
  (WB-Seq-Tree-To-Vector (wb-seq-contents s)))

;;; Always returns a string.  Signals `type-error' if it encounters a non-character.
(defmethod convert ((to-type (eql 'string)) (s wb-seq) &key)
  (WB-Seq-Tree-To-String (wb-seq-contents s)))

(defmethod convert ((to-type (eql 'seq)) (l list) &key)
  (make-wb-seq (WB-Seq-Tree-From-List l)))

(defmethod convert ((to-type (eql 'wb-seq)) (l list) &key)
  (make-wb-seq (WB-Seq-Tree-From-List l)))

(defmethod convert ((to-type (eql 'list)) (s wb-seq) &key)
  (WB-Seq-Tree-To-List (wb-seq-contents s)))

(defmethod convert ((to-type (eql 'seq)) (s set) &key)
  ;; Not sure we can improve on this much.
  (convert 'seq (convert 'list s)))

(defmethod convert ((to-type (eql 'wb-seq)) (s set) &key)
  ;; Not sure we can improve on this much.
  (convert 'wb-seq (convert 'list s)))

(defmethod convert ((to-type (eql 'set)) (s wb-seq) &key)
  (make-wb-set (WB-Seq-Tree-To-Set-Tree (wb-seq-contents s))))

(defmethod convert ((to-type (eql 'wb-set)) (s wb-seq) &key)
  (make-wb-set (WB-Seq-Tree-To-Set-Tree (wb-seq-contents s))))

(defmethod convert ((to-type (eql 'wb-seq)) (b bag) &key)
  (convert 'wb-seq (convert 'list b)))

(defmethod convert ((to-type (eql 'wb-seq)) (m map) &key (pair-fn #'cons))
  (convert 'wb-seq (convert 'list m :pair-fn pair-fn)))

(defmethod compare ((s1 wb-seq) (s2 wb-seq))
  (WB-Seq-Tree-Compare (wb-seq-contents s1) (wb-seq-contents s2)))

(defmethod compare-lexicographically ((s1 wb-seq) (s2 wb-seq))
  (WB-Seq-Tree-Compare-Lexicographically (wb-seq-contents s1) (wb-seq-contents s2)))

(defgeneric internal-do-seq (seq elt-fn value-fn index?
				 &key start end from-end?)
  (:documentation
    "Calls `elt-fn' on successive elements of `seq', possibly restricted by
`start' and `end', and in reverse order if `from-end?' is true.  When done,
calls `value-fn' on no arguments and returns the result(s).  This is called
by `do-seq' to provide for the possibility of different seq implementations;
it is not for public use.  `elt-fn' and `value-fn' must be function objects,
not symbols."))


(defmethod internal-do-seq ((s wb-seq) elt-fn value-fn index?
			    &key (start 0)
			         (end (WB-Seq-Tree-Size (wb-seq-contents s)))
			         from-end?)
  (declare (optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  (check-type start fixnum)
  (check-type end fixnum)
  ;; Expect Python notes about "can't use known return convention"
  (if index?
      (let ((i start))
	(declare (type fixnum i))
	(Do-WB-Seq-Tree-Members-Gen (x (wb-seq-contents s) start end from-end?
				       (funcall value-fn))
	  (funcall elt-fn x i)
	  (incf i)))
    (Do-WB-Seq-Tree-Members-Gen (x (wb-seq-contents s) start end from-end?
				     (funcall value-fn))
	(funcall elt-fn x))))

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

(defmethod iterator ((s wb-seq) &key)
  (Make-WB-Seq-Tree-Iterator (wb-seq-contents s)))

(defmethod domain-contains? ((s seq) x)
  (and (integerp x) (>= x 0) (< x (size s))))

(defmethod range-contains? ((s seq) x)
  (declare (optimize (speed 3) (safety 0)))
  (do-seq (y s)
    (when (equal? y x)
      (return t))))

(defmethod filter ((fn function) (s seq))
  (seq-filter fn s))

(defmethod filter ((fn symbol) (s seq))
  (seq-filter (coerce fn 'function) s))

(defmethod filter ((fn map) (s seq))
  (seq-filter #'(lambda (x) (lookup fn x)) s))

(defmethod filter ((fn set) (s seq))
  (seq-filter #'(lambda (x) (lookup fn x)) s))

(defmethod filter ((fn bag) (s seq))
  (seq-filter #'(lambda (x) (lookup fn x)) s))

(defun seq-filter (fn s)
  (declare (optimize (speed 3) (safety 0))
	   (type function fn))
  (let ((result nil))
    (do-seq (x s)
      (when (funcall fn x)
	(push x result)))
    (make-wb-seq (WB-Seq-Tree-From-List (nreverse result))
		 (seq-default s))))

(defmethod partition ((fn function) (s seq))
  (seq-partition fn s))

(defmethod partition ((fn symbol) (s seq))
  (seq-partition (coerce fn 'function) s))

(defmethod partition ((fn map) (s seq))
  (seq-partition #'(lambda (x) (lookup fn x)) s))

(defmethod partition ((fn set) (s seq))
  (seq-partition #'(lambda (x) (lookup fn x)) s))

(defmethod partition ((fn bag) (s seq))
  (seq-partition #'(lambda (x) (lookup fn x)) s))

(defun seq-partition (fn s)
  (declare (optimize (speed 3) (safety 0))
	   (type function fn))
  (let ((result-1 nil)
	(result-2 nil))
    (do-seq (x s)
      (if (funcall fn x)
	  (push x result-1)
	(push x result-2)))
    (values (make-wb-seq (WB-Seq-Tree-From-List (nreverse result-1))
			 (seq-default s))
	    (make-wb-seq (WB-Seq-Tree-From-List (nreverse result-2))
			 (seq-default s)))))

(defmethod image ((fn function) (s seq))
  (seq-image fn s))

(defmethod image ((fn symbol) (s seq))
  (seq-image (coerce fn 'function) s))

(defmethod image ((fn map) (s seq))
  (seq-image #'(lambda (x) (lookup fn x)) s))

(defmethod image ((fn set) (s seq))
  (seq-image #'(lambda (x) (lookup fn x)) s))

(defmethod image ((fn bag) (s seq))
  (seq-image #'(lambda (x) (lookup fn x)) s))

(defun seq-image (fn s)
  (declare (optimize (speed 3) (safety 0))
	   (type function fn))
  ;; This is not bad, but we could do better by walking the tree of `s' and building
  ;; the result in the same shape.
  (let ((result nil))
    (do-seq (x s)
      (push (funcall fn x) result))
    (make-wb-seq (WB-Seq-Tree-From-List (nreverse result))
		 (seq-default s))))

(defmethod reduce ((fn function) (s seq)
		   &key key (initial-value nil init?)
		   (start 0) (end (size s)) (from-end nil))
  (seq-reduce fn s initial-value (and key (coerce key 'function)) init?
	      start end from-end))

(defmethod reduce ((fn symbol) (s seq)
		   &key key (initial-value nil init?)
		   (start 0) (end (size s)) (from-end nil))
  (seq-reduce (coerce fn 'function) s initial-value (and key (coerce key 'function))
	      init? start end from-end))

(defun seq-reduce (fn s initial-value key init? start end from-end?)
  (declare (optimize (speed 3) (safety 0))
	   (type function fn)
	   (type (or function null) key)
	   (type fixnum start end))
  (let ((result initial-value)
	(call-fn? init?))
    (if (and (not init?) (empty? s))
	(setq result (funcall fn))
      (if (and (= start 0) (= end (the fixnum (size s))) (not from-end?))
	  (do-seq (x s)
	    (if call-fn?
		(setq result (funcall fn result (if key (funcall key x) x)))
	      (setq result (if key (funcall key x) x)
		    call-fn? t)))
	;; &&& Would be nice if our iterators were up to this.
	(dotimes (i (- end start))
	  (declare (type fixnum i))
	  (let ((x (lookup s (if from-end? (the fixnum (- end i 1))
			       (the fixnum (+ i start))))))
	    (if call-fn?
		(setq result (funcall fn result (if key (funcall key x) x)))
	      (setq result (if key (funcall key x) x)
		    call-fn? t))))))
    result))

(defmethod find (item (s seq) &key key test start end from-end)
  (declare (optimize (speed 3) (safety 0)))
  (let ((start (or start 0))
	(end (or end (size s))))
    (if key
	(let ((key (coerce key 'function)))
	  (if test
	      (let ((test (coerce test 'function)))
		(do-seq (x s :start start :end end :from-end? from-end :value nil)
		  (when (funcall test item (funcall key x))
		    (return x))))
	    (do-seq (x s :start start :end end :from-end? from-end :value nil)
	      (when (equal? item (funcall key x))
		(return x)))))
      (if test
	  (let ((test (coerce test 'function)))
	    (do-seq (x s :start start :end end :from-end? from-end :value nil)
	      (when (funcall test item x)
		    (return x))))
	(do-seq (x s :start start :end end :from-end? from-end :value nil)
	  (when (equal? item x)
	    (return x)))))))

(defmethod find-if (pred (s seq) &key key start end from-end)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function))
	(start (or start 0))
	(end (or end (size s))))
    (if key
	(let ((key (coerce key 'function)))
	  (do-seq (x s :start start :end end :from-end? from-end :value nil)
	    (when (funcall pred (funcall key x))
	      (return x))))
      (do-seq (x s :start start :end end :from-end? from-end :value nil)
	(when (funcall pred x)
	  (return x))))))

(defmethod find-if-not (pred (s seq) &key key start end from-end)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function)))
    (find-if #'(lambda (x) (not (funcall pred x))) s
	     :key key :start start :end end :from-end from-end)))

(defmethod count (item (s seq) &key key test start end from-end)
  (declare (optimize (speed 3) (safety 0)))
  (let ((total 0)
	(start (or start 0))
	(end (or end (size s))))
    (declare (fixnum total))
    (if key
	(let ((key (coerce key 'function)))
	  (if test
	      (let ((test (coerce test 'function)))
		(do-seq (x s :start start :end end :from-end? from-end)
		  (when (funcall test item (funcall key x))
		    (incf total))))
	    (do-seq (x s :start start :end end :from-end? from-end)
	      (when (equal? item (funcall key x))
		(incf total)))))
      (if (and test (not (or (eq test 'equal?) (eq test #'equal?))))
	  (let ((test (coerce test 'function)))
	    (do-seq (x s :start start :end end :from-end? from-end)
	      (when (funcall test item x)
		(incf total))))
	(do-seq (x s :start start :end end :from-end? from-end)
	  (when (equal? item x)
	    (incf total)))))
    total))

(defmethod count-if (pred (s seq) &key key start end from-end)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function))
	(n 0)
	(start (or start 0))
	(end (or end (size s))))
    (declare (fixnum n))
    (if key
	(let ((key (coerce key 'function)))
	  (do-seq (x s :start start :end end :from-end? from-end)
	    (when (funcall pred (funcall key x))
	      (incf n))))
      (do-seq (x s :start start :end end :from-end? from-end)
	(when (funcall pred x)
	  (incf n))))
    n))

(defmethod count-if-not (pred (s seq) &key key start end from-end)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function)))
    (count-if #'(lambda (x) (not (funcall pred x))) s
	      :key key :start start :end end :from-end from-end)))

(defmethod position (item (s seq) &key key test start end from-end)
  (declare (optimize (speed 3) (safety 0)))
  (let ((start (or start 0))
	((pos start))
	(end (or end (size s))))
    (declare (fixnum pos))
    (macrolet ((done () '(return (if from-end (- end pos 1) pos))))
      (if key
	  (let ((key (coerce key 'function)))
	    (if test
		(let ((test (coerce test 'function)))
		  (do-seq (x s :start start :end end :from-end? from-end)
		    (when (funcall test item (funcall key x))
		      (done))
		    (incf pos)))
		(do-seq (x s :start start :end end :from-end? from-end)
		  (when (equal? item (funcall key x))
		    (done))
		  (incf pos))))
	  (if (and test (not (or (eq test 'equal?) (eq test #'equal?))))
	      (let ((test (coerce test 'function)))
		(do-seq (x s :start start :end end :from-end? from-end)
		  (when (funcall test item x)
		    (done))
		  (incf pos)))
	      (do-seq (x s :start start :end end :from-end? from-end)
		(when (equal? item x)
		  (done))
		(incf pos)))))))

(defmethod position-if (pred (s seq) &key key start end from-end)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function))
	(start (or start 0))
	((pos start))
	(end (or end (size s))))
    (declare (fixnum pos))
    (macrolet ((done () '(return (if from-end (- end pos 1) pos))))
      (if key
	  (let ((key (coerce key 'function)))
	    (do-seq (x s :start start :end end :from-end? from-end)
	      (when (funcall pred (funcall key x))
		(done))
	      (incf pos)))
	  (do-seq (x s :start start :end end :from-end? from-end)
	    (when (funcall pred x)
	      (done))
	    (incf pos))))))

(defmethod position-if-not (pred (s seq) &key key start end from-end)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function)))
    (position-if #'(lambda (x) (not (funcall pred x))) s
		 :key key :start start :end end :from-end from-end)))

(defmethod remove (item (s seq) &key key test start end from-end count)
  (declare (optimize (speed 3) (safety 0)))
  (let ((start (or start 0))
	(end (or end (size s)))
	(count (or count (size s)))
	((head (subseq s 0 start))
	 (tail (subseq s end)))
	(mid nil)
	(test (if test (coerce test 'function) #'equal?))
	(key (and key (coerce key 'function))))
    (declare (fixnum count))
    (do-seq (x s :start start :end end :from-end? from-end)
      (if (and (> count 0)
	       (funcall test item (if key (funcall key x) x)))
	  (decf count)
	(push x mid)))
    (concat head (concat (convert 'seq (if from-end mid (nreverse mid)))
			 tail))))

(defmethod remove-if (pred (s seq) &key key start end from-end count)
  (declare (optimize (speed 3) (safety 0)))
  (let ((start (or start 0))
	(end (or end (size s)))
	(count (or count (size s)))
	((head (subseq s 0 start))
	 (tail (subseq s end)))
	(mid nil)
	(pred (coerce pred 'function))
	(key (and key (coerce key 'function))))
    (declare (fixnum count))
    (do-seq (x s :start start :end end :from-end? from-end)
      (if (and (> count 0)
	       (funcall pred (if key (funcall key x) x)))
	  (decf count)
	(push x mid)))
    (concat head (concat (convert 'seq (if from-end mid (nreverse mid)))
			 tail))))

(defmethod remove-if-not (pred (s seq) &key key start end from-end count)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function)))
    (remove-if #'(lambda (x) (not (funcall pred x))) s
	       :key key :start start :end end :from-end from-end :count count)))

(defmethod substitute (newitem olditem (s seq) &key key test start end from-end count)
  (declare (optimize (speed 3) (safety 0)))
  (let ((start (or start 0))
	(end (or end (size s)))
	(count (or count (size s)))
	((head (subseq s 0 start))
	 (tail (subseq s end)))
	(mid nil)
	(test (if test (coerce test 'function) #'equal?))
	(key (and key (coerce key 'function))))
    (declare (fixnum count))
    (do-seq (x s :start start :end end :from-end? from-end)
      (if (and (> count 0)
	       (funcall test olditem (if key (funcall key x) x)))
	  (progn (push newitem mid) (decf count))
	(push x mid)))
    (concat head (concat (convert 'seq (if from-end mid (nreverse mid)))
			 tail))))

(defmethod substitute-if (newitem pred (s seq) &key key start end from-end count)
  (declare (optimize (speed 3) (safety 0)))
  (let ((start (or start 0))
	(end (or end (size s)))
	(count (or count (size s)))
	((head (subseq s 0 start))
	 (tail (subseq s end)))
	(mid nil)
	(pred (coerce pred 'function))
	(key (and key (coerce key 'function))))
    (declare (fixnum count))
    (do-seq (x s :start start :end end :from-end? from-end)
      (if (and (> count 0)
	       (funcall pred (if key (funcall key x) x)))
	  (progn (push newitem mid) (decf count))
	(push x mid)))
    (concat head (concat (convert 'seq (if from-end mid (nreverse mid)))
			 tail))))

(defmethod substitute-if-not (newitem pred (s seq) &key key start end from-end count)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce pred 'function)))
    (substitute-if newitem #'(lambda (x) (not (funcall pred x))) s
		   :key key :start start :end end :from-end from-end :count count)))

(defun print-wb-seq (seq stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "#[")
    (do-seq (x seq)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (write x :stream stream))
    (format stream " ]~:[~;/~:*~S~]" (seq-default seq))))

(def-gmap-arg-type :seq (seq)
  "Yields the elements of `seq'."
  `((iterator ,seq)
    #'(lambda (it) (declare (type function it)) (funcall it ':done?))
    #'(lambda (it) (declare (type function it)) (funcall it ':get))))

(def-gmap-arg-type :wb-seq (seq)
  "Yields the elements of `seq'."
  `((Make-WB-Seq-Tree-Iterator-Internal (wb-seq-contents ,seq))
    #'WB-Seq-Tree-Iterator-Done?
    #'WB-Seq-Tree-Iterator-Get))

(def-gmap-res-type :seq (&key filterp)
  "Returns a seq of the values, optionally filtered by `filterp'."
  `(nil
    #'(lambda (a b) (cons b a))
    #'(lambda (s) (convert 'seq (nreverse s)))
    ,filterp))

(def-gmap-res-type :wb-seq (&key filterp)
  "Returns a seq of the values, optionally filtered by `filterp'."
  `(nil
    #'(lambda (a b) (cons b a))
    #'(lambda (s) (convert 'seq (nreverse s)))
    ,filterp))

(def-gmap-res-type :concat (&key filterp)
  "Returns the concatenation of the seq values, optionally filtered by `filterp'."
  `((seq) #'concat nil ,filterp))


;;; ================================================================================
;;; CL Sequences

;;; Convenience methods for some of the FSet generic functions.

(defmethod empty? ((l list))
  (null l))

(defmethod empty? ((s sequence))
  (zerop (length s)))

(defmethod size ((s sequence))
  (length s))

(defmethod lookup ((s sequence) (idx integer))
  (elt s idx))


;;; ================================================================================
;;; Miscellany

;;; Oooops -- I somehow thought CL already had this.
(define-condition simple-program-error (simple-condition program-error)
  ())

