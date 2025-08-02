;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: fset.lisp
;;; Contents: Top level of FSet, the fast functional set-theoretic datatypes package.
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2025 Scott L. Burson.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; This license provides NO WARRANTY.

(in-package :fset)


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

;; (declaim (inline lastcons head tail))

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
two argument bags.

If the collections are of different implementations or use different custom
compare or hash functions, the returned collection will be like the first
argument."))

(defgeneric bag-sum (bag1 bag2)
  (:documentation
    "Returns a bag whose multiplicity, for any value, is the sum of its
multiplicities in the two argument bags.

If the collections are of different implementations or use different custom
compare or hash functions, the returned collection will be like the first
argument."))

(defgeneric intersection (set-or-bag1 set-or-bag2 &key)
  (:documentation
    "Returns the intersection of the two sets/bags.  The result is a bag
if both arguments are bags; otherwise a set.  The intersection of two bags
is the bag whose multiplicity, for any value, is the minimum of its
multiplicities in the two argument bags.

If the collections are of different implementations or use different custom
compare or hash functions, the returned collection will be like the first
argument."))

(defgeneric bag-product (bag1 bag2)
  (:documentation
    "Returns a bag whose multiplicity, for any value, is the product of
its multiplicities in the two argument bags.

If the collections are of different implementations or use different custom
compare or hash functions, the returned collection will be like the first
argument."))

(defgeneric set-difference (set1 set2 &key)
  (:documentation
    "Returns the set difference of set1 and set2, i.e., the set containing
every member of `set1' that is not in `set2'.

If the collections are of different implementations or use different custom
compare or hash functions, the returned collection will be like the first
argument."))

(defgeneric set-difference-2 (set1 set2)
  (:documentation
    "Returns `set1 - set2' and `set2 - set1' as two values.

If the collections are of different implementations or use different custom
compare or hash functions, the first value will be like the first argument,
and the second value like the second."))

(defgeneric bag-difference (bag1 bag2)
  (:documentation
    "Returns a bag whose multiplicity, for any value, is its multiplicity
in `bag1' less that in `bag2', but of course not less than zero.

If the collections are of different implementations or use different custom
compare or hash functions, the returned collection will be like the first
argument."))

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

(defmethod filter (fn (s sequence))
  (cl:remove-if-not fn s))

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

;;; Prior to 1.4.0, this unconditionally called `val-fn' on the defaults of the
;;; two maps to produce the defaults of the result.  This was a PITA in the
;;; common case in which the defaults were null.  Now, it calls `val-fn' only
;;; if at least one of the defaults is nonnull.
(defgeneric map-union (map1 map2 &optional val-fn)
  (:documentation
    "Returns a map containing all the keys of `map1' and `map2', where the
value for each key contained in only one map is the value from that map, and
the value for each key contained in both maps is the result of calling
`val-fn' on the value from `map1' and the value from `map2'.  `val-fn'
defaults to simply returning its second argument, so the entries in `map2'
simply shadow those in `map1'.  The default for the new map is the result of
calling `val-fn' on the defaults for the two maps, if either of those is
nonnull.  `map-union' assumes that `val-fn' is idempotent, i.e., if the two
values passed to `val-fn' are equal, `val-fn' must return the same value; it
may elide calls to `val-fn' on that basis.

New feature: if `val-fn' returns `:no-value' as a second value, the result
will contain no pair with the corresponding key."))

;;; Prior to 1.4.0, this unconditionally called `val-fn' on the defaults of the
;;; two maps to produce the defaults of the result.  This was a PITA in the
;;; common case in which the defaults were null.  Now, it calls `val-fn' only
;;; if at least one of the defaults is nonnull.
(defgeneric map-intersection (map1 map2 &optional val-fn)
  (:documentation
    "Returns a map containing all the keys that are in the domains of both
`map1' and `map2', where the value for each key is the result of calling
`val-fn' on the value from `map1' and the value from `map2'.  `val-fn'
defaults to simply returning its second argument, so the entries in `map2'
simply shadow those in `map1'.  The default for the new map is the result
of calling `val-fn' on the defaults for the two maps, if either of those
is nonnull.  `map-intersection' assumes that `val-fn' is idempotent, i.e.,
if the two values passed to `val-fn' are equal, `val-fn' must return the
same value; it may elide calls to `val-fn' on that basis.

New feature: if `val-fn' returns `:no-value' as a second value, the result
will contain no pair with the corresponding key."))

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
  (:documentation "Converts the collection to the specified type.  Some methods
have additional keyword parameters to further specify the kind of conversion.
\(`sequence' here refers to a CL sequence -- either a list or a vector, unless
the implementation has more sequence subtypes.  FSet's `seq' is not a subtype of
`sequence'.)

When `to-type' is one of the abstract types -- `set', `bag', `map' etc. -- the
only guarantee is that `convert' will return an instance of the requested type.
The choices of implementation and organization are up to FSet, and there's no
option to request a specific organization.  In contrast, when `to-type' is one
of the concrete types -- `wb-set', `ch-set', `wb-map', etc. -- the `convert'
method accepts keyword arguments specifying the organization (e.g.
`compare-fn-name'), and guarantees to return a collection of that type and
organization; including the case where no organization is specified, in which
`convert' is guaranteed to return a collection of the default organization \(the
one using `fset:compare'\).

Method summary:     to-type       collection type         notes
                     set                set               0
                     wb-set             set               0, 15
                     wb-set             wb-set            15
                     set                list              1, 2
                     wb-set             list              2, 15
                     list               set
                     set                seq               1, 2
                     wb-set             seq               2, 15
                     set                sequence          1, 2
                     wb-set             sequence          2, 15
                     vector             set
                     ch-set             set               15
                     ch-set             ch-set            0, 15
                     ch-set             list              15
                     ch-set             seq               15
                     ch-set             sequence          15

                     bag                bag               0
                     wb-bag             wb-bag            0, 15
                     bag                wb-set            1
                     wb-bag             wb-set            15
                     bag                set
                     wb-bag             set               15
                     set                wb-bag            1
                     wb-set             wb-bag            15
                     list               bag               3
                     seq                bag               3
                     vector             bag               3
                     alist              bag
                     bag                list              1, 2, 3, 4
                     wb-bag             list              2, 3, 4, 15
                     bag                seq               1, 2, 3
                     wb-bag             seq               2, 3, 15
                     bag                sequence          1, 2, 3
                     wb-bag             sequence          2, 3, 15

                     map                map               0
                     wb-map             wb-map            0, 16
                     list               map               5
                     alist              map
                     seq                map               5
                     vector             map               5
                     set                map               5
                     map                list              1, 2, 6, 7
                     wb-map             list              2, 6, 7, 16
                     map                seq               1, 2, 6, 7, 17
                     wb-map             seq               2, 6, 7, 16, 17
                     map                sequence          1, 2, 6, 7
                     wb-map             sequence          2, 6, 7, 16
                     map                bag               1
                     wb-map             bag               16
                     map                hash-table        1
                     wb-map             hash-table        16
                     hash-table         map               8
                     wb-map             ch-map            16

                     seq                seq               0
                     wb-seq             wb-seq            0
                     seq                list              1, 9
                     wb-seq             list              9
                     list               wb-seq
                     seq                vector            1, 9
                     wb-seq             vector            9
                     seq                set               1
                     wb-seq             set
                     string             wb-seq            14
                     seq                bag               1, 5
                     wb-seq             bag               5
                     seq                map               1, 5, 17
                     wb-seq             map               5, 17

                     2-relation         2-relation        0
                     wb-2-relation      wb-2-relation     0
                     set                2-relation        1, 5
                     wb-set             2-relation        5
                     2-relation         map               1, 10
                     wb-2-relation      map               10
                     2-relation         list              1, 6
                     wb-2-relation      list              6
                     2-relation         seq               1, 6
                     wb-2-relation      seq               6
                     map                wb-2-relation     1, 11
                     wb-map             wb-2-relation     1, 11
                     map-to-sets        wb-2-relation     12

                     tuple              tuple             0
                     dyn-tuple          dyn-tuple         0
                     map                tuple             1
                     wb-map             tuple
                     list               tuple             5
                     tuple              list              6, 7, 13
                     dyn-tuple          list              7, 13

                     replay-set         replay-set        0
                     wb-replay-set      wb-replay-set     0
                     list               wb-replay-set
                     vector             wb-replay-set
                     seq                wb-replay-set     1
                     wb-seq             wb-replay-set
                     set                wb-replay-set     1
                     wb-set             wb-replay-set
                     replay-set         wb-set            1
                     wb-replay-set      wb-set
                     replay-set         list              1
                     wb-replay-set      list
                     replay-set         seq               1
                     wb-replay-set      seq
                     replay-set         sequence          1
                     wb-replay-set      sequence

                     replay-map         replay-map        0
                     wb-replay-map      wb-replay-map     0
                     map                wb-replay-map     1
                     wb-map             wb-replay-map
                     list               wb-replay-map     5
                     vector             wb-replay-map     5
                     seq                wb-replay-map     5
                     replay-map         list              1, 6
                     wb-replay-map      list              6
                     replay-map         seq               1, 6
                     wb-replay-map      seq               6
                     replay-map         sequence          1, 6
                     wb-replay-map      sequence          6

                     list               list              0
                     vector             vector            0
                     vector             list
                     list               sequence

Notes:
0. Identity conversion, provided for convenience.  (If the method also accepts
   an organization, then it performs the identity conversion only if the
   organization of the argument matches that requested.)
1. Constructs the WB (weight-balanced tree) implementation of its type (`wb-set'
   etc.).
2. Has keyword parameter `input-sorted?'.  If true, uses an algorithm optimized
   for this case: if the input is sorted (by `less-than?'), it runs in O(n) time
   and is much faster, but is up to 2.4x _slower_ than the default algorithm if
   the input is unsorted.
3. Has keyword parameter `pairs?'.  If true, treats the input elements as pairs
   (as conses) of a value and a count (multiplicity).
4. Has keyword parameter `from-type'.  If it's the symbol `alist', treats the
   input elements as pairs (as conses) of a value and a count.  Deprecated; use
   `pairs?'.
5. Has keyword parameter `pair-fn', defaulting to `#'cons'.  The function used
   to combine each key/value pair.
6. Has keyword parameters `key-fn' and `value-fn', used to extract a key/value
   pair from each input element.  Defaults are `#'car' and `#'cdr'.
7. If a key occurs more than once in the input, the result will have the value
   associated with the last occurrence.
8. All keyword arguments are forwarded to `make-hash-table'.
9. Optimized O(n) algorithm.
10. Has keyword parameter `from-type'.  If it's the symbol `map-to-sets', the
    range elements must all be sets, and the result pairs each domain element
    with each member of the corresponding range set.  Otherwise, the result pairs
    each domain element with the corresponding range element directly.
11. The relation must be a function (must have only one range value per domain
    value).
12. Returns a map mapping each domain value to the set of corresponding range
    values.
13. Constructs a `dyn-tuple'.
14. Always returns a string.  Signals `type-error' if it encounters a non-
    character.
15. Has keyword parameter `compare-fn-name'.  If nonnull, must be a symbol, the
    name of the comparison function to be used.
16. Has keyword parameters `key-compare-fn-name' and `val-compare-fn-name'.  If
    nonnull, these must be symbols, the names of the key and value comparison
    functions, respectively, to be used.
17. The default of the result is the same as that of the argument."))

;;; The `&allow-other-keys' is to persuade SBCL not to issue warnings about keywords
;;; that are accepted by some methods of `convert'.  This differs from the behavior
;;; we would get by specifying `&allow-other-keys' in the `defgeneric' parameter list;
;;; this way, we still get runtime errors on unexpected keywords (which I guess is an
;;; SBCL extension, since CLHS 7.6.4 says we shouldn't).
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
    "Returns an iterator for the collection.  \(These are stateful iterators and
are not thread-safe; if you want a pure iterator, see `fun-iterator'.\)  The iterator
is a function of one argument; given `:done?', it returns true iff the iterator is
exhausted; given `:more?', it returns true iff the iterator is _not_ exhausted.
Given `:get', if the iterator is not exhausted, it returns the next element (or
pair, for a map, as two values), with the second value (third, for a map) being
true, and advances one element; if it is exhausted, it returns two `nil' values
\(three, for a map\).

The bag method takes a `pairs?' keyword argument; if true, it returns each element
only once, with its multiplicity as the second value, as for a map."))

(defgeneric fun-iterator (collection &key from-end?)
  (:documentation
    "Returns a functional iterator for the collection.  \(These iterators are
thread-safe.\)  The iterator is a function of one argument; given `:empty?', it
returns true iff the iterator is exhausted; given `:more?', it returns true iff
the iterator is _not_ exhausted.  Given `:first', if it is not exhausted, it
returns the next element \(pair, for a map, as two values\), with an additional
true value; if it is exhausted, it returns two \(three, for a map\) `nil'
values.  Given `:rest', if it is not exhausted, it returns an iterator for the
rest of the collection.

If `from-end?' is true, the collection is iterated in reverse order.  The bag
method also takes a `pairs?' keyword argument; if true, it returns each element
only once, with its multiplicity as the second value, as for a map."))

;;; Iterators for the Lisp sequence types are useful for some generic operations
;;; (e.g. `some' and friends).
(defmethod iterator ((ls list) &key)
  (lambda (op)
    (ecase op
      (:get (if ls (values (pop ls) t)
	      (values nil nil)))
      (:done? (null ls))
      (:more? ls))))

;;; Not sure we have any use for functional iterators on the Lisp sequence types,
;;; but for completeness, here they are anyway.
;;; Of course, if you know the sequence is a list, `car' and `cdr' will be much faster.
(defmethod fun-iterator ((ls list) &key from-end?)
  (labels ((iter (ls)
	     (if (null ls)
		 (lambda (op)
		   (ecase op
		     (:first (values nil nil))
		     (:empty? t)
		     (:more? nil)))
	       (lambda (op)
		 (ecase op
		   (:first (values (car ls) t))
		   (:rest (iter (cdr ls)))
		   (:empty? nil)
		   (:more? t))))))
    (iter (if from-end? (reverse ls) ls))))

(defmethod iterator ((vec vector) &key)
  (let ((idx 0)
	(len (length vec)))
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

;;; Again, if you know it's a vector, you're much better off just indexing it.
(defmethod fun-iterator ((vec vector) &key from-end?)
  (labels ((done ()
	     (lambda (op)
	       (ecase op
		 (:first (values nil nil))
		 (:empty? t)
		 (:more? nil)))))
    (if from-end?
	(if (simple-vector-p vec)
	    (labels ((iter (i)
		       (if (>= i 0)
			   (lambda (op)
			     (ecase op
			       (:first (values (svref vec i) t))
			       (:rest (iter (1- i)))
			       (:empty? nil)
			       (:more? t)))
			 (done))))
	      (iter (1- (length vec))))
	  (labels ((iter (i)
		     (if (>= i 0)
			 (lambda (op)
			   (ecase op
			     (:first (values (aref vec i) t))
			     (:rest (iter (1- i)))
			     (:empty? nil)
			     (:more? t)))
		       (done))))
	    (iter (1- (length vec)))))
      (let ((len (length vec)))
	(if (simple-vector-p vec)
	    (labels ((iter (i)
		       (if (< i len)
			   (lambda (op)
			     (ecase op
			       (:first (values (svref vec i) t))
			       (:rest (iter (1+ i)))
			       (:empty? nil)
			       (:more? t)))
			 (done))))
	      (iter 0))
	  (labels ((iter (i)
		     (if (< i len)
			 (lambda (op)
			   (ecase op
			     (:first (values (aref vec i) t))
			     (:rest (iter (1+ i)))
			     (:empty? nil)
			     (:more? t)))
		       (done))))
	    (iter 0)))))))

(defmethod iterator ((str string) &key)
  (let ((idx 0)
	(len (length str)))
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

(defmethod fun-iterator ((str string) &key from-end?)
  (labels ((done ()
	     (lambda (op)
	       (ecase op
		 (:first (values nil nil))
		 (:empty? t)
		 (:more? nil)))))
    (if from-end?
	(if (simple-string-p str)
	    (labels ((iter (i)
		       (if (>= i 0)
			   (lambda (op)
			     (ecase op
			       (:first (values (schar str i) t))
			       (:rest (iter (1- i)))
			       (:empty? nil)
			       (:more? t)))
			 (done))))
	      (iter (1- (length str))))
	  (labels ((iter (i)
		     (if (>= i 0)
			 (lambda (op)
			   (ecase op
			     (:first (values (char str i) t))
			     (:rest (iter (1- i)))
			     (:empty? nil)
			     (:more? t)))
		       (done))))
	    (iter (1- (length str)))))
      (let ((len (length str)))
	(if (simple-string-p str)
	    (labels ((iter (i)
		       (if (< i len)
			   (lambda (op)
			     (ecase op
			       (:first (values (schar str i) t))
			       (:rest (iter (1+ i)))
			       (:empty? nil)
			       (:more? t)))
			 (done))))
	      (iter 0))
	  (labels ((iter (i)
		     (if (< i len)
			 (lambda (op)
			   (ecase op
			     (:first (values (char str i) t))
			     (:rest (iter (1+ i)))
			     (:empty? nil)
			     (:more? t)))
		       (done))))
	    (iter 0)))))))

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

(defmethod fun-iterator ((seq sequence) &key from-end?)
  (labels ((done ()
	     (lambda (op)
	       (ecase op
		 (:first (values nil nil))
		 (:empty? t)
		 (:more? nil)))))
    (if from-end?
	(labels ((iter (i)
		   (if (>= i 0)
		       (lambda (op)
			 (ecase op
			   (:first (values (elt seq i) t))
			   (:rest (iter (1- i)))
			   (:empty? nil)
			   (:more? t)))
		     (done))))
	  (iter (1- (length seq))))
      (let ((len (length seq)))
	(labels ((iter (i)
		   (if (< i len)
		       (lambda (op)
			 (ecase op
			   (:first (values (elt seq i) t))
			   (:rest (iter (1+ i)))
			   (:empty? nil)
			   (:more? t)))
		     (done))))
	  (iter 0))))))

;;; This "old syntax" GMap call also defines `:sequence'.
(gmap:def-gmap-arg-type sequence (seq)
  "Yields the elements of `seq', which can be of any CL sequence type as well
as an FSet seq, or a set or bag as well."
  `((the function (iterator ,seq))
    #'(lambda (it) (declare (type function it)) (funcall it ':done?))
    #'(lambda (it) (declare (type function it)) (funcall it ':get))))

(gmap:def-arg-type iterator (it)
  `(,it
    #'(lambda (it) (declare (type function it)) (funcall it ':done?))
    #'(lambda (it) (declare (type function it)) (funcall it ':get))))

;;; The new (for 1.4.7) functional iterators all take a `:from-end?' option.
;;; Other than that, there's no reason to use them with `gmap'; they're slower
;;; (though not as much slower as you might expect; less than 2x) and generate
;;; lots of garbage.  They were fun to write, though :-)
(gmap:def-arg-type fun-sequence (fun-iterable &key from-end?)
  "Yields the elements of `fun-iterable', which can be an FSet seq, set, or bag.
If `:from-end?' is true, iterates in reverse order."
  `((the function (fun-iterator ,fun-iterable :from-end? ,from-end?))
    #'(lambda (it) (funcall it ':empty?))
    #'(lambda (it) (funcall it ':first))
    #'(lambda (it) (funcall it ':rest))))


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

(declaim (inline coerce-to-function coerce-to-function-or-equal?))
(defun coerce-to-function (s)
  (coerce s 'function))

(defun coerce-to-function-or-equal? (s)
  (cond ((null s) (values #'equal? t))
	((symbolp s)
	 (values (coerce s 'function) (eq s 'equal?)))
	((functionp s)
	 (values s (eq s #'equal?)))
	(t
	 (values (coerce s 'function) nil))))

;;; `(gmap :or ...)' is a bit faster.
(defun some (pred sequence0 &rest more-sequences)
  "FSet generic version of `cl:some'."
  (let ((it0 (the function (iterator sequence0)))
	(more-its (mapcar #'iterator more-sequences))
	(pred (coerce-to-function pred)))
    (do ()
	((or (funcall it0 ':done?)
	     (gmap (:result or) (fn (it) (funcall it ':done?))
		   (:arg list more-its)))
	 nil)
      (let ((val (apply pred (funcall it0 ':get)
			(mapcar (fn (it) (funcall it ':get)) more-its))))
	(when val
	  (return val))))))

;;; `(gmap (:result and) ...)' is a bit faster.
(defun every (pred sequence0 &rest more-sequences)
  "FSet generic version of `cl:every'."
  (let ((it0 (the function (iterator sequence0)))
	(more-its (mapcar #'iterator more-sequences))
	(pred (coerce-to-function pred)))
    (do ()
	((or (funcall it0 ':done?)
	     (gmap (:result or) (fn (it) (funcall it ':done?))
		   (:arg list more-its)))
	 t)
      (let ((val (apply pred (funcall it0 ':get)
			(mapcar (fn (it) (funcall it ':get)) more-its))))
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
	   (ignorable test test-not))
  (apply #'cl:union ls1 ls2 keyword-args))

(defmethod intersection ((ls1 list) (ls2 list) &rest keyword-args &key test test-not)
  (declare (dynamic-extent keyword-args)
	   (ignorable test test-not))
  (apply #'cl:intersection ls1 ls2 keyword-args))

(defmethod set-difference ((ls1 list) (ls2 list) &rest keyword-args &key test test-not)
  (declare (dynamic-extent keyword-args)
	   (ignorable test test-not))
  (apply #'cl:set-difference ls1 ls2 keyword-args))


;;; ================================================================================
;;; FSet methods for CL sequences

(defmethod with-first ((ls list) val)
  (cons val ls))

(defmethod less-first ((ls list))
  (cdr ls))

(defmethod with-last ((ls list) val)
  (append ls (list val)))

(defmethod less-last ((ls list))
  (butlast ls))

(defmethod contains? ((ls list) x &optional (y nil y?))
  (declare (ignore y))
  (check-two-arguments y? 'contains 'list)
  (member x ls :test #'equal?))

(defmethod concat ((a list) &rest seqs)
  (append a (reduce #'append seqs :key (fn (x) (convert 'list x)) :from-end t)))

(defmethod convert ((to-type (eql 'list)) (ls list) &key)
  ls)

(defmethod filter ((pred symbol) (ls list))
  (remove-if-not (coerce pred 'function) ls))

(defmethod filter ((pred function) (ls list))
  (remove-if-not pred ls))

(defmethod convert ((to-type (eql 'vector)) (v vector) &key)
  v)

(defmethod partition ((pred symbol) (ls list))
  (list-partition (coerce-to-function pred) ls))

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

(defmethod image ((fn function) (l list))
  (mapcar fn l))

(defmethod image ((fn symbol) (l list))
  (mapcar (coerce fn 'function) l))

(defmethod image ((fn map) (l list))
  (mapcar (lambda (x) (lookup fn x)) l))

(defmethod image ((fn set) (l list))
  (mapcar (lambda (x) (lookup fn x)) l))

(defmethod image ((fn function) (l vector))
  (cl:map 'vector fn l))

(defmethod image ((fn symbol) (l vector))
  (cl:map 'vector (coerce fn 'function) l))

(defmethod image ((fn map) (l vector))
  (cl:map 'vector (lambda (x) (lookup fn x)) l))

(defmethod image ((fn set) (l vector))
  (cl:map 'vector (lambda (x) (lookup fn x)) l))


;;; ----------------
;;; This series of methods provides an elegant way to do functional update on small lists.
;;; E.g., (incf (@ x 'first)) --> (setq x (cons (1+ (car x)) (cdr x))).
;;;
;;; I've defined all ten of these because CL has them, but as a stylistic recommendation, if
;;; your list is longer than three or maybe four, you should probably use a dynamic tuple;
;;; see `tuples.lisp'.

(defmethod lookup ((ls list) (key (eql 'first)))
  (cl:first ls))
(defmethod lookup ((ls list) (key (eql 'cl:first)))  ; in case they don't shadowing-import `fset:first'
  (cl:first ls))

(defmethod with ((ls list) (key (eql 'first)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (cons val (cdr ls)))
(defmethod with ((ls list) (key (eql 'cl:first)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (cons val (cdr ls)))

(defmethod lookup ((ls list) (key (eql 'second)))
  (second ls))

(defmethod with ((ls list) (key (eql 'second)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (list* (cl:first ls) val (cddr ls)))

(defmethod lookup ((ls list) (key (eql 'third)))
  (third ls))

(defmethod with ((ls list) (key (eql 'third)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (list* (cl:first ls) (second ls) val (cdddr ls)))

(defmethod lookup ((ls list) (key (eql 'fourth)))
  (fourth ls))

(defmethod with ((ls list) (key (eql 'fourth)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (list* (cl:first ls) (second ls) (third ls) val (cddddr ls)))

(defmethod lookup ((ls list) (key (eql 'fifth)))
  (fifth ls))

(defmethod with ((ls list) (key (eql 'fifth)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (list* (cl:first ls) (second ls) (third ls) (fourth ls) val (cdr (cddddr ls))))

(defmethod lookup ((ls list) (key (eql 'sixth)))
  (sixth ls))

(defmethod with ((ls list) (key (eql 'sixth)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (list* (cl:first ls) (second ls) (third ls) (fourth ls) (fifth ls) val (cddr (cddddr ls))))

(defmethod lookup ((ls list) (key (eql 'seventh)))
  (seventh ls))

(defmethod with ((ls list) (key (eql 'seventh)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (list* (cl:first ls) (second ls) (third ls) (fourth ls) (fifth ls) (sixth ls) val (cdddr (cddddr ls))))

(defmethod lookup ((ls list) (key (eql 'eighth)))
  (eighth ls))

(defmethod with ((ls list) (key (eql 'eighth)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (list* (cl:first ls) (second ls) (third ls) (fourth ls) (fifth ls) (sixth ls) (seventh ls) val (cddddr (cddddr ls))))

(defmethod lookup ((ls list) (key (eql 'ninth)))
  (ninth ls))

(defmethod with ((ls list) (key (eql 'ninth)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (list* (cl:first ls) (second ls) (third ls) (fourth ls) (fifth ls) (sixth ls) (seventh ls) (eighth ls)
	 val (cdr (cddddr (cddddr ls)))))

(defmethod lookup ((ls list) (key (eql 'tenth)))
  (tenth ls))

(defmethod with ((ls list) (key (eql 'tenth)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (list* (cl:first ls) (second ls) (third ls) (fourth ls) (fifth ls) (sixth ls) (seventh ls) (eighth ls)
	 (ninth ls) val (cddr (cddddr (cddddr ls)))))


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
anyway.  `fn' can be a function object, an fbound symbol, or a map."
  (labels ((rec (fn coll keys)
	     (if (null keys) (@ (if (symbolp fn) (coerce fn 'function) fn) coll)
	       (with coll (car keys) (rec fn (lookup coll (car keys)) (cdr keys))))))
    (rec fn coll keys)))

;;; If the `fn' is nontrivial, binds a variable to it with a `dynamic-extent' declaration.
;;; (Really, should do this for `image', `filter', etc. etc.)
(define-compiler-macro update (&whole form fn coll &rest keys)
  (if (not (or (symbolp fn)
	       (and (listp fn)
		    (eq (car fn) 'function)
		    (symbolp (cadr fn)))))
      (let ((fn-var (gensymx #:fn-)))
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

;;; Terminological note: a "compare-fn" is what the user supplies; an "org" (short for
;;; "organization") is the internal object holding the compare-fn's name and function
;;; object, for efficient access (for hashed collections, it also holds the hash function(s)).
(defstruct (tree-set-org
	     (:constructor make-tree-set-org (compare-fn-name compare-fn))
	     (:predicate tree-set-org?)
	     (:copier nil))
  "Defines an ordering to be used by `wb-custom-set'.  The name is used
when printing instances."
  (compare-fn-name nil :type symbol :read-only t)
  (compare-fn nil :type function :read-only t))

(deflex +fset-default-tree-set-org+ (make-tree-set-org 'compare #'compare))

(declaim (inline make-wb-set make-wb-custom-set))

(defstruct (wb-set
	     (:include set)
	     (:constructor nil)
	     (:predicate wb-set?)
	     (:copier nil))
  "An abstract class for functional sets represented as weight-balanced binary
trees."
  (contents nil :read-only t))

;;; For WB-sets, the space overhead of carrying around the comparison function in every instance
;;; of this wrapper class, plus the time required to initialize it, is measurable -- for a singleton
;;; set, space increases from 4 to 5 words, and a micro-benchmark that does nothing but create
;;; singleton sets runs 9% slower.  That's an extreme case; in real code, the difference will
;;; probably not be noticeable.  But I went ahead and added `wb-custom-set` before making this
;;; measurement, and having written it, I'm inclined to leave it in.  `WB-Set' will be the only
;;; class I do this for, though.
(defstruct (wb-default-set
	     (:include wb-set)
	     (:constructor make-wb-default-set (contents))
	     (:predicate wb-default-set?)
	     (:print-function print-wb-default-set)
	     (:copier nil))
  "A class of functional sets represented as weight-balanced binary trees, and
ordered by `fset:compare'.  This is the default implementation of sets in FSet.")

(defstruct (wb-custom-set
	     (:include wb-set)
	     (:constructor make-wb-custom-set (contents org))
	     (:predicate wb-custom-set?)
	     (:print-function print-wb-custom-set)
	     (:copier nil))
  "A class of functional sets represented as weight-balanced binary trees, with a
custom comparison function."
  (org nil :type tree-set-org :read-only t))

(declaim (inline wb-set-org))
(defun wb-set-org (s)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set s))
  (if (wb-custom-set? s)
      (wb-custom-set-org s)
    +fset-default-tree-set-org+))

(declaim (inline wb-set-compare-fn))
(defun wb-set-compare-fn (s)
  (if (wb-custom-set? s)
      (tree-set-org-compare-fn (wb-custom-set-org s))
    #'compare))

(declaim (inline wb-set-compare-fn-name))
(defun wb-set-compare-fn-name (s)
  (if (wb-custom-set? s)
      (tree-set-org-compare-fn-name (wb-custom-set-org s))
    'compare))

(declaim (inline make-wb-set))
(defun make-wb-set (contents &optional org)
  (if (or (null org) (eq (tree-set-org-compare-fn org) #'compare))
      (make-wb-default-set contents)
    (make-wb-custom-set contents org)))

(defparameter *empty-wb-set* (make-wb-set nil))

(declaim (inline empty-set))
(defun empty-set ()
  "Returns an empty set of the default implementation."
  *empty-wb-set*)

;; For the constructor macros.
(defmethod empty-instance-function ((class-name (eql 'set)))
  'empty-set)

(declaim (inline empty-wb-set))
(defun empty-wb-set (&optional compare-fn-name)
  "Returns an empty wb-set.  By default, it will be ordered by `fset:compare';
to use a custom ordering, supply the comparison function name (a symbol) as
`compare-fn-name'."
  (if (null compare-fn-name)
      *empty-wb-set*
    (empty-wb-custom-set compare-fn-name)))

;;; We cache the last empty `wb-custom-set' instance with a given org, so as to reuse them
;;; in the common case.
(deflex +empty-wb-custom-set-cache+ (make-hash-table :test 'equal))

;;; The goal here is to take the `symbol-function' of the comparison function exactly when
;;; the set is created.  Taking it earlier would mean we wouldn't notice if it were redefined;
;;; taking it later would break existing sets if the function were modified.
(defun empty-wb-custom-set (compare-fn-name)
  "Returns an empty `wb-set' ordered according to `compare-fn-name', which
must be a symbol."
  (assert (and (symbolp compare-fn-name) (not (null compare-fn-name))))
  (if (eq compare-fn-name 'compare)
      *empty-wb-set*
    (let ((prev-instance (gethash compare-fn-name +empty-wb-custom-set-cache+))
	  (compare-fn (symbol-function compare-fn-name)))
      (if (and prev-instance
	       (eq compare-fn (wb-set-compare-fn prev-instance)))
	  prev-instance
	(setf (gethash compare-fn-name +empty-wb-custom-set-cache+)
	      (make-wb-custom-set nil (make-tree-set-org compare-fn-name compare-fn)))))))

(defmethod empty-instance-function ((class-name (eql 'wb-set)))
  `empty-wb-set)

(defgeneric empty-set-like (s)
  (:documentation
    "Returns an empty set of the same implementation, and using the same compare
or hash function, as `s'."))

(defmethod empty-set-like ((s wb-default-set))
  (empty-set))

(defmethod empty-set-like ((s wb-custom-set))
  (empty-wb-custom-set (tree-set-org-compare-fn-name (wb-custom-set-org s))))

(defmethod compare-fn ((s wb-default-set))
  #'compare)

(defmethod compare-fn ((s wb-custom-set))
  (tree-set-org-compare-fn (wb-custom-set-org s)))

(defmethod compare-fn-name ((s wb-default-set))
  'compare)

(defmethod compare-fn-name ((s wb-custom-set))
  (tree-set-org-compare-fn-name (wb-custom-set-org s)))

(define-wb-set-methods empty? ((s wb-set))
  (null (contents s)))

(define-wb-set-methods size ((s wb-set))
  (WB-Set-Tree-Size (contents s)))

(define-wb-set-methods set-size ((s wb-set))
  (WB-Set-Tree-Size (contents s)))

(define-wb-set-methods arb ((s wb-set))
  (let ((tree (contents s)))
    (if tree (values (WB-Set-Tree-Arb tree) t)
      (values nil nil))))

(define-wb-set-methods contains? ((s wb-set) x &optional (y nil y?))
  (declare (ignore y))
  (check-two-arguments y? 'contains? 'wb-set)
  (WB-Set-Tree-Member? (contents s) x (compare-fn s)))

;;; Note, first value is `t' or `nil'.
(define-wb-set-methods lookup ((s wb-set) key)
  (WB-Set-Tree-Find-Equal (contents s) key (compare-fn s)))

(define-wb-set-methods rank ((s wb-set) x)
  (let ((found? rank (WB-Set-Tree-Rank (contents s) x (compare-fn s))))
    (values (if found? rank (1- rank)) found?)))

(define-wb-set-methods at-rank ((s wb-set) rank)
  (let ((contents (contents s))
	((size (WB-Set-Tree-Size contents))))
    (unless (and (>= rank 0) (< rank size))
      (error 'simple-type-error :datum rank :expected-type `(integer 0 (,size))
	     :format-control "Rank ~D out of bounds on ~A"
	     :format-arguments (list rank s)))
    (WB-Set-Tree-Rank-Element contents rank)))

(define-wb-set-methods least ((s wb-set))
  (let ((tree (contents s)))
    (if tree (values (WB-Set-Tree-Least tree) t)
      (values nil nil))))

(define-wb-set-methods greatest ((s wb-set))
  (let ((tree (contents s)))
    (if tree (values (WB-Set-Tree-Greatest tree) t)
        (values nil nil))))

(define-wb-set-methods with ((s wb-set) value &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'with 'wb-set)
  (let ((contents (contents s))
	((new-contents (WB-Set-Tree-With contents value (compare-fn s)))))
    (if (eq new-contents contents)
	s
      (make s new-contents))))

(define-wb-set-methods less ((s wb-set) value &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'wb-set)
  (let ((contents (contents s))
	((new-contents (WB-Set-Tree-Less contents value (compare-fn s)))))
    (if (eq new-contents contents)
	s
      (make s new-contents))))

(define-wb-set-methods split-from ((s wb-set) value)
  (let ((new-contents (WB-Set-Tree-Split-Above (contents s) value (compare-fn s))))
    (make s (if (WB-Set-Tree-Member? (contents s) value (compare-fn s))
		(WB-Set-Tree-With new-contents value (compare-fn s))
	      new-contents))))

(define-wb-set-methods split-above ((s wb-set) value)
  (make s (WB-Set-Tree-Split-Above (contents s) value (compare-fn s))))

(define-wb-set-methods split-through ((s wb-set) value)
  (let ((new-contents (WB-Set-Tree-Split-Below (contents s) value (compare-fn s))))
    (make s (if (WB-Set-Tree-Member? (contents s) value (compare-fn s))
		(WB-Set-Tree-With new-contents value (compare-fn s))
	      new-contents))))

(define-wb-set-methods split-below ((s wb-set) value)
  (make s (WB-Set-Tree-Split-Below (contents s) value (compare-fn s))))

(defmethod union ((s1 set) (s2 set) &key)
  "Fallback method for mixed implementations."
  (let ((result s1))
    (do-set (x s2)
      (includef result x))
    result))

(define-wb-set-methods union ((s1 wb-set) (s2 wb-set) &key)
  (if-same-compare-fns (s1 s2)
      (make s1 (WB-Set-Tree-Union (contents s1) (contents s2) (compare-fn s1)))
    (call-next-method)))

(defmethod intersection ((s1 set) (s2 set) &key)
  "Fallback method for mixed implementations."
  (let ((result (empty-set-like s1)))
    (do-set (x s2)
      (when (contains? s1 x)
	(includef result x)))
    result))

(define-wb-set-methods intersection ((s1 wb-set) (s2 wb-set) &key)
  (if-same-compare-fns (s1 s2)
      (make s1 (WB-Set-Tree-Intersect (contents s1) (contents s2) (compare-fn s1)))
    (call-next-method)))

(defmethod set-difference ((s1 set) (s2 set) &key)
  "Fallback method for mixed implementations."
  (let ((result s1))
    (do-set (x s2)
      (excludef result x))
    result))

(define-wb-set-methods set-difference ((s1 wb-set) (s2 wb-set) &key)
  (if-same-compare-fns (s1 s2)
      (make s1 (WB-Set-Tree-Diff (contents s1) (contents s2) (compare-fn s1)))
    (call-next-method)))

(defmethod set-difference-2 ((s1 set) (s2 set))
  "Fallback method for mixed implementations."
  (let ((res1 s1)
	(res2 s2))
    (do-set (x s2)
      (excludef res1 x))
    (do-set (x s1)
      (excludef res2 x))
    (values res1 res2)))

(define-wb-set-methods set-difference-2 ((s1 wb-set) (s2 wb-set))
  (if-same-compare-fns (s1 s2)
      (let ((newc1 newc2 (WB-Set-Tree-Diff-2 (contents s1) (contents s2) (compare-fn s1))))
	(values (make s1 newc1) (make s2 newc2)))
    (call-next-method)))

(defmethod subset? ((s1 set) (s2 set))
  "Fallback method for mixed implementations."
  (do-set (x s1 t)
    (unless (contains? s2 x)
      (return nil))))

(define-wb-set-methods subset? ((s1 wb-set) (s2 wb-set))
  (if-same-compare-fns (s1 s2)
      (WB-Set-Tree-Subset? (contents s1) (contents s2) (compare-fn s1))
    (call-next-method)))

(defun proper-subset? (sub super)
  "Returns true iff `sub' is a proper subset of `super', that is, `sub' is
a subset of `super' and the two are not equal."
  (and (subset? sub super)
       (< (size sub) (size super))))

(defmethod disjoint? ((s1 set) (s2 set))
  "Fallback method for mixed implementations."
  (let ((s1 s2 (if (< (size s1) (size s2))
		   (values s2 s1)
		 (values s1 s2))))
    (do-set (x s2 t)
      (when (contains? s1 x)
	(return nil)))))

(define-wb-set-methods disjoint? ((s1 wb-set) (s2 wb-set))
  (if-same-compare-fns (s1 s2)
      (WB-Set-Tree-Disjoint? (contents s1) (contents s2) (compare-fn s1))
    (call-next-method)))

;;; This is a little inconsistent with FSet's past practice, though not in a way that could
;;; break existing code (unless it uses CH-sets, which are still in a pre-alpha state).
;;; But it's in the direction FSet is going.  Specifically, it will report two sets as equal
;;; iff they contain the same elements; the root method on `compare' distinguishes by class,
;;; so that two sets of different classes could never be equal.
(defmethod compare ((s1 set) (s2 set))
  "Fallback method for mixed implementations."
  (let ((size1 (size s1))
	(size2 (size s2)))
    (cond ((< size1 size2) ':less)
	  ((> size1 size2) ':greater)
	  ((do-set (x s1 t)
	     (unless (contains? s2 x)
	       (return nil)))
	   ':equal)
	  (t ':unequal))))

(define-wb-set-methods compare ((s1 wb-set) (s2 wb-set))
  (if-same-compare-fns (s1 s2)
      (WB-Set-Tree-Compare (contents s1) (contents s2) (compare-fn s1))
    (call-next-method)))

(defgeneric internal-do-set (set elt-fn value-fn)
  (:documentation
    "Calls `elt-fn' on successive elements of the set; when done, calls `value-fn'
on no arguments and returns the result(s).  This is called by `do-set' to provide
for the possibility of different set implementations; it is not for public use.
`elt-fn' and `value-fn' must be function objects, not symbols."))

(define-wb-set-methods internal-do-set ((s wb-set) elt-fn value-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  (Do-WB-Set-Tree-Members (x (contents s) (funcall value-fn))
    (funcall elt-fn x)))

(define-wb-set-methods iterator ((s wb-set) &key)
  (Make-WB-Set-Tree-Iterator (contents s)))

(define-wb-set-methods fun-iterator ((s wb-set) &key from-end?)
  (if from-end?
      (WB-Set-Tree-Rev-Fun-Iter (contents s))
    (WB-Set-Tree-Fun-Iter (contents s))))

(define-wb-set-methods filter ((pred function) (s wb-set))
  (make s (wb-set-filter pred (contents s) (compare-fn s))))

(define-wb-set-methods filter ((pred symbol) (s wb-set))
  (make s (wb-set-filter (coerce-to-function pred) (contents s) (compare-fn s))))

(define-wb-set-methods filter ((pred map) (s wb-set))
  (make s (wb-set-filter #'(lambda (x) (lookup pred x)) (contents s) (compare-fn s))))

(defun wb-set-filter (pred contents compare-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function pred))
  (let ((result nil))
    (do-wb-set-tree-members (x contents)
      (when (funcall pred x)
	(setq result (WB-Set-Tree-With result x compare-fn))))
    result))

(define-wb-set-methods partition ((pred function) (s wb-set))
  (let ((res1 res2 (wb-set-partition pred (contents s) (compare-fn s))))
    (values (make s res1) (make s res2))))

(define-wb-set-methods partition ((pred symbol) (s wb-set))
  (let ((res1 res2 (wb-set-partition (coerce-to-function pred) (contents s) (compare-fn s))))
    (values (make s res1) (make s res2))))

(define-wb-set-methods partition ((pred map) (s wb-set))
  (let ((res1 res2 (wb-set-partition #'(lambda (x) (lookup pred x)) (contents s) (compare-fn s))))
    (values (make s res1) (make s res2))))

(defun wb-set-partition (pred contents compare-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function pred))
  (let ((result-1 nil)
	(result-2 nil))
    (do-wb-set-tree-members (x contents)
      (if (funcall pred x)
	  (setq result-1 (WB-Set-Tree-With result-1 x compare-fn))
	(setq result-2 (WB-Set-Tree-With result-2 x compare-fn))))
    (values result-1 result-2)))

;;; A set is another kind of boolean-valued map.
(defmethod filter ((pred set) (s set))
  (intersection pred s))

;;; A bag is yet another kind of boolean-valued map.
(defmethod filter ((pred bag) (s set))
  (intersection pred s))

(define-wb-set-methods image ((fn function) (s wb-set))
  (make s (wb-set-image fn (contents s) (compare-fn s))))

(define-wb-set-methods image ((fn symbol) (s wb-set))
  (make s (wb-set-image (coerce-to-function fn) (contents s) (compare-fn s))))

(define-wb-set-methods image ((fn map) (s wb-set))
  (make s (wb-set-image fn (contents s) (compare-fn s))))

(define-wb-set-methods image ((fn set) (s wb-set))
  (make s (wb-set-image fn (contents s) (compare-fn s))))

(define-wb-set-methods image ((fn bag) (s wb-set))
  (make s (wb-set-image fn (contents s) (compare-fn s))))

(defun wb-set-image (fn contents compare-fn)
  (let ((result nil))
    (do-wb-set-tree-members (x contents)
      (setq result (WB-Set-Tree-With result (@ fn x) compare-fn)))
    result))

(defmethod reduce ((fn function) (s set) &key key (initial-value nil init?))
  (set-reduce fn s initial-value (and key (coerce-to-function key)) init?))

(defmethod reduce ((fn symbol) (s set) &key key (initial-value nil init?))
  (set-reduce (coerce-to-function fn) s initial-value (and key (coerce-to-function key))
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

(defmethod convert ((to-type (eql 'wb-set)) (s set) &key compare-fn-name)
  (convert-to-wb-set s nil
    (let ((tree nil))
      (do-set (x s tree)
	(setq tree (WB-Set-Tree-With tree x compare-fn))))))

(define-wb-set-methods convert ((to-type (eql 'wb-set)) (s wb-set) &key compare-fn-name)
  (convert-to-wb-set s (eq compare-fn (compare-fn s))
    (let ((tree nil))
      (do-wb-set-tree-members (x (contents s) tree)
	(setq tree (WB-Set-Tree-With tree x compare-fn))))))

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

(defmethod convert ((to-type (eql 'set)) (l list) &key input-sorted?)
  (make-wb-set (wb-set-from-list l input-sorted? #'compare)))

(defmethod convert ((to-type (eql 'wb-set)) (l list) &key input-sorted? compare-fn-name)
  (convert-to-wb-set s nil
    (wb-set-from-list l input-sorted? compare-fn)))

(defun wb-set-from-list (l input-sorted? compare-fn)
  (if input-sorted?
      (WB-Set-Tree-From-Sorted-Iterable (the function (iterator l)) (length l) compare-fn)
    (WB-Set-Tree-From-List l compare-fn)))

(defmethod convert ((to-type (eql 'set)) (s seq) &key input-sorted?)
  (make-wb-set (wb-set-from-sequence s input-sorted? #'compare)))

(defmethod convert ((to-type (eql 'wb-set)) (s seq) &key input-sorted? compare-fn-name)
  (convert-to-wb-set s nil
    (wb-set-from-sequence s input-sorted? compare-fn)))

(defmethod convert ((to-type (eql 'set)) (s sequence) &key input-sorted?)
  (make-wb-set (wb-set-from-sequence s input-sorted? #'compare)))

(defmethod convert ((to-type (eql 'wb-set)) (s sequence) &key input-sorted? compare-fn-name)
  (convert-to-wb-set s nil
    (wb-set-from-sequence s input-sorted? compare-fn)))

(defun wb-set-from-sequence (s input-sorted? compare-fn)
  (if input-sorted?
      (WB-Set-Tree-From-Sorted-Iterable (the function (iterator s)) (size s) compare-fn)
    (WB-Set-Tree-From-Iterable (the function (iterator s)) compare-fn)))

(defmethod find (item (s set) &key key test)
  (declare (optimize (speed 3) (safety 0)))
  (let ((test (coerce-to-function-or-equal? test)))
    (if key
        (let ((key (coerce-to-function key)))
          (do-set (x s)
            (when (funcall test item (funcall key x))
              (return x))))
        (if (not (eq test #'equal?))
            (do-set (x s)
              (when (funcall test item x)
                (return x)))
            (nth-value 1 (lookup s item))))))

(defmethod find-if (pred (s set) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce-to-function pred)))
    (if key
	(let ((key (coerce-to-function key)))
	  (do-set (x s)
	    (when (funcall pred (funcall key x))
	      (return x))))
      (do-set (x s)
	(when (funcall pred x)
	  (return x))))))

(defmethod find-if-not (pred (s set) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce-to-function pred)))
    (find-if #'(lambda (x) (not (funcall pred x))) s :key key)))

(defmethod count (item (s set) &key key test)
  (declare (optimize (speed 3) (safety 0)))
  (let ((test default? (coerce-to-function-or-equal? test)))
    (if key
	(let ((key (coerce-to-function key))
              (total 0))
          (declare (fixnum total))
          (do-set (x s total)
            (when (funcall test item (funcall key x))
              (incf total))))
      (if default?
          (if (lookup s item) 1 0)
        (let ((total 0))
	  (declare (fixnum total))
	  (do-set (x s total)
	    (when (funcall test item x)
	      (incf total))))))))

(defmethod count-if (pred (s set) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce-to-function pred))
	(n 0))
    (declare (fixnum n))
    (if key
	(let ((key (coerce-to-function key)))
	  (do-set (x s n)
	    (when (funcall pred (funcall key x))
	      (incf n))))
      (do-set (x s n)
	(when (funcall pred x)
	  (incf n))))))

(defmethod count-if-not (pred (s set) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce-to-function pred)))
    (count-if #'(lambda (x) (not (funcall pred x))) s :key key)))

(defun print-wb-default-set (set stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "#{" :suffix " }")
    (do-set (x set)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (write x :stream stream))))

(defun print-wb-custom-set (set stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "#{"
				    :suffix (format nil " }[~S]"
 						    (tree-set-org-compare-fn-name
						      (wb-custom-set-org set))))
    (do-set (x set)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (write x :stream stream))))

(gmap:def-gmap-arg-type set (set)
  "Yields the elements of `set'."
  `((the function (iterator ,set))
    #'(lambda (it) (funcall it ':done?))
    #'(lambda (it) (funcall it ':get))))

(gmap:def-gmap-res-type set (&key filterp)
  "Returns a set of the values, optionally filtered by `filterp'."
  `(nil #'(lambda (s x) (WB-Set-Tree-With s x #'compare)) #'make-wb-set ,filterp))


;;; A bit faster than `set', if you know it's a `wb-set'.
(gmap:def-gmap-arg-type wb-set (set)
  "Yields the elements of `set'."
  `((Make-WB-Set-Tree-Iterator-Internal (wb-set-contents ,set))
    #'WB-Set-Tree-Iterator-Done?
    #'WB-Set-Tree-Iterator-Get))

(gmap:def-gmap-res-type wb-set (&key filterp compare-fn-name)
  "Returns a set of the values, optionally filtered by `filterp'.  If
`compare-fn-name' is nonnull, it specifies a custom ordering."
  (let ((proto-var (gensymx #:prototype-))
	(cf-var (gensymx #:cmp-)))
    `(nil #'(lambda (s x) (WB-Set-Tree-With s x ,cf-var))
	  #'(lambda (s)
	      (if (or (null compare-fn-name) (eq (wb-set-compare-fn ,proto-var) #'compare))
		  (make-wb-set s)
		(make-wb-custom-set s (wb-custom-set-org ,proto-var))))
	  ,filterp
	  ((,proto-var (empty-wb-set ,compare-fn-name))
	   ((,cf-var (wb-set-compare-fn ,proto-var)))))))


(gmap:def-gmap-res-type union (&key filterp)
  "Returns the union of the values, optionally filtered by `filterp'."
  `((set) #'union nil ,filterp))

(gmap:def-gmap-res-type intersection (&key filterp)
  "Returns the intersection of the values, optionally filtered by `filterp'."
  `((complement (set)) #'intersection nil ,filterp))

(defmethod make-load-form ((s wb-set) &optional environment)
  (declare (ignore environment))
  `(convert 'wb-set ',(convert 'list s)))

(defmethod make-load-form ((s wb-custom-set) &optional environment)
  (declare (ignore environment))
  (let ((comp (wb-custom-set-org s)))
    `(convert 'wb-set ',(convert 'list s) :compare-fn-name ',(tree-set-org-compare-fn-name comp))))


;;; ================================================================================
;;; CHAMP sets

(defstruct (hash-set-org
	     (:constructor make-hash-set-org (compare-fn-name compare-fn hash-fn))
	     (:predicate hash-set-type?)
	     (:copier nil))
  (compare-fn-name nil :type symbol :read-only t)
  (compare-fn nil :type function :read-only t)
  (hash-fn nil :type function :read-only t))

(declaim (inline make-ch-set))

(defstruct (ch-set
	     (:include set)
	     (:constructor make-ch-set (contents org))
	     (:predicate ch-set?)
	     (:print-function print-ch-set)
	     (:copier nil))
  (contents nil :read-only t)
  (org nil :type hash-set-org :read-only t))

(defparameter *empty-ch-set* (make-ch-set nil (make-hash-set-org 'compare #'compare #'hash-value)))

(declaim (inline empty-ch-set))
(defun empty-ch-set (&optional compare-fn-name)
  "Returns an empty ch-set.  By default, it will use `fset:compare' to compare
values, and `fset:hash-value' to hash them; to use a custom comparison and hash,
use `define-hash-function' to associate them, and then supply the name of the
comparison function as `compare-fn-name'."
  (if (null compare-fn-name)
      *empty-ch-set*
    (empty-ch-custom-set compare-fn-name)))

(deflex +empty-ch-custom-set-cache+ (make-hash-table :test 'equal))

(defun empty-ch-custom-set (compare-fn-name)
  (assert (and (symbolp compare-fn-name) (not (null compare-fn-name))))
  (if (eq compare-fn-name 'compare)
      *empty-ch-set*
    (let ((prev-instance (gethash compare-fn-name +empty-ch-custom-set-cache+))
	  (compare-fn (symbol-function compare-fn-name))
	  (hash-fn-name (or (get compare-fn-name 'hash-function)
			    (error "compare-fn-name `~S' not defined for hashing -- see `define-hash-function'"
				   compare-fn-name)))
	  ((hash-fn (symbol-function hash-fn-name))))
      (if (and prev-instance
	       (let ((prev-comp (ch-set-org prev-instance)))
		 (and (eq compare-fn (hash-set-org-compare-fn prev-comp))
		      (eq hash-fn (hash-set-org-hash-fn prev-comp)))))
	  prev-instance
	(setf (gethash compare-fn-name +empty-ch-custom-set-cache+)
	      (make-ch-set nil (make-hash-set-org compare-fn-name compare-fn hash-fn)))))))

(defmethod empty-instance-function ((class-name (eql 'ch-set)))
  'empty-ch-set)

(defmethod empty-set-like ((s ch-set))
  (empty-ch-set))

(defmethod compare-fn ((s ch-set))
  (hash-set-org-compare-fn (ch-set-org s)))

(defmethod compare-fn-name ((s ch-set))
  (hash-set-org-compare-fn-name (ch-set-org s)))

(defmethod empty? ((s ch-set))
  (null (ch-set-contents s)))

(defmethod size ((s ch-set))
  (ch-set-tree-size (ch-set-contents s)))

(defmethod set-size ((s ch-set))
  (ch-set-tree-size (ch-set-contents s)))

(defmethod arb ((s ch-set))
  (let ((tree (ch-set-contents s)))
    (if tree (values (ch-set-tree-arb tree) t)
      (values nil nil))))

(defmethod contains? ((s ch-set) x &optional (y nil y?))
  (declare (ignore y))
  (check-two-arguments y? 'contains? 'ch-set)
  (let ((hsorg (ch-set-org s)))
    (ch-set-tree-contains? (ch-set-contents s) x (hash-set-org-hash-fn hsorg)
			   (hash-set-org-compare-fn hsorg))))

(defmethod with ((s ch-set) value &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'with 'ch-set)
  (let ((contents (ch-set-contents s))
	(hsorg (ch-set-org s))
	((new-contents (ch-set-tree-with contents value (hash-set-org-hash-fn hsorg)
					 (hash-set-org-compare-fn hsorg)))))
    (if (eq new-contents contents)
	s
      (make-ch-set new-contents hsorg))))

(defmethod less ((s ch-set) value &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'ch-set)
  (let ((contents (ch-set-contents s))
	(hsorg (ch-set-org s))
	((new-contents (ch-set-tree-less contents value (hash-set-org-hash-fn hsorg)
					 (hash-set-org-compare-fn hsorg)))))
    (if (eq new-contents contents)
	s
      (make-ch-set new-contents hsorg))))

(defmethod union ((s1 ch-set) (s2 ch-set) &key)
  (if-same-ch-set-orgs (s1 s2 hsorg)
      (make-ch-set (ch-set-tree-union (ch-set-contents s1) (ch-set-contents s2)
				      (hash-set-org-hash-fn hsorg) (hash-set-org-compare-fn hsorg))
		   hsorg)
    (call-next-method)))

(defmethod intersection ((s1 ch-set) (s2 ch-set) &key)
  (if-same-ch-set-orgs (s1 s2 hsorg)
      (make-ch-set (ch-set-tree-intersection (ch-set-contents s1) (ch-set-contents s2)
					     (hash-set-org-hash-fn hsorg)
					     (hash-set-org-compare-fn hsorg))
		   hsorg)
    (call-next-method)))

(defmethod disjoint? ((s1 ch-set) (s2 ch-set))
  (if-same-ch-set-orgs (s1 s2 hsorg)
      (ch-set-tree-disjoint? (ch-set-contents s1) (ch-set-contents s2)
			     (hash-set-org-hash-fn hsorg) (hash-set-org-compare-fn hsorg))
    (call-next-method)))

(defmethod filter ((pred function) (s ch-set))
  (ch-set-filter pred s))

(defmethod filter ((pred symbol) (s ch-set))
  (ch-set-filter (coerce-to-function pred) s))

(defmethod filter ((pred map) (s ch-set))
  (ch-set-filter #'(lambda (x) (lookup pred x)) s))

(defun ch-set-filter (pred s)
  (declare (optimize (speed 3) (safety 0))
	   (type function pred))
  (let ((result nil)
	(hsorg (ch-set-org s)))
    (do-ch-set-tree-members (x (ch-set-contents s))
      (when (funcall pred x)
	(setq result (ch-set-tree-with result x (hash-set-org-hash-fn hsorg)
				       (hash-set-org-compare-fn hsorg)))))
    (make-ch-set result hsorg)))

(defmethod image ((fn function) (s ch-set))
  (ch-set-image fn s))

(defmethod image ((fn symbol) (s ch-set))
  (ch-set-image (coerce-to-function fn) s))

(defmethod image ((fn map) (s ch-set))
  (ch-set-image fn s))

(defmethod image ((fn set) (s ch-set))
  (ch-set-image fn s))

(defmethod image ((fn bag) (s ch-set))
  (ch-set-image fn s))

(defun ch-set-image (fn s)
  (let ((result nil)
	(hsorg (ch-set-org s)))
    (do-ch-set-tree-members (x (ch-set-contents s))
      (setq result (ch-set-tree-with result (@ fn x) (hash-set-org-hash-fn hsorg)
				     (hash-set-org-compare-fn hsorg))))
    (make-ch-set result hsorg)))

;;; Analogous to `at-rank' on a WB-Set, but I didn't want to make this a method of that, because
;;; the ordering, though deterministic, is not one that will make any sense to a client.
;;; &&& Needs `defgeneric'
(defmethod at-index ((s ch-set) index)
  (let ((contents (ch-set-contents s))
	((size (ch-set-tree-size contents))))
    (unless (and (>= index 0) (< index size))
      (error 'simple-type-error :datum index :expected-type `(integer 0 (,size))
	     :format-control "Index ~D out of bounds on ~A"
				:format-arguments (list index s)))
    (ch-set-tree-index-element contents index)))

(defmethod compare ((s1 ch-set) (s2 ch-set))
  (if-same-ch-set-orgs (s1 s2 hsorg)
      (ch-set-tree-compare (ch-set-contents s1) (ch-set-contents s2)
			   (hash-set-org-compare-fn hsorg))
    (call-next-method)))

(defmethod internal-do-set ((s ch-set) elt-fn value-fn)
  (declare ;(optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  (do-ch-set-tree-members (x (ch-set-contents s) (funcall value-fn))
    (funcall elt-fn x)))

(defmethod iterator ((s ch-set) &key)
  ;; O(log n) per element.   &&& Write a proper tree iterator.
  (let ((tree (ch-set-contents s))
	(idx 0))
    (lambda (op)
      (ecase op
	(:get (ch-set-tree-index-element tree (postincf idx)))
	(:done? (>= idx (ch-set-tree-size tree)))
	(:more? (< idx (ch-set-tree-size tree)))))))

(defmethod convert ((to-type (eql 'ch-set)) (s set) &key compare-fn-name)
  (convert-to-ch-set s nil
    (let ((tree nil))
      (do-set (x s tree)
	(setq tree (ch-set-tree-with tree x hash-fn compare-fn))))))

(defmethod convert ((to-type (eql 'ch-set)) (s ch-set) &key compare-fn-name)
  (convert-to-ch-set s (let ((from-hsorg (ch-set-org s)))
			 (or (eq from-hsorg hsorg)
			     (and (eq (hash-set-org-hash-fn from-hsorg) hash-fn)
				  (eq (hash-set-org-compare-fn from-hsorg) compare-fn))))
    (let ((tree nil))
      (do-ch-set-tree-members (x (ch-set-contents s) tree)
	(setq tree (ch-set-tree-with tree x hash-fn compare-fn))))))

(defmethod convert ((to-type (eql 'ch-set)) (l list) &key compare-fn-name)
  (convert-to-ch-set l nil
    (let ((tree nil))
      (dolist (x l tree)
	(setq tree (ch-set-tree-with tree x hash-fn compare-fn))))))

(defmethod convert ((to-type (eql 'ch-set)) (s seq) &key compare-fn-name)
  (convert-to-ch-set s nil
    (let ((tree nil))
      (do-seq (x s :value tree)
	(setq tree (ch-set-tree-with tree x hash-fn compare-fn))))))

(defmethod convert ((to-type (eql 'ch-set)) (s sequence) &key compare-fn-name)
  (convert-to-ch-set s nil
    (let ((tree nil))
      (dotimes (i (length s) tree)
	(setq tree (ch-set-tree-with tree (elt s i) hash-fn compare-fn))))))

(defun print-ch-set (set stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "##{"
				    :suffix (let ((hsorg (ch-set-org set))
						  ((compare-fn-name (hash-set-org-compare-fn-name hsorg))))
					      (if (eq compare-fn-name 'compare) " }"
						(format nil " }[~S]" compare-fn-name))))
    (do-set (x set)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (write x :stream stream))))

(defmethod make-load-form ((s ch-set) &optional environment)
  (declare (ignore environment))
  `(convert 'ch-set ',(convert 'list s) :compare-fn-name ',(hash-set-org-compare-fn-name (ch-set-org s))))


;;; ================================================================================
;;; Bags

(declaim (inline make-wb-bag))

(defstruct (wb-bag
	     (:include bag)
	     (:constructor make-wb-bag (contents org))
	     (:predicate wb-bag?)
	     (:print-function print-wb-bag)
	     (:copier nil))
  "A class of functional bags (multisets) represented as weight-balanced binary
trees.  This is the default implementation of bags in FSet."
  (contents nil :read-only t)
  (org nil :type tree-set-org :read-only t))

(defparameter *empty-wb-bag* (make-wb-bag nil +fset-default-tree-set-org+))

(declaim (inline empty-bag))
(defun empty-bag ()
  "Returns an empty bag of the default implementation and type."
  *empty-wb-bag*)

(defmethod empty-instance-function ((class-name (eql 'bag)))
  'empty-bag)

(declaim (inline empty-wb-bag))
(defun empty-wb-bag (&optional compare-fn-name)
  "Returns an empty `wb-bag' ordered according to `compare-fn-name', which
must be a symbol."
  (if (null compare-fn-name)
      *empty-wb-bag*
    (empty-wb-custom-bag compare-fn-name)))

(deflex +empty-wb-custom-bag-cache+ (make-hash-table :test 'equal))

(defun empty-wb-custom-bag (compare-fn-name)
  (assert (and (symbolp compare-fn-name) (not (null compare-fn-name))))
  (if (eq compare-fn-name 'compare)
      *empty-wb-bag*
    (let ((prev-instance (gethash compare-fn-name +empty-wb-custom-bag-cache+))
	  (compare-fn (symbol-function compare-fn-name)))
      (if (and prev-instance
	       (eq compare-fn (tree-set-org-compare-fn (wb-bag-org prev-instance))))
	  prev-instance
	(setf (gethash compare-fn-name +empty-wb-custom-bag-cache+)
	      (make-wb-bag nil (make-tree-set-org compare-fn-name compare-fn)))))))

(defmethod empty-instance-function ((class-name (eql 'wb-bag)))
  'empty-wb-bag)

(defmethod empty-set-like ((b wb-bag))
  (let ((tsorg (wb-bag-org b)))
    (empty-wb-custom-set (tree-set-org-compare-fn-name tsorg))))

(defmethod empty-bag-like ((b wb-bag))
  (let ((tsorg (wb-bag-org b)))
    (empty-wb-custom-bag (tree-set-org-compare-fn-name tsorg))))

;;; Needs to be down here for inlining reasons.
(defmethod empty-bag-like ((s wb-set))
  (let ((tsorg (wb-custom-set-org s)))
    (empty-wb-bag (tree-set-org-compare-fn-name tsorg))))

(defmethod compare-fn ((b wb-bag))
  (tree-set-org-compare-fn (wb-bag-org b)))

(defmethod compare-fn-name ((b wb-bag))
  (tree-set-org-compare-fn-name (wb-bag-org b)))

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
  (plusp (WB-Bag-Tree-Multiplicity (wb-bag-contents b) x (tree-set-org-compare-fn (wb-bag-org b)))))

(defmethod lookup ((b wb-bag) x)
  (let ((mult value-found (WB-Bag-Tree-Multiplicity (wb-bag-contents b) x
						    (tree-set-org-compare-fn (wb-bag-org b)))))
    (if (plusp mult)
	(values t value-found)
      (values nil nil))))

(defmethod rank ((b wb-bag) x)
  (let ((found? rank (WB-Bag-Tree-Rank (wb-bag-contents b) x (tree-set-org-compare-fn (wb-bag-org b)))))
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

;;; Alas, `count' is taken.
(defmethod multiplicity ((b wb-bag) x)
  (WB-Bag-Tree-Multiplicity (wb-bag-contents b) x (tree-set-org-compare-fn (wb-bag-org b))))

(defmethod multiplicity ((s set) x)
  (if (contains? s x) 1 0))

(defmethod with ((b wb-bag) value &optional (multiplicity 1))
  (assert (and (integerp multiplicity) (not (minusp multiplicity))))
  (if (zerop multiplicity) b
    (make-wb-bag (WB-Bag-Tree-With (wb-bag-contents b) value (tree-set-org-compare-fn (wb-bag-org b))
				   multiplicity)
		 (wb-bag-org b))))

(defmethod less ((b wb-bag) value &optional (multiplicity 1))
  (assert (and (integerp multiplicity) (not (minusp multiplicity))))
  (if (zerop multiplicity) b
    (make-wb-bag (WB-Bag-Tree-Less (wb-bag-contents b) value (tree-set-org-compare-fn (wb-bag-org b))
				   multiplicity)
		 (wb-bag-org b))))

(defmethod union ((b1 bag) (b2 bag) &key)
  "Fallback method for mixed implementations."
  (let ((result b1))
    (do-bag-pairs (x n2 b2)
      (let ((n1 (multiplicity result x)))
	(when (< n1 n2)
	  (includef result x (- n2 n1)))))
    result))

(defmethod union ((b1 wb-bag) (b2 wb-bag) &key)
  (if-same-wb-bag-orgs (b1 b2 tsorg)
      (make-wb-bag (WB-Bag-Tree-Union (wb-bag-contents b1) (wb-bag-contents b2) (tree-set-org-compare-fn tsorg))
		   tsorg)
    (call-next-method)))

(defmethod union ((b bag) (s set) &key)
  "Fallback method for mixed implementations."
  (let ((result b))
    (do-set (x s)
      (unless (contains? result x)
	(setq result (with result x))))
    result))

(defmethod union ((b wb-bag) (s wb-set) &key)
  (let ((scmp (wb-set-compare-fn s))
	(bcmp (tree-set-org-compare-fn (wb-bag-org b))))
    (if (eq scmp bcmp)
	(make-wb-bag (WB-Bag-Tree-Union (WB-Set-Tree-To-Bag-Tree (wb-set-contents s))
					(wb-bag-contents b) bcmp)
		     (wb-bag-org b))
      (call-next-method))))

(defmethod union ((s set) (b bag) &key)
  "Fallback method for mixed implementations."
  (let ((result (convert 'bag s)))
    (do-bag-pairs (x n b)
      (setq result (with result x (- n (if (contains? s x) 1 0)))))
    result))

(defmethod union ((s wb-set) (b wb-bag) &key)
  (let ((scmp (wb-set-compare-fn s))
	(bcmp (tree-set-org-compare-fn (wb-bag-org b))))
    (if (eq scmp bcmp)
	(make-wb-bag (WB-Bag-Tree-Union (WB-Set-Tree-To-Bag-Tree (wb-set-contents s))
					(wb-bag-contents b) bcmp)
		     (wb-set-org s))
      (call-next-method))))

(defmethod bag-sum ((b1 bag) (b2 bag))
  "Fallback method for mixed implementations."
  (let ((result b1))
    (do-bag-pairs (x n b2)
      (includef result x n))
    result))

(defmethod bag-sum ((b1 wb-bag) (b2 wb-bag))
  (if-same-wb-bag-orgs (b1 b2 tsorg)
      (make-wb-bag (WB-Bag-Tree-Sum (wb-bag-contents b1) (wb-bag-contents b2) (tree-set-org-compare-fn tsorg))
		   tsorg)
    (call-next-method)))

(defmethod bag-sum ((b bag) (s set))
  "Fallback method for mixed implementations."
  (let ((result b))
    (do-set (x s)
      (setq result (with result x)))
    result))

(defmethod bag-sum ((b wb-bag) (s wb-set))
  (let ((scmp (wb-set-compare-fn s))
	(bcmp (tree-set-org-compare-fn (wb-bag-org b))))
    (if (eq scmp bcmp)
	(make-wb-bag (WB-Bag-Tree-Sum (WB-Set-Tree-To-Bag-Tree (wb-set-contents s))
				      (wb-bag-contents b) bcmp)
		     (wb-bag-org b))
      (call-next-method))))

(defmethod bag-sum ((s set) (b bag))
  "Fallback method for mixed implementations."
  (let ((result (convert 'bag s)))
    (do-bag-pairs (x n b)
      (setq result (with result x n)))
    result))

(defmethod bag-sum ((s wb-set) (b wb-bag))
  (let ((scmp (wb-set-compare-fn s))
	(bcmp (tree-set-org-compare-fn (wb-bag-org b))))
    (if (eq scmp bcmp)
	(make-wb-bag (WB-Bag-Tree-Sum (WB-Set-Tree-To-Bag-Tree (wb-set-contents s))
				      (wb-bag-contents b) bcmp)
		     (wb-set-org s))
      (call-next-method))))

(defmethod intersection ((b1 bag) (b2 bag) &key)
  "Fallback method for mixed implementations."
  (let ((result (empty-bag-like b1)))
    (do-bag-pairs (x n2 b2)
      (includef result x (min (multiplicity b1 x) n2)))
    result))

(defmethod intersection ((b1 wb-bag) (b2 wb-bag) &key)
  (if-same-wb-bag-orgs (b1 b2 tsorg)
      (make-wb-bag (WB-Bag-Tree-Intersect (wb-bag-contents b1) (wb-bag-contents b2)
					  (tree-set-org-compare-fn tsorg))
		   tsorg)
    (call-next-method)))

(defmethod intersection ((b bag) (s set) &key)
  "Fallback method for mixed implementations."
  (let ((result (empty-set-like b)))
    (do-bag-pairs (x n b)
      (declare (ignore n))
      (when (contains? s x)
	(includef result x)))
    result))

(defmethod intersection ((b wb-bag) (s wb-set) &key)
  (let ((scmp (wb-set-compare-fn s))
	(bcmp (tree-set-org-compare-fn (wb-bag-org b))))
    (if (eq scmp bcmp)
	(make-wb-set (WB-Set-Tree-Intersect (WB-Bag-Tree-To-Set-Tree (wb-bag-contents b))
					    (wb-set-contents s) bcmp)
		     (wb-bag-org b))
      (call-next-method))))

(defmethod intersection ((s set) (b bag) &key)
  "Fallback method for mixed implementations."
  (let ((result (empty-set-like s)))
    (do-bag-pairs (x n b)
      (declare (ignore n))
      (when (contains? s x)
	(includef result x)))
    result))

(defmethod intersection ((s wb-set) (b wb-bag) &key)
  (let ((scmp (wb-set-compare-fn s))
	(bcmp (tree-set-org-compare-fn (wb-bag-org b))))
    (if (eq scmp bcmp)
	(make-wb-set (WB-Set-Tree-Intersect (WB-Bag-Tree-To-Set-Tree (wb-bag-contents b))
					    (wb-set-contents s) bcmp)
		     (wb-set-org s))
      (call-next-method))))

(defmethod bag-product ((b1 bag) (b2 bag))
  "Fallback method for mixed implementations."
  (let ((result (empty-bag-like b1)))
    (do-bag-pairs (x n2 b2)
      (includef result x (* (multiplicity b1 x) n2)))
    result))

(defmethod bag-product ((b1 wb-bag) (b2 wb-bag))
  (if-same-wb-bag-orgs (b1 b2 tsorg)
      (make-wb-bag (WB-Bag-Tree-Product (wb-bag-contents b1) (wb-bag-contents b2)
					(tree-set-org-compare-fn tsorg))
		   tsorg)
    (call-next-method)))

(defmethod bag-product ((b bag) (s set))
  "Fallback method for mixed implementations."
  (let ((result (empty-bag-like b)))
    (do-bag-pairs (x n b)
      (when (contains? s x)
	(includef result x n)))
    result))

(defmethod bag-product ((b wb-bag) (s wb-set))
  (let ((scmp (wb-set-compare-fn s))
	(bcmp (tree-set-org-compare-fn (wb-bag-org b))))
    (if (eq scmp bcmp)
	(make-wb-bag (WB-Bag-Tree-Product (WB-Set-Tree-To-Bag-Tree (wb-set-contents s))
					  (wb-bag-contents b) bcmp)
		     (wb-bag-org b))
      (call-next-method))))

(defmethod bag-product ((s set) (b bag))
  "Fallback method for mixed implementations."
  (let ((result (empty-bag-like s)))
    (do-bag-pairs (x n b)
      (when (contains? s x)
	(includef result x n)))
    result))

(defmethod bag-product ((s wb-set) (b wb-bag))
  (let ((scmp (wb-set-compare-fn s))
	(bcmp (tree-set-org-compare-fn (wb-bag-org b))))
    (if (eq scmp bcmp)
	(make-wb-bag (WB-Bag-Tree-Product (WB-Set-Tree-To-Bag-Tree (wb-set-contents s))
					  (wb-bag-contents b) bcmp)
		     (wb-set-org s))
      (call-next-method))))

(defmethod bag-difference ((b1 bag) (b2 bag))
  "Fallback method for mixed implementations."
  (let ((result b1))
    (do-bag-pairs (x n2 b2)
      (excludef result x n2))
    result))

(defmethod bag-difference ((b1 wb-bag) (b2 wb-bag))
  (if-same-wb-bag-orgs (b1 b2 tsorg)
      (make-wb-bag (WB-Bag-Tree-Diff (wb-bag-contents b1) (wb-bag-contents b2)
				     (tree-set-org-compare-fn tsorg))
		   tsorg)
    (call-next-method)))

(defmethod bag-difference ((b bag) (s set))
  "Fallback method for mixed implementations."
  (let ((result b))
    (do-set (x s)
      (excludef result x))
    result))

(defmethod bag-difference ((b wb-bag) (s wb-set))
  (let ((scmp (wb-set-compare-fn s))
	(bcmp (tree-set-org-compare-fn (wb-bag-org b))))
    (if (eq scmp bcmp)
	(make-wb-bag (WB-Bag-Tree-Diff (wb-bag-contents b)
				       (WB-Set-Tree-To-Bag-Tree (wb-set-contents s)) bcmp)
		     (wb-bag-org b))
      (call-next-method))))

(defmethod bag-difference ((s set) (b bag))
  "Fallback method for mixed implementations."
  (let ((result (convert 'bag s)))
    (do-bag-pairs (x n b)
      (declare (ignore n))
      (excludef result x))
    result))

(defmethod bag-difference ((s wb-set) (b wb-bag))
  (let ((scmp (wb-set-compare-fn s))
	(bcmp (tree-set-org-compare-fn (wb-bag-org b))))
    (if (eq scmp bcmp)
	(make-wb-bag (WB-Bag-Tree-Diff (WB-Set-Tree-To-Bag-Tree (wb-set-contents s))
				       (wb-bag-contents b) bcmp)
		     (wb-set-org s))
      (call-next-method))))

(defmethod subbag? ((b1 bag) (b2 bag))
  "Fallback method for mixed implementations."
  (do-bag-pairs (x n1 b1 t)
    (unless (<= n1 (multiplicity b2 x))
      (return nil))))

(defmethod subbag? ((b1 wb-bag) (b2 wb-bag))
  (if-same-wb-bag-orgs (b2 b2 tsorg)
      (WB-Bag-Tree-Subbag? (wb-bag-contents b1) (wb-bag-contents b2) (tree-set-org-compare-fn tsorg))
    (call-next-method)))

(defmethod subbag? ((s set) (b bag))
  (do-set (x s t)
    (unless (contains? b x)
      (return nil))))

(defmethod subbag? ((b bag) (s set))
  (do-bag-pairs (x n b t)
    (unless (and (= n 1) (contains? s x))
      (return nil))))

;;; I had `subbag?' specializations for (`wb-set' `wb-bag') and (`wb-bag' `wb-set') here, but
;;; they were not well implemented, calling `WB-Set-Tree-To-Bag-Tree' first.  Although that's
;;; technically O(n), the constant factor is terrible (not to mention all the consing).

(defun proper-subbag? (sub super)
  "Returns true iff `sub' is a proper subbag of `super', that is, for every
member of `sub', `super' contains the same value with at least the same
multiplicity, but the two bags are not equal."
  (and (subbag? sub super)
       (< (size sub) (size super))))

(defmethod disjoint? ((b1 bag) (b2 bag))
  "Fallback method for mixed implementations."
  (let ((b1 b2 (if (< (size b1) (size b2))
		   (values b2 b1)
		 (values b1 b2))))
    (do-bag-pairs (x n b2 t)
      (declare (ignore n))
      (when (contains? b1 x)
	(return nil)))))

(defmethod disjoint? ((b1 wb-bag) (b2 wb-bag))
  (if-same-wb-bag-orgs (b1 b2 tsorg)
      (WB-Bag-Tree-Disjoint? (wb-bag-contents b1) (wb-bag-contents b2) (tree-set-org-compare-fn tsorg))
    (call-next-method)))

(defmethod disjoint? ((b wb-bag) (s wb-set))
  (bag-set-disjoint? b s))

(defmethod disjoint? ((s wb-set) (b wb-bag))
  (bag-set-disjoint? b s))

(defun bag-set-disjoint? (b s)
  ;; Too lazy to write the WB hedge algorithm.  Maybe l8r.
  (if (< (size s) (set-size b))
      (do-set (x s t)
	(when (contains? b x)
	  (return nil)))
    (do-bag-pairs (x c b t)
      (declare (ignore c))
      (when (contains? s x)
	(return nil)))))

(defmethod compare ((b1 bag) (b2 bag))
  "Fallback method for mixed implementations."
  (let ((size1 (size b1))
	(size2 (size b2))
	(set-size1 (set-size b1))
	(set-size2 (set-size b2)))
    (cond ((< size1 size2) ':less)
	  ((> size1 size2) ':greater)
	  ((< set-size1 set-size2) ':less)
	  ((> set-size1 set-size2) ':greater)
	  ((do-bag-pairs (x n1 b1 t)
	     (unless (= n1 (multiplicity b2 x))
	       (return nil)))
	   ':equal)
	  (t ':unequal))))

(defmethod compare ((b1 wb-bag) (b2 wb-bag))
  (if-same-wb-bag-orgs (b1 b2 tsorg)
      (WB-Bag-Tree-Compare (wb-bag-contents b1) (wb-bag-contents b2) (tree-set-org-compare-fn tsorg))
    (call-next-method)))

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
  (Do-WB-Bag-Tree-Pairs (x n (wb-bag-contents b) (funcall value-fn))
    (funcall elt-fn x n)))

(defmethod iterator ((b wb-bag) &key pairs?)
  (if pairs?
      (Make-WB-Bag-Tree-Pair-Iterator (wb-bag-contents b))
    (Make-WB-Bag-Tree-Iterator (wb-bag-contents b))))

(defmethod fun-iterator ((s wb-bag) &key pairs? from-end?)
  (if pairs?
      (if from-end?
	  (WB-Bag-Tree-Pair-Rev-Fun-Iter (wb-bag-contents s))
	(WB-Bag-Tree-Pair-Fun-Iter (wb-bag-contents s)))
    (if from-end?
	(WB-Bag-Tree-Rev-Fun-Iter (wb-bag-contents s))
      (WB-Bag-Tree-Fun-Iter (wb-bag-contents s)))))

(defmethod filter ((pred function) (b bag))
  (bag-filter pred b))

(defmethod filter ((pred symbol) (b bag))
  (bag-filter (coerce-to-function pred) b))

(defmethod filter ((pred map) (b bag))
  (bag-filter pred b))

(defun bag-filter (pred b)
  (let ((result (empty-bag-like b)))
    (do-bag-pairs (x n b)
      (when (@ pred x)
	(setq result (with result x n))))
    result))

(defmethod filter ((pred set) (b bag))
  (bag-product (convert 'bag pred) b))

(defmethod filter ((pred bag) (b bag))
  (bag-filter pred b))

(defmethod filter-pairs ((pred function) (b bag))
  (bag-filter-pairs pred b))

(defmethod filter-pairs ((pred symbol) (b bag))
  (bag-filter-pairs (coerce-to-function pred) b))

(defun bag-filter-pairs (pred b)
  (let ((result (empty-bag-like b)))
    (do-bag-pairs (x n b)
      (when (funcall pred x n)
	(setq result (with result x n))))
    result))

(defmethod image ((fn function) (b bag))
  (bag-image fn b))

(defmethod image ((fn symbol) (b bag))
  (bag-image (coerce-to-function fn) b))

(defmethod image ((fn map) (b bag))
  (bag-image fn b))

(defmethod image ((fn set) (b bag))
  (bag-image fn b))

(defmethod image ((fn bag) (b bag))
  (bag-image fn b))

(defun bag-image (fn b)
  (let ((result (empty-bag-like b)))
    (do-bag-pairs (x n b)
      (setq result (with result (@ fn x) n)))
    result))

(defmethod reduce ((fn function) (b bag) &key key (initial-value nil init?))
  (bag-reduce fn b initial-value (and key (coerce-to-function key)) init?))

(defmethod reduce ((fn symbol) (b bag) &key key (initial-value nil init?))
  (bag-reduce (coerce-to-function fn) b initial-value (and key (coerce-to-function key))
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

(defmethod convert ((to-type (eql 'wb-bag)) (b bag) &key compare-fn-name)
  (convert-to-wb-bag b nil
    (let ((tree nil))
      (do-bag-pairs (x n b tree)
	(setq tree (WB-Bag-Tree-With tree x compare-fn n))))))

(defmethod convert ((to-type (eql 'wb-bag)) (b wb-bag) &key compare-fn-name)
  (convert-to-wb-bag b (eq compare-fn (tree-set-org-compare-fn (wb-bag-org b)))
    (let ((tree nil))
      (do-wb-bag-tree-pairs (x n (wb-bag-contents b) tree)
	(setq tree (WB-Bag-Tree-With tree x compare-fn n))))))

(defmethod convert ((to-type (eql 'set)) (b wb-bag) &key)
  (make-wb-set (WB-Bag-Tree-To-Set-Tree (wb-bag-contents b)) (wb-bag-org b)))

(defmethod convert ((to-type (eql 'wb-set)) (b wb-bag) &key compare-fn-name)
  (convert-to-wb-set b nil
    (if (eq compare-fn (tree-set-org-compare-fn (wb-bag-org b)))
	(WB-Bag-Tree-To-Set-Tree (wb-bag-contents b))
      (let ((result nil))
	(do-wb-bag-tree-pairs (x n (wb-bag-contents b) result)
	  (declare (ignore n))
	  (setq result (WB-Set-Tree-With result x compare-fn)))))))

(defmethod convert ((to-type (eql 'bag)) (s wb-set) &key)
  (make-wb-bag (WB-Set-Tree-To-Bag-Tree (wb-set-contents s)) (wb-set-org s)))

(defmethod convert ((to-type (eql 'wb-bag)) (s wb-set) &key compare-fn-name)
  (convert-to-wb-bag s nil
    (if (eq compare-fn (wb-set-compare-fn s))
	(WB-Set-Tree-To-Bag-Tree (wb-set-contents s))
      (let ((result nil))
	(do-wb-set-tree-members (x (wb-set-contents s) result)
	  (setq result (WB-Bag-Tree-With result x compare-fn)))))))

(defmethod convert ((to-type (eql 'bag)) (s set) &key)
  (wb-bag-from-set s))
(defmethod convert ((to-type (eql 'wb-bag)) (s set) &key compare-fn-name)
  (wb-bag-from-set s compare-fn-name))
(defun wb-bag-from-set (s &optional compare-fn-name)
  (convert-to-wb-bag s nil
    (let ((tree nil))
      (do-set (x s tree)
	(setq tree (WB-Bag-Tree-With tree x compare-fn))))))

(defmethod convert ((to-type (eql 'list)) (b bag) &key pairs?)
  (declare (optimize (speed 3) (safety 0)))
  (if pairs?
      (bag-to-alist b)
    (let ((result nil))
      (do-bag (value b)
	(push value result))
      (nreverse result))))

(defmethod convert ((to-type (eql 'vector)) (b bag) &key pairs?)
  (coerce (convert 'list b :pairs? pairs?) 'vector))

(defmethod convert ((to-type (eql 'alist)) (b bag) &key)
  (bag-to-alist b))
(defun bag-to-alist (b)
  (declare (optimize (speed 3) (safety 0)))
  (let ((result nil))
    (do-bag-pairs (value count b)
      (push (cons value count) result))
    (nreverse result)))

(defmethod convert ((to-type (eql 'bag)) (l list) &key input-sorted? pairs? from-type)
  (wb-bag-from-list l input-sorted? (or pairs? (eq from-type 'alist))))

(defmethod convert ((to-type (eql 'wb-bag)) (l list) &key input-sorted? pairs? from-type compare-fn-name)
  (wb-bag-from-list l input-sorted? (or pairs? (eq from-type 'alist)) compare-fn-name))

(defun wb-bag-from-list (l input-sorted? pairs? &optional compare-fn-name)
  (convert-to-wb-bag l nil
    (if input-sorted?
	(WB-Bag-Tree-From-Sorted-Iterable (the function (iterator l)) (length l) pairs? compare-fn)
      (WB-Bag-Tree-From-List l pairs? compare-fn))))

(defmethod convert ((to-type (eql 'bag)) (s seq) &key input-sorted? pairs?)
  (wb-bag-from-sequence s input-sorted? pairs?))

(defmethod convert ((to-type (eql 'wb-bag)) (s seq) &key input-sorted? pairs? compare-fn-name)
  (wb-bag-from-sequence s input-sorted? pairs? compare-fn-name))

(defmethod convert ((to-type (eql 'bag)) (s sequence) &key input-sorted? pairs?)
  (wb-bag-from-sequence s input-sorted? pairs?))

(defmethod convert ((to-type (eql 'wb-bag)) (s sequence) &key input-sorted? pairs? compare-fn-name)
  (wb-bag-from-sequence s input-sorted? pairs? compare-fn-name))

(defun wb-bag-from-sequence (s input-sorted? pairs? &optional compare-fn-name)
  (convert-to-wb-bag s nil
    (if input-sorted?
	(WB-Bag-Tree-From-Sorted-Iterable (the function (iterator s)) (size s) pairs? compare-fn)
      (WB-Bag-Tree-From-Iterable (the function (iterator s)) pairs? compare-fn))))

(defmethod find (item (b bag) &key key test)
  (declare (optimize (speed 3) (safety 0)))
  (let ((test (coerce-to-function-or-equal? test)))
    (if key
        (let ((key (coerce-to-function key)))
          (do-bag-pairs (x n b nil)
            (declare (ignore n))
            (when (funcall test item (funcall key x))
              (return x))))
        (if (not (eq test #'equal?))
            (do-bag-pairs (x n b nil)
              (declare (ignore n))
              (when (funcall test item x)
                (return x)))
            (nth-value 1 (lookup b item))))))

(defmethod find-if (pred (b bag) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce-to-function pred)))
    (if key
	(let ((key (coerce-to-function key)))
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
  (let ((pred (coerce-to-function pred)))
    (find-if #'(lambda (x) (not (funcall pred x))) b :key key)))

(defmethod count (item (b bag) &key key test)
  (declare (optimize (speed 3) (safety 0)))
  (let ((test default? (coerce-to-function-or-equal? test)))
    (if key
	(let ((key (coerce-to-function key))
              (total 0))
          (do-bag-pairs (x n b total)
            (when (funcall test item (funcall key x))
              (setq total (gen + total n)))))
        (if default?
            (multiplicity b item)
            (let ((total 0))
              (do-bag-pairs (x n b total)
                (when (funcall test item x)
                  (setq total (gen + total n)))))))))

(defmethod count-if (pred (b bag) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce-to-function pred))
	(total 0))
    (if key
	(let ((key (coerce-to-function key)))
	  (do-bag-pairs (x n b total)
	    (when (funcall pred (funcall key x))
	      (setq total (gen + total n)))))
      (do-bag-pairs (x n b total)
	(when (funcall pred x)
	  (setq total (gen + total n)))))))

(defmethod count-if-not (pred (s bag) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce-to-function pred)))
    (count-if #'(lambda (x) (not (funcall pred x))) s :key key)))

(defun print-wb-bag (bag stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "#{%"
				    :suffix (let ((tsorg (wb-bag-org bag)))
					      (if (eq (tree-set-org-compare-fn-name tsorg) 'compare) " %}"
						(format nil " %}[~S]" (tree-set-org-compare-fn-name tsorg)))))
    (let ((i 0))
      (do-bag-pairs (x n bag)
        (pprint-pop)
        (write-char #\Space stream)
        (pprint-newline :linear stream)
        (incf i)
        (if (> n 1)
	    ;; There might be a bag entry for 'quote or 'function...
	    (let (#+sbcl (sb-pretty:*pprint-quote-with-syntactic-sugar* nil))
	      (write (if *print-readably* `(% ,x ,n) `(,x ,n)) :stream stream))
          (write x :stream stream))))))

(gmap:def-gmap-arg-type bag (bag)
  "Yields each element of `bag', as many times as its multiplicity."
  `((the function (iterator ,bag))
    #'(lambda (it) (funcall it ':done?))
    #'(lambda (it) (funcall it ':get))))

(gmap:def-gmap-arg-type bag-pairs (bag)
  "Yields each element of `bag' and its multiplicity as two values."
  `((the function (iterator ,bag :pairs? t))
    #'(lambda (it) (funcall it ':done?))
    (:values 2 #'(lambda (it) (funcall it ':get)))))

(gmap:def-gmap-arg-type wb-bag (bag)
  "Yields each element of `bag', as many times as its multiplicity."
  `((Make-WB-Bag-Tree-Iterator-Internal (wb-bag-contents ,bag))
    #'WB-Bag-Tree-Iterator-Done?
    #'WB-Bag-Tree-Iterator-Get))

(gmap:def-gmap-arg-type wb-bag-pairs (bag)
  "Yields each element of `bag' and its multiplicity as two values."
  `((Make-WB-Bag-Tree-Pair-Iterator-Internal (wb-bag-contents ,bag))
    #'WB-Bag-Tree-Pair-Iterator-Done?
    (:values 2 #'WB-Bag-Tree-Pair-Iterator-Get)))

(gmap:def-arg-type fun-bag-pairs (bag &key from-end?)
  `((fun-iterator ,bag :pairs? t :from-end? ,from-end?)
    #'(lambda (it) (funcall it ':empty?))
    (:values 2 #'(lambda (it) (funcall it ':first)))
    #'(lambda (it) (funcall it ':rest))))

(gmap:def-gmap-res-type bag (&key filterp)
  "Returns a bag of the values, optionally filtered by `filterp'."
  `(nil
    (fn (b x) (WB-Bag-Tree-With b x #'compare))
    #'(lambda (b) (make-wb-bag b +fset-default-tree-set-org+))
    ,filterp))

(gmap:def-gmap-res-type bag-pairs (&key filterp)
  "Consumes two values from the mapped function; returns a bag of the pairs.
Note that `filterp', if supplied, must take two arguments."
  `(nil
    (:consume 2 (fn (b x n) (WB-Bag-Tree-With b x #'compare n)))
    #'(lambda (b) (make-wb-bag b +fset-default-tree-set-org+))
    ,filterp))

(gmap:def-gmap-res-type wb-bag (&key filterp compare-fn-name)
  "Returns a wb-bag of the values, optionally filtered by `filterp'.  To use a
non-default comparison function in the result, supply `compare-fn-name`."
  (let ((proto-var (gensymx #:prototype-))
	(cf-var (gensymx #:cmp-)))
    `(nil #'(lambda (s x) (WB-Bag-Tree-With s x ,cf-var))
	  #'(lambda (s) (make-wb-bag s (wb-bag-org ,proto-var)))
	  ,filterp
	  ((,proto-var (empty-wb-bag ,compare-fn-name))
	   ((,cf-var (tree-set-org-compare-fn (wb-bag-org ,proto-var))))))))

(gmap:def-gmap-res-type wb-bag-pairs (&key filterp compare-fn-name)
  "Consumes two values from the mapped function; returns a wb-bag of the pairs.
Note that `filterp', if supplied, must take two arguments.  To use a non-default
comparison function in the result, supply `compare-fn-name`."
  (let ((proto-var (gensymx #:prototype-))
	(cf-var (gensymx #:cmp-)))
    `(nil (:consume 2 #'(lambda (tree x n) (WB-Bag-Tree-With tree x ,cf-var n)))
	  #'(lambda (tree) (make-wb-bag tree (wb-bag-org ,proto-var)))
	  ,filterp
	  ((,proto-var (empty-wb-bag ,compare-fn-name))
	   ((,cf-var (tree-set-org-compare-fn (wb-bag-org ,proto-var))))))))

(gmap:def-gmap-res-type bag-sum (&key filterp)
  "Returns the bag-sum of the values, optionally filtered by `filterp'."
  `((bag) #'bag-sum nil ,filterp))

(gmap:def-gmap-res-type bag-product (&key filterp)
  "Returns the bag-product of the values, optionally filtered by `filterp'."
  `(nil #'(lambda (prev bag) (if (null prev) bag (bag-product prev bag))) nil ,filterp))

(defmethod make-load-form ((b wb-bag) &optional environment)
  (declare (ignore environment))
  `(convert 'wb-bag ',(convert 'alist b) :from-type 'alist
					 :compare-fn-name ',(tree-set-org-compare-fn-name
							      (wb-bag-org b))))


;;; ================================================================================
;;; Maps

(defstruct (tree-map-org
	     (:constructor make-tree-map-org (key-compare-fn-name key-compare-fn
					      val-compare-fn-name val-compare-fn))
	     (:predicate tree-map-org?)
	     (:copier nil))
  (key-compare-fn-name nil :type symbol :read-only t)
  (key-compare-fn nil :type function :read-only t)
  (val-compare-fn-name nil :type symbol :read-only t)
  (val-compare-fn nil :type function :read-only t))

(deflex +fset-default-tree-map-org+ (make-tree-map-org 'compare #'compare 'compare #'compare))

(declaim (inline make-wb-map))

(defstruct (wb-map
	     (:include map)
	     (:constructor make-wb-map (contents org default))
	     (:predicate wb-map?)
	     (:print-function print-wb-map)
	     (:copier nil))
  "A class of functional maps represented as weight-balanced binary trees.  This is
the default implementation of maps in FSet."
  (contents nil :read-only t)
  (org nil :type tree-map-org :read-only t))

(defparameter *empty-wb-map* (make-wb-map nil +fset-default-tree-map-org+ nil))

(declaim (inline empty-map))
(defun empty-map (&optional default)
  "Returns an empty map of the default implementation."
  (if default (make-wb-map nil +fset-default-tree-map-org+ default)
    *empty-wb-map*))

(defmethod empty-instance-function ((class-name (eql 'map)))
  `empty-map)

(declaim (inline empty-wb-map))
(defun empty-wb-map (&optional default key-compare-fn-name val-compare-fn-name)
  "Returns an empty wb-map."
  (if (and (null key-compare-fn-name) (null val-compare-fn-name))
      (if (null default)
	  *empty-wb-map*
	(make-wb-map nil +fset-default-tree-map-org+ default))
    (empty-wb-custom-map default (or key-compare-fn-name 'compare) (or val-compare-fn-name 'compare))))

(deflex +empty-wb-custom-map-cache+ (make-hash-table :test 'equal))

(defun empty-wb-custom-map (default key-compare-fn-name val-compare-fn-name)
  (assert (and (symbolp key-compare-fn-name) (not (null key-compare-fn-name))))
  (assert (and (symbolp val-compare-fn-name) (not (null val-compare-fn-name))))
  (if (and (eq key-compare-fn-name 'compare) (eq val-compare-fn-name 'compare))
      (if (null default) *empty-wb-map*
	(make-wb-map nil +fset-default-tree-map-org+ default))
    ;; &&& This caches one default per type.  We could use a two-level map to cache multiple defaults,
    ;; but the inner maps would have to be custom wb-maps, whose creation couldn't call this function (!).
    ;; Alternatively, the cached default could always be nil, as it is for the default org.
    (let ((cache-key (list key-compare-fn-name val-compare-fn-name))
	  ((prev-instance (gethash cache-key +empty-wb-custom-map-cache+)))
	  (key-compare-fn (symbol-function key-compare-fn-name))
	  (val-compare-fn (symbol-function val-compare-fn-name)))
      (if (and prev-instance
	       (let ((prev-comp (wb-map-org prev-instance)))
		 (and (eq key-compare-fn (tree-map-org-key-compare-fn prev-comp))
		      (eq val-compare-fn (tree-map-org-val-compare-fn prev-comp))
		      (equal?-cmp default (map-default prev-instance) val-compare-fn))))
	  prev-instance
	(setf (gethash cache-key +empty-wb-custom-map-cache+)
	      (make-wb-map nil (make-tree-map-org key-compare-fn-name key-compare-fn
						  val-compare-fn-name val-compare-fn)
			   default))))))

(defmethod empty-instance-function ((class-name (eql 'wb-map)))
  `empty-wb-map)

(defmethod empty-map-like ((m wb-map))
  (let ((tmorg (wb-map-org m)))
    (empty-wb-custom-map (map-default m) (tree-map-org-key-compare-fn-name tmorg)
			 (tree-map-org-val-compare-fn-name tmorg))))

(defmethod key-compare-fn ((m wb-map))
  (tree-map-org-key-compare-fn (wb-map-org m)))

(defmethod val-compare-fn ((m wb-map))
  (tree-map-org-val-compare-fn (wb-map-org m)))

(defmethod key-compare-fn-name ((m wb-map))
  (tree-map-org-key-compare-fn-name (wb-map-org m)))

(defmethod val-compare-fn-name ((m wb-map))
  (tree-map-org-val-compare-fn-name (wb-map-org m)))

(defmethod default ((m map))
  (map-default m))

(defmethod with-default ((m wb-map) new-default)
  (make-wb-map (wb-map-contents m) (wb-map-org m) new-default))

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
  (let ((comp (wb-map-org m))
	((val? val (WB-Map-Tree-Lookup (wb-map-contents m) x (tree-map-org-key-compare-fn comp)))))
    (and val? (equal?-cmp val y (tree-map-org-val-compare-fn comp)))))

(defmethod lookup ((m wb-map) key)
  (let ((val? val (WB-Map-Tree-Lookup (wb-map-contents m) key
				      (tree-map-org-key-compare-fn (wb-map-org m)))))
    ;; Our internal convention is the reverse of the external one.
    (values (if val? val (map-default m)) val?)))

(defmethod rank ((m wb-map) x)
  (let ((found? rank (WB-Map-Tree-Rank (wb-map-contents m) x
				       (tree-map-org-key-compare-fn (wb-map-org m)))))
    (values (if found? rank (1- rank)) found?)))

(defmethod at-rank ((m wb-map) rank)
  (let ((contents (wb-map-contents m))
	((size (WB-Map-Tree-Size contents))))
    (unless (and (>= rank 0) (< rank size))
      (error 'simple-type-error :datum rank :expected-type `(integer 0 (,size))
	     :format-control "Rank ~D out of bounds on ~A"
	     :format-arguments (list rank m)))
    (WB-Map-Tree-Rank-Pair contents rank)))

(defmethod with ((m wb-map) key &optional (value nil value?))
  (check-three-arguments value? 'with 'wb-map)
  (let ((contents (wb-map-contents m))
	(tmorg (wb-map-org m))
	((new-contents (WB-Map-Tree-With contents key value (tree-map-org-key-compare-fn tmorg)
					 (tree-map-org-val-compare-fn tmorg)))))
    (if (eq new-contents contents)
	m
      (make-wb-map new-contents tmorg (map-default m)))))

(defmethod less ((m wb-map) key &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'wb-map)
  (let ((contents (wb-map-contents m))
	(tmorg (wb-map-org m))
	((new-contents (WB-Map-Tree-Less contents key (tree-map-org-key-compare-fn tmorg)))))
    (if (eq new-contents contents)
	m
      (make-wb-map new-contents tmorg (map-default m)))))

(defmethod domain ((m wb-map))
  (let ((tmorg (wb-map-org m))
	((set-prototype (empty-wb-set (tree-map-org-key-compare-fn-name tmorg)))))
    (make-wb-set (WB-Map-Tree-Domain (wb-map-contents m)) (wb-set-org set-prototype))))

(defmethod compare ((m1 map) (m2 map))
  "Fallback method for mixed implementations."
  (let ((size1 (size m1))
	(size2 (size m2)))
    (cond ((< size1 size2) ':less)
	  ((> size1 size2) ':greater)
	  ((let ((vcf1 (val-compare-fn m1))
		 (vcf2 (val-compare-fn m2)))
	     (do-map (k v1 m1 t)
	       (let ((v2 v2? (lookup m2 k)))
		 (unless (and v2?
			      (let ((eqv1? (equal?-cmp v1 v2 vcf1))
				    (eqv2? (equal?-cmp v1 v2 vcf2)))
				;; Requiring `(eq vcf1 vcf2)' seems a little too strong.  This is our
				;; next best choice.
				(unless (eqv eqv1? eqv2?)
				  (error "Can't compare maps with incompatible val-compare-fns, ~A vs. ~A"
					 (tree-map-org-val-compare-fn-name (wb-map-org m1))
					 (tree-map-org-val-compare-fn-name (wb-map-org m2))))
				eqv1?))
		   (return nil)))))
	   ':equal)
	  (t ':unequal))))

(defun eqv (a b &rest more)
  (and (or (eq a b) (and a b))
       (gmap (:result and) #'eqv (:arg constant a) (:arg list more))))

;;; Prior to FSet 1.4.0, this method ignored the defaults, so two maps with the same
;;; key/value pairs but different defaults compared `:equal'.  While that was clearly
;;; a bug, there is a remote chance that someone has inadvertently depended on this
;;; behavior.
(defmethod compare ((map1 wb-map) (map2 wb-map))
  (if-same-wb-map-orgs (map1 map2 tmorg)
      (let ((val-compare-fn (tree-map-org-val-compare-fn tmorg))
	    ((comp (WB-Map-Tree-Compare (wb-map-contents map1) (wb-map-contents map2)
					(tree-map-org-key-compare-fn tmorg) val-compare-fn))))
	(if (member comp '(:less :greater))
	    comp
	  (let ((def-comp (funcall val-compare-fn (map-default map1) (map-default map2))))
	    (if (member def-comp '(:less :greater))
		def-comp
	      (if (or (eq comp ':unequal) (eq def-comp ':unequal))
		  ':unequal
		':equal)))))
    (call-next-method)))

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

(defmethod fun-iterator ((s wb-map) &key from-end?)
  (if from-end?
      (WB-Map-Tree-Rev-Fun-Iter (wb-map-contents s))
    (WB-Map-Tree-Fun-Iter (wb-map-contents s))))

(defmethod filter ((pred function) (m wb-map))
  (let ((tmorg (wb-map-org m)))
    (make-wb-map (wb-map-filter pred m (tree-map-org-key-compare-fn tmorg) (tree-map-org-val-compare-fn tmorg))
		 tmorg (map-default m))))

(defmethod filter ((pred symbol) (m wb-map))
  (let ((tmorg (wb-map-org m)))
    (make-wb-map (wb-map-filter (coerce-to-function pred) m
				(tree-map-org-key-compare-fn tmorg) (tree-map-org-val-compare-fn tmorg))
		 tmorg (map-default m))))

(defun wb-map-filter (pred m key-compare-fn val-compare-fn)
  (let ((result nil))
    (do-map (x y m)
      (when (funcall pred x y)
	(setq result (WB-Map-Tree-With result x y key-compare-fn val-compare-fn))))
    result))

(defmethod image ((fn function) (m wb-map))
  (let ((tmorg (wb-map-org m)))
    (make-wb-map (wb-map-image fn m (tree-map-org-key-compare-fn tmorg) (tree-map-org-val-compare-fn tmorg))
		 tmorg (map-default m))))

(defmethod image ((fn symbol) (m wb-map))
  (let ((tmorg (wb-map-org m)))
    (make-wb-map (wb-map-image (coerce-to-function fn) m
			       (tree-map-org-key-compare-fn tmorg) (tree-map-org-val-compare-fn tmorg))
		 tmorg (map-default m))))

(defun wb-map-image (fn m key-compare-fn val-compare-fn)
  (declare (type function fn))
  (let ((result nil))
    (do-map (x y m)
      (let ((new-x new-y (funcall fn x y)))
	(setq result (WB-Map-Tree-With result new-x new-y key-compare-fn val-compare-fn))))
    result))

(defmethod reduce ((fn function) (m map) &key key (initial-value nil init?))
  (map-reduce fn m initial-value (and key (coerce-to-function key)) init?))

(defmethod reduce ((fn symbol) (m map) &key key (initial-value nil init?))
  (map-reduce (coerce-to-function fn) m initial-value (and key (coerce-to-function key))
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

(defmethod range ((m wb-map))
  (let ((tmorg (wb-map-org m))
	((set-prototype (empty-wb-set (tree-map-org-val-compare-fn-name tmorg))))
	(result nil))
    (do-map (key val m)
      (declare (ignore key))
      (setq result (WB-Set-Tree-With result val (tree-map-org-val-compare-fn tmorg))))
    (make-wb-set result (wb-set-org set-prototype))))

(defmethod domain-contains? ((m wb-map) x)
  (let ((tmorg (wb-map-org m)))
    (WB-Map-Tree-Lookup (wb-map-contents m) x (tree-map-org-key-compare-fn tmorg))))

(defmethod range-contains? ((m map) x)
  (let ((vcfn (val-compare-fn m)))
    (do-map (k v m)
      (declare (ignore k))
      (when (equal?-cmp v x vcfn)
	(return t)))))

(defmethod map-union ((m1 map) (m2 map) &optional (val-fn (fn (_v1 v2) v2)))
  "Fallback method for mixed implementations."
  (let ((result m1)
	(vcf1 (val-compare-fn m1))
	(vcf2 (val-compare-fn m2)))
    (unless (eq vcf1 vcf2)
      (error "Can't take map-union of maps with different val-compare-fns, ~A vs. ~A"
	     vcf1 vcf2))
    (do-map (k v2 m2)
      (let ((v1 v1? (lookup m1 k)))
	(if (not v1?)
	    (setf (lookup result k) v2)
	  (unless (equal?-cmp v1 v2 vcf1)
	    (let ((new-v second-val (funcall val-fn v1 v2)))
	      (unless (eq second-val ':no-value)
		(setf (lookup result k) new-v)))))))
    result))

(defmethod map-union ((map1 wb-map) (map2 wb-map)
		      &optional (val-fn (fn (_v1 v2) v2)))
  (if-same-wb-map-orgs (map1 map2 tmorg)
      (make-wb-map (WB-Map-Tree-Union (wb-map-contents map1) (wb-map-contents map2)
				      (coerce val-fn 'function) (tree-map-org-key-compare-fn tmorg))
		   tmorg (let ((def1 (map-default map1))
			     (def2 (map-default map2)))
			 (and (or def1 def2) (funcall val-fn def1 def2))))
    (call-next-method)))

(defmethod map-intersection ((m1 map) (m2 map) &optional (val-fn (fn (_v1 v2) v2)))
  "Fallback method for mixed implementations."
  (let ((result (empty-map-like m1))
	(vcf1 (val-compare-fn m1))
	(vcf2 (val-compare-fn m2)))
    (unless (eq vcf1 vcf2)
      (error "Can't take map-intersection of maps with different val-compare-fns, ~A vs. ~A"
	     vcf1 vcf2))
    (do-map (k v1 m1)
      (let ((v2 v2? (lookup m2 k)))
	(when v2?
	  (let ((new-v second-val (funcall val-fn v1 v2)))
	    (unless (eq second-val ':no-value)
	      (setf (lookup result k) new-v))))))
    result))

(defmethod map-intersection ((map1 wb-map) (map2 wb-map)
			     &optional (val-fn (fn (_v1 v2) v2)))
  (if-same-wb-map-orgs (map1 map2 tmorg)
      (make-wb-map (WB-Map-Tree-Intersect (wb-map-contents map1) (wb-map-contents map2)
					  (coerce val-fn 'function) (tree-map-org-key-compare-fn tmorg))
		   tmorg (let ((def1 (map-default map1))
			     (def2 (map-default map2)))
			 (and (or def1 def2) (funcall val-fn def1 def2))))
    (call-next-method)))

(defmethod map-difference-2 ((m1 map) (m2 map))
  "Fallback method for mixed implementations."
  (let ((result1 (empty-map-like m1))
	(result2 (empty-map-like m2))
	(vcf1 (val-compare-fn m1))
	(vcf2 (val-compare-fn m2)))
    (unless (eq vcf1 vcf2)
      (error "Can't take map-difference-2 of maps with different val-compare-fns, ~A vs. ~A"
	     vcf1 vcf2))
    (do-map (k v1 m1)
      (let ((v2 v2? (lookup m2 k)))
	(when (or (not v2?) (not (equal?-cmp v1 v2 vcf1)))
	  (setf (lookup result1 k) v1))))
    (do-map (k v2 m2)
      (let ((v1 v1? (lookup m1 k)))
	(when (or (not v1?) (not (equal?-cmp v1 v2 vcf1)))
	  (setf (lookup result2 k) v2))))
    (values result1 result2)))

(defmethod map-difference-2 ((map1 wb-map) (map2 wb-map))
  (if-same-wb-map-orgs (map1 map2 tmorg)
      (let ((newc1 newc2 (WB-Map-Tree-Diff-2 (wb-map-contents map1) (wb-map-contents map2)
					     (tree-map-org-key-compare-fn tmorg)
					     (tree-map-org-val-compare-fn tmorg))))
	(values (make-wb-map newc1 tmorg (map-default map1))
		(make-wb-map newc2 tmorg (map-default map2))))
    (call-next-method)))

(defmethod restrict ((m map) (s set))
  "Fallback method for mixed implementations."
  (let ((result (empty-map-like m)))
    (do-map (k v m)
      (when (contains? s k)
	(setf (lookup result k) v)))
    result))

(defmethod restrict ((m wb-map) (s wb-set))
  (let ((tmorg (wb-map-org m))
	(tsorg (wb-set-org s)))
    (if (eq (tree-map-org-key-compare-fn tmorg) (tree-set-org-compare-fn tsorg))
	(make-wb-map (WB-Map-Tree-Restrict (wb-map-contents m) (wb-set-contents s)
					   (tree-map-org-key-compare-fn tmorg))
		     tmorg (map-default m))
      (call-next-method))))

(defmethod restrict-not ((m map) (s set))
  "Fallback method for mixed implementations."
  (let ((result (empty-map-like m)))
    (do-map (k v m)
      (unless (contains? s k)
	(setf (lookup result k) v)))
    result))

(defmethod restrict-not ((m wb-map) (s wb-set))
  (let ((tmorg (wb-map-org m))
	(tsorg (wb-set-org s)))
    (if (eq (tree-map-org-key-compare-fn tmorg) (tree-set-org-compare-fn tsorg))
	(make-wb-map (WB-Map-Tree-Restrict-Not (wb-map-contents m) (wb-set-contents s)
					       (tree-map-org-key-compare-fn tmorg))
		     tmorg (map-default m))
      (call-next-method))))

(declaim (inline compose-functions))
(defun compose-functions (f1 f2)
  (lambda (&rest args) (multiple-value-call f2 (apply f1 args))))

(defmethod compose ((f1 function) (f2 function))
  (compose-functions f1 f2))

(defmethod compose ((f1 symbol) (f2 function))
  (compose-functions f1 f2))

(defmethod compose ((f1 function) (f2 symbol))
  (compose-functions f1 f2))

(defmethod compose ((f1 symbol) (f2 symbol))
  (compose-functions f1 f2))

(defmethod compose ((m1 map) (m2 map))
  "Fallback method for mixed implementations."
  (let ((result (empty-map-like m1)))
    (do-map (k v1 m1)
      (setf (lookup result k) (lookup m2 v1)))
    result))

(defmethod compose ((map1 wb-map) (map2 wb-map))
  (let ((tree2 (wb-map-contents map2))
	(kcf2 (tree-map-org-key-compare-fn (wb-map-org map2))))
    (make-wb-map (WB-Map-Tree-Compose (wb-map-contents map1)
				      #'(lambda (x)
					  (let ((val2? val2 (WB-Map-Tree-Lookup tree2 x kcf2)))
					    (if val2? val2 (map-default map2)))))
		 (wb-map-org map1)
		 (let ((new-default? new-default
			 (WB-Map-Tree-Lookup tree2 (map-default map1) kcf2)))
		   (if new-default? new-default (map-default map2))))))

(defmethod compose ((m map) (fn function))
  (map-fn-compose m fn))

(defmethod compose ((m map) (fn symbol))
  (map-fn-compose m (coerce-to-function fn)))

(defmethod compose ((m map) (s seq))
  (map-fn-compose m (fn (x) (lookup s x))))

(defun map-fn-compose (m fn)
  (declare (type function fn))
  (let ((result (empty-map-like m)))
    (do-map (k v m)
      (setf (lookup result k) (funcall fn v)))
    result))

(defmethod compose ((m wb-map) (fn function))
  (wb-map-fn-compose m fn))

(defmethod compose ((m wb-map) (fn symbol))
  (wb-map-fn-compose m (coerce-to-function fn)))

(defmethod compose ((m wb-map) (s seq))
  (wb-map-fn-compose m (fn (x) (lookup s x))))

(defun wb-map-fn-compose (m fn)
  (declare (type function fn))
  (make-wb-map (WB-Map-Tree-Compose (wb-map-contents m) fn) (wb-map-org m)
	       (funcall fn (map-default m))))

(defmethod convert ((to-type (eql 'map)) (m map) &key)
  m)

(defmethod convert ((to-type (eql 'wb-map)) (m wb-map)
		    &key (default nil default?) key-compare-fn-name val-compare-fn-name)
  "The result uses `default' if supplied, otherwise has the same default as `m'."
  (convert-to-wb-map m (if default? default (map-default m))
      (let ((m-tmorg (wb-map-org m)))
	(or (eq m-tmorg tmorg)
	    (and (eq key-compare-fn (tree-map-org-key-compare-fn m-tmorg))
		 (eq val-compare-fn (tree-map-org-val-compare-fn m-tmorg)))))
    (let ((tree nil))
      (Do-WB-Map-Tree-Pairs (k v (wb-map-contents m) tree)
	(setq tree (WB-Map-Tree-With tree k v key-compare-fn val-compare-fn))))))

(defmethod convert ((to-type (eql 'list)) (m map) &key (pair-fn #'cons))
  (let ((result nil))
    (do-map (key val m)
      (push (funcall pair-fn key val) result))
    (nreverse result)))

(defmethod convert ((to-type (eql 'alist)) (m map) &key)
  (let ((result nil))
    (do-map (key val m)
      (push (cons key val) result))
    (nreverse result)))

(defmethod convert ((to-type (eql 'vector)) (m map) &key (pair-fn #'cons))
  (let ((result (make-array (size m)))
	(i 0))
    (do-map (k v m)
      (setf (svref result (postincf i)) (funcall pair-fn k v)))
    result))

(defmethod convert ((to-type (eql 'set)) (m map) &key (pair-fn #'cons))
  (let ((result nil))
    (do-map (key val m)
      (setq result (WB-Set-Tree-With result (funcall pair-fn key val) #'compare)))
    (make-wb-set result)))

;;; &&& Plist support?
(defmethod convert ((to-type (eql 'map)) (l list)
		    &key (key-fn #'car) (value-fn #'cdr) input-sorted?)
  (wb-map-from-sequence l key-fn value-fn input-sorted?))

(defmethod convert ((to-type (eql 'wb-map)) (l list)
		    &key (key-fn #'car) (value-fn #'cdr) input-sorted?
		      key-compare-fn-name val-compare-fn-name)
  (wb-map-from-sequence l key-fn value-fn input-sorted? key-compare-fn-name val-compare-fn-name))

(defmethod convert ((to-type (eql 'map)) (s seq)
		    &key (key-fn #'car) (value-fn #'cdr) input-sorted?)
  (with-default (wb-map-from-sequence s key-fn value-fn input-sorted?)
		(seq-default s)))

(defmethod convert ((to-type (eql 'wb-map)) (s seq)
		    &key (key-fn #'car) (value-fn #'cdr) input-sorted?
		      key-compare-fn-name val-compare-fn-name)
  (with-default (wb-map-from-sequence s key-fn value-fn input-sorted? key-compare-fn-name val-compare-fn-name)
		(seq-default s)))

(defmethod convert ((to-type (eql 'map)) (s sequence)
		    &key (key-fn #'car) (value-fn #'cdr) input-sorted?)
  (wb-map-from-sequence s key-fn value-fn input-sorted?))

(defmethod convert ((to-type (eql 'wb-map)) (s sequence)
		    &key (key-fn #'car) (value-fn #'cdr) input-sorted?
		      key-compare-fn-name val-compare-fn-name)
  (wb-map-from-sequence s key-fn value-fn input-sorted? key-compare-fn-name val-compare-fn-name))

(defun wb-map-from-sequence (s key-fn value-fn input-sorted? &optional key-compare-fn-name val-compare-fn-name)
  (convert-to-wb-map s nil nil
    (let ((key-fn (coerce key-fn 'function))
	  (value-fn (coerce value-fn 'function)))
      (cond (input-sorted?
	     (WB-Map-Tree-From-Sorted-Iterable (iterator s) (size s) key-fn value-fn
					       key-compare-fn val-compare-fn))
	    ((listp s)
	     (WB-Map-Tree-From-List s key-fn value-fn key-compare-fn val-compare-fn))
	    (t
	     (WB-Map-Tree-From-Iterable (iterator s) key-fn value-fn
					key-compare-fn val-compare-fn))))))

(defmethod convert ((to-type (eql 'map)) (b bag) &key)
  (convert 'wb-map b))

(defmethod convert ((to-type (eql 'wb-map)) (b bag) &key key-compare-fn-name val-compare-fn-name)
  ;; &&& If desired, we can easily make a very fast version of this -- all it has to do is
  ;; build new interior nodes, reusing the leaf vectors.  (But only if the compare-fns match.)
  (convert-to-wb-map b nil nil
    (let ((tree nil))
      (do-bag-pairs (x n b tree)
	(setq tree (WB-Map-Tree-With tree x n key-compare-fn val-compare-fn))))))

(defmethod convert ((to-type (eql 'map)) (ht hash-table) &key)
  (convert 'wb-map ht))

(defmethod convert ((to-type (eql 'wb-map)) (ht hash-table) &key key-compare-fn-name val-compare-fn-name)
  (convert-to-wb-map ht nil nil
    (let ((tree nil))
      (maphash (lambda (k v)
		 (setq tree (WB-Map-Tree-With tree k v key-compare-fn val-compare-fn)))
	       ht)
      tree)))

(defmethod convert ((to-type (eql 'hash-table)) (m map)
		    &rest make-hash-table-args &key &allow-other-keys)
  (let ((ht (apply #'make-hash-table make-hash-table-args)))
    (do-map (x y m)
      (setf (gethash x ht) y))
    ht))

(defmethod find (item (m map) &key key test)
  (declare (optimize (speed 3) (safety 0)))
  (let ((test (coerce-to-function-or-equal? test)))
    (if key
        (let ((key (coerce-to-function key)))
          (do-map (x y m nil)
            (when (funcall test item (funcall key x))
              (return (values x y)))))
        (if (not (eq test #'equal?))
            (do-map (x y m nil)
              (when (funcall test item x)
                (return (values x y))))
            (let ((val val? (lookup m item)))
              (if val? (values item val)
                  (values nil nil)))))))

(defmethod find-if (pred (m map) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce-to-function pred)))
    (if key
	(let ((key (coerce-to-function key)))
	  (do-map (x y m nil)
	    (when (funcall pred (funcall key x))
	      (return (values x y)))))
      (do-map (x y m nil)
	(when (funcall pred x)
	  (return (values x y)))))))

(defmethod find-if-not (pred (m map) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce-to-function pred)))
    (find-if #'(lambda (x) (not (funcall pred x))) m :key key)))

(defmethod count (item (m map) &key key test)
  (declare (optimize (speed 3) (safety 0)))
  (let ((test default? (coerce-to-function-or-equal? test)))
    (if key
	(let ((key (coerce-to-function key))
              (total 0))
          (declare (fixnum total))
          (do-map (x y m total)
            (declare (ignore y))
            (when (funcall test item (funcall key x))
              (incf total))))
        (if default?
            (if (lookup m item) 1 0)
            (let ((total 0))
              (declare (fixnum total))
              (do-map (x y m total)
                (declare (ignore y))
                (when (funcall test item x)
                  (incf total))))))))

(defmethod count-if (pred (m map) &key key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce-to-function pred))
	(n 0))
    (declare (fixnum n))
    (if key
	(let ((key (coerce-to-function key)))
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
  (let ((pred (coerce-to-function pred)))
    (count-if #'(lambda (x) (not (funcall pred x))) m :key key)))

(defun print-wb-map (map stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "#{|"
				    :suffix (let ((tmorg (wb-map-org map))
						  ((key-cf-name (tree-map-org-key-compare-fn-name tmorg))
						   (val-cf-name (tree-map-org-val-compare-fn-name tmorg))
						   ((key-default? (eq key-cf-name 'compare))
						    (val-default? (eq val-cf-name 'compare)))))
					      (format nil " |}~:[[~:[~S~;~*~];~:[~S~;~*~]]~;~4*~]~@[/~S~]"
						      (and key-default? val-default?)
						      key-default? key-cf-name val-default? val-cf-name
						      (map-default map))))
    (do-map (x y map)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      ;; There might be a map entry for 'quote or 'function...
      (let (#+sbcl (sb-pretty:*pprint-quote-with-syntactic-sugar* nil))
	(write (list x y) :stream stream)))))

(gmap:def-gmap-arg-type map (map)
  "Yields each pair of `map', as two values."
  `((the function (iterator ,map))
    #'(lambda (it) (funcall it ':done?))
    (:values 2 #'(lambda (it) (funcall it ':get)))))

(gmap:def-gmap-arg-type wb-map (map)
  "Yields each pair of `map', as two values."
  `((Make-WB-Map-Tree-Iterator-Internal (wb-map-contents ,map))
    #'WB-Map-Tree-Iterator-Done?
    (:values 2 #'WB-Map-Tree-Iterator-Get)))

(gmap:def-arg-type fun-map (map &key from-end?)
  `((fun-iterator ,map :from-end? ,from-end?)
    #'(lambda (it) (funcall it ':empty?))
    (:values 2 #'(lambda (it) (funcall it ':first)))
    #'(lambda (it) (funcall it ':rest))))

(gmap:def-gmap-res-type map (&key filterp default)
  "Consumes two values from the mapped function; returns a map of the pairs.
Note that `filterp', if supplied, must take two arguments."
  `(nil (:consume 2 (fn (m x y) (WB-Map-Tree-With m x y #'compare #'compare)))
	#'(lambda (tree) (make-wb-map tree +fset-default-tree-map-org+ ,default))
    ,filterp))

(gmap:def-gmap-res-type wb-map (&key filterp default key-compare-fn-name val-compare-fn-name)
  "Consumes two values from the mapped function; returns a wb-map of the pairs.
Note that `filterp', if supplied, must take two arguments."
  (let ((proto-var (gensymx #:prototype-))
	(kcf-var (gensymx #:key-cmp-))
	(vcf-var (gensymx #:val-cmp-)))
    `(nil (:consume 2 (fn (tree k v) (WB-Map-Tree-With tree k v ,kcf-var ,vcf-var)))
	  #'(lambda (tree) (make-wb-map tree (wb-map-org ,proto-var) ,default))
	  ,filterp
	  ((,proto-var (empty-wb-map nil ,key-compare-fn-name ,val-compare-fn-name))
	   ((,kcf-var (tree-map-org-key-compare-fn (wb-map-org ,proto-var)))
	    (,vcf-var (tree-map-org-val-compare-fn (wb-map-org ,proto-var))))))))

(gmap:def-gmap-res-type map-union (&key (val-fn nil val-fn?)
				    (default nil default?) filterp)
  "Returns the map-union of the values, optionally filtered by `filterp'.  If `val-fn'
is supplied, it is passed to `map-union' (q.v.).  If `default' is supplied, it is used
as the initial map default."
  `((map . ,(and default? `(:default ,default)))
    ,(if val-fn? `(fn (a b) (map-union a b ,val-fn)) '#'map-union)
    nil ,filterp))

(gmap:def-gmap-res-type map-intersection (&key (val-fn nil val-fn?)
					   (default nil default?) filterp)
  "Returns the map-intersection of the values, optionally filtered by `filterp'.  If
`val-fn' is supplied, it is passed to `map-intersection' (q.v.).  If `default' is
supplied, it is used as the initial map default."
  `((map . ,(and default? `(:default ,default)))
    ,(if val-fn? `(fn (a b) (map-intersection a b ,val-fn)) '#'map-intersection)
    nil ,filterp))

(gmap:def-gmap-res-type map-to-sets (&key filterp key-compare-fn-name val-compare-fn-name)
  "Consumes two values from the mapped function.  Returns a map from the first
values, with each one mapped to a set of the corresponding second values.
Note that `filterp', if supplied, must take two arguments."
  `((empty-wb-map (set) ,key-compare-fn-name ,val-compare-fn-name)
    ;; &&& Could use `WB-Map-Tree-Update-Value' here.
    (:consume 2 (fn (m x y) (with m x (with (lookup m x) y))))
    nil ,filterp))

(defmethod make-load-form ((m wb-map) &optional environment)
  (declare (ignore environment))
  `(convert 'wb-map ',(convert 'list m) :key-compare-fn-name ',(tree-map-org-key-compare-fn-name (wb-map-org m))
					:val-compare-fn-name ',(tree-map-org-val-compare-fn-name (wb-map-org m))))


;;; ================================================================================
;;; CHAMP maps

(defstruct (hash-map-org
	     (:constructor make-hash-map-org (key-compare-fn-name key-compare-fn key-hash-fn
					      val-compare-fn-name val-compare-fn val-hash-fn))
	     (:predicate hash-map-org?)
	     (:copier nil))
  (key-compare-fn-name nil :type symbol :read-only t)
  (key-compare-fn nil :type function :read-only t)
  (key-hash-fn nil :type function :read-only t)
  (val-compare-fn-name nil :type symbol :read-only t)
  (val-compare-fn nil :type function :read-only t)
  (val-hash-fn nil :type function :read-only t))

(declaim (inline make-ch-map))

(defstruct (ch-map
	     (:include map)
	     (:constructor make-ch-map (contents org default))
	     (:predicate ch-map?)
	     (:print-function print-ch-map)
	     (:copier nil))
  (contents nil :read-only t)
  (org nil :type hash-map-org :read-only t))

(defparameter *empty-ch-map*
  (make-ch-map nil (make-hash-map-org 'compare #'compare #'hash-value 'compare #'compare #'hash-value)
	       nil))

(declaim (inline empty-ch-map))
(defun empty-ch-map (&optional default key-compare-fn-name val-compare-fn-name)
  (if (and (null key-compare-fn-name) (null val-compare-fn-name))
      (if (null default)
	  *empty-ch-map*
	(make-ch-map nil (ch-map-org *empty-ch-map*) default))
    (empty-ch-custom-map default (or key-compare-fn-name 'compare) (or val-compare-fn-name 'compare))))

(deflex +empty-ch-custom-map-cache+ (make-hash-table :test 'equal))

(defun empty-ch-custom-map (default key-compare-fn-name val-compare-fn-name)
  (assert (and (symbolp key-compare-fn-name) (not (null key-compare-fn-name))))
  (assert (and (symbolp val-compare-fn-name) (not (null val-compare-fn-name))))
  (if (and (eq key-compare-fn-name 'compare) (eq val-compare-fn-name 'compare))
      (if (null default) *empty-ch-map*
	(make-ch-map nil (ch-map-org *empty-ch-map*) default))
    ;; See note in `empty-wb-custom-map'.
    (let ((cache-key (list key-compare-fn-name val-compare-fn-name))
	  ((prev-instance (gethash cache-key +empty-ch-custom-map-cache+)))
	  (key-hash-fn-name (or (get key-compare-fn-name 'hash-function)
				(error "key-compare-fn-name `~S' not defined for hashing -- see `define-hash-function'"
				       key-compare-fn-name)))
	  (val-hash-fn-name (or (get val-compare-fn-name 'hash-function)
				(error "val-compare-fn-name `~S' not defined for hashing -- see `define-hash-function'"
				       val-compare-fn-name)))
	  ((key-compare-fn (symbol-function key-compare-fn-name))
	   (key-hash-fn (symbol-function key-hash-fn-name))
	   (val-compare-fn (symbol-function val-compare-fn-name))
	   (val-hash-fn (symbol-function val-hash-fn-name))))
      (if (and prev-instance
	       (let ((prev-org (ch-map-org prev-instance)))
		 (and (eq key-compare-fn (hash-map-org-key-compare-fn prev-org))
		      (eq key-hash-fn (hash-map-org-key-hash-fn prev-org))
		      (eq val-compare-fn (hash-map-org-val-compare-fn prev-org))
		      (eq val-hash-fn (hash-map-org-val-hash-fn prev-org))
		      (equal?-cmp default (map-default prev-instance) val-compare-fn))))
	  prev-instance
	(setf (gethash cache-key +empty-ch-custom-map-cache+)
	      (make-ch-map nil (make-hash-map-org key-compare-fn-name key-compare-fn key-hash-fn
						  val-compare-fn-name val-compare-fn val-hash-fn)
			   default))))))

(defmethod empty-instance-function ((class-name (eql 'ch-map)))
  `empty-ch-map)

(defmethod empty-map-like ((m ch-map))
  (let ((hmorg (ch-map-org m)))
    (empty-ch-custom-map (map-default m) (hash-map-org-key-compare-fn-name hmorg)
			 (hash-map-org-val-compare-fn-name hmorg))))

(defmethod key-compare-fn ((m ch-map))
  (hash-map-org-key-compare-fn (ch-map-org m)))

(defmethod val-compare-fn ((m ch-map))
  (hash-map-org-val-compare-fn (ch-map-org m)))

(defmethod key-compare-fn-name ((m ch-map))
  (hash-map-org-key-compare-fn-name (ch-map-org m)))

(defmethod val-compare-fn-name ((m ch-map))
  (hash-map-org-val-compare-fn-name (ch-map-org m)))

(defmethod with-default ((m ch-map) new-default)
  (make-ch-map (ch-map-contents m) (ch-map-org m) new-default))

(defmethod empty? ((m ch-map))
  (null (ch-map-contents m)))

(defmethod size ((m ch-map))
  (ch-map-tree-size (ch-map-contents m)))

(defmethod with ((m ch-map) key &optional (value nil value?))
  (check-three-arguments value? 'with 'ch-map)
  (let ((contents (ch-map-contents m))
	(hmorg (ch-map-org m))
	((new-contents (ch-map-tree-with contents key value
					 (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)
					 (hash-map-org-val-hash-fn hmorg) (hash-map-org-val-compare-fn hmorg)))))
    (if (eq new-contents contents)
	m
      (make-ch-map new-contents hmorg (map-default m)))))

(defmethod less ((m ch-map) key &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'ch-map)
  (let ((contents (ch-map-contents m))
	(hmorg (ch-map-org m))
	((new-contents (ch-map-tree-less contents key (hash-map-org-key-hash-fn hmorg)
					 (hash-map-org-key-compare-fn hmorg) (hash-map-org-val-hash-fn hmorg)))))
    (if (eq new-contents contents)
	m
      (make-ch-map new-contents hmorg (map-default m)))))

(defmethod lookup ((m ch-map) key)
  (let ((hmorg (ch-map-org m))
	((val? val (ch-map-tree-lookup (ch-map-contents m) key
				       (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)))))
    ;; Our internal convention is the reverse of the external one.
    (values (if val? val (map-default m)) val?)))

(defmethod domain-contains? ((m ch-map) x)
  (let ((hmorg (ch-map-org m)))
    (ch-map-tree-lookup (ch-map-contents m) x (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg))))

(defmethod internal-do-map ((m ch-map) elt-fn value-fn)
  (declare ;(optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  (do-ch-map-tree-pairs (x y (ch-map-contents m) (funcall value-fn))
    (funcall elt-fn x y)))

(defmethod convert ((to-type (eql 'wb-map)) (m ch-map) &key key-compare-fn-name val-compare-fn-name)
  (convert-to-wb-map m nil nil
    (let ((tree nil))
      (do-ch-map-tree-pairs (k v (ch-map-contents m) tree)
	(setq tree (WB-Map-Tree-With tree k v key-compare-fn val-compare-fn))))))

(defun print-ch-map (map stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "##{|"
				    :suffix (let ((hmorg (ch-map-org map))
						  ((key-cf-name (hash-map-org-key-compare-fn-name hmorg))
						   (val-cf-name (hash-map-org-val-compare-fn-name hmorg))
						   ((key-default? (eq key-cf-name 'compare))
						    (val-default? (eq val-cf-name 'compare)))))
					      (format nil " |}~:[[~:[~S~;~*~];~:[~S~;~*~]]~;~4*~]~@[/~S~]"
						      (and key-default? val-default?)
						      key-default? key-cf-name val-default? val-cf-name
						      (map-default map))))
    (do-map (x y map)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      ;; There might be a map entry for 'quote or 'function...
      (let (#+sbcl (sb-pretty:*pprint-quote-with-syntactic-sugar* nil))
	(write (list x y) :stream stream)))))


;;; ================================================================================
;;; Seqs

(declaim (inline make-wb-seq))

(defstruct (wb-seq
	     (:include seq)
	     (:constructor make-wb-seq (contents &optional default))
	     (:predicate wb-seq?)
	     (:print-function print-wb-seq)
	     (:copier nil))
  "A class of functional seqs (sequences, but we use the short name to avoid
confusion with `cl:sequence') represented as weight-balanced binary trees.
This is the default implementation of seqs in FSet."
  (contents nil :read-only t))


(defparameter *empty-wb-seq* (make-wb-seq nil))

(declaim (inline empty-seq))
(defun empty-seq (&optional default)
  "Returns an empty seq of the default implementation."
  (if default (make-wb-seq nil default)
    *empty-wb-seq*))

(defmethod empty-instance-function ((class-name (eql 'seq)))
  'empty-seq)

(declaim (inline empty-wb-seq))
(defun empty-wb-seq ()
  "Returns an empty wb-seq."
  *empty-wb-seq*)

(defmethod empty-instance-function ((class-name (eql 'wb-seq)))
  'empty-wb-seq)

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
      (locally (declare (type fixnum key))
        (let ((val? val (WB-Seq-Tree-Subscript (wb-seq-contents s) key)))
          (values (if val? val (seq-default s)) val?)))
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
    (make-wb-seq (WB-Seq-Tree-Append tree val)
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
      ;; (setq size idx)
      )
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
      (setq result (WB-Set-Tree-With result i #'compare)))
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

(defmethod convert ((to-type (eql 'string)) (s wb-seq) &key)
  (WB-Seq-Tree-To-String (wb-seq-contents s)))

(defmethod convert ((to-type (eql 'seq)) (l list) &key)
  (make-wb-seq (WB-Seq-Tree-From-List l)))

(defmethod convert ((to-type (eql 'wb-seq)) (l list) &key)
  (make-wb-seq (WB-Seq-Tree-From-List l)))

(defmethod convert ((to-type (eql 'list)) (s wb-seq) &key)
  (WB-Seq-Tree-To-List (wb-seq-contents s)))

(defmethod convert ((to-type (eql 'seq)) (s set) &key)
  (make-wb-seq (wb-seq-tree-from-iterable (iterator s) (size s))))

(defmethod convert ((to-type (eql 'wb-seq)) (s set) &key)
  (make-wb-seq (wb-seq-tree-from-iterable (iterator s) (size s))))

(defmethod convert ((to-type (eql 'seq)) (b bag) &key pairs? (pair-fn #'cons))
  (convert 'wb-seq b :pairs? pairs? :pair-fn pair-fn))

(defmethod convert ((to-type (eql 'wb-seq)) (b bag) &key pairs? (pair-fn #'cons))
  (make-wb-seq (wb-seq-tree-from-iterable (if pairs?
					      (let ((it (iterator b :pairs? t)))
						(lambda (op)
						  (if (eq op ':get)
						      (let ((v n (funcall it ':get)))
							(funcall pair-fn v n))
						    (funcall it op))))
					    (iterator b))
					  (if pairs? (set-size b) (size b)))))

(defmethod convert ((to-type (eql 'seq)) (m map) &key (pair-fn #'cons))
  (convert 'wb-seq m :pair-fn pair-fn))

(defmethod convert ((to-type (eql 'wb-seq)) (m map) &key (pair-fn #'cons))
  (make-wb-seq (wb-seq-tree-from-iterable (let ((m-it (iterator m)))
					    (lambda (op)
					      (ecase op
						(:get (let ((k v (funcall m-it ':get)))
							(funcall pair-fn k v))))))
					  (size m))
	       (map-default m)))

;;; Prior to FSet 1.4.0, this method ignored the defaults, so two seqs with the same
;;; elements but different defaults compared `:equal'.  While that was clearly a bug,
;;; there is a remote chance that someone has inadvertently depended on this behavior.
(defmethod compare ((s1 wb-seq) (s2 wb-seq))
  (let ((comp (WB-Seq-Tree-Compare (wb-seq-contents s1) (wb-seq-contents s2) #'compare)))
    (if (member comp '(:less :greater))
	comp
      (let ((def-comp (compare (seq-default s1) (seq-default s2))))
	(if (member def-comp '(:less :greater))
	    def-comp
	  (if (or (eq comp ':unequal) (eq def-comp ':unequal))
	      ':unequal
	    ':equal))))))

(defmethod compare-lexicographically ((s1 wb-seq) (s2 wb-seq) &key (val-compare-fn #'compare))
  (WB-Seq-Tree-Compare-Lexicographically (wb-seq-contents s1) (wb-seq-contents s2) val-compare-fn))

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
  (assert (and (typep start 'fixnum) (typep end 'fixnum)))
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

(defmethod iterator ((s wb-seq) &key)
  (Make-WB-Seq-Tree-Iterator (wb-seq-contents s)))

(defmethod fun-iterator ((s wb-seq) &key from-end?)
  (if from-end?
      (WB-Seq-Tree-Rev-Fun-Iter (wb-seq-contents s))
    (WB-Seq-Tree-Fun-Iter (wb-seq-contents s))))

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
  (seq-filter (coerce-to-function fn) s))

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
  (seq-partition (coerce-to-function fn) s))

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

(defmethod sort-and-group ((s seq) pred &key key)
  (if (empty? s) s
    (let ((sorted (stable-sort s pred :key key))
	  (result (empty-seq))
	  (group (empty-seq)))
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

(defmethod image ((fn function) (s seq))
  (seq-image fn s))

(defmethod image ((fn symbol) (s seq))
  (seq-image (coerce-to-function fn) s))

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
  (seq-reduce fn s initial-value (and key (coerce-to-function key)) init?
	      start end from-end))

(defmethod reduce ((fn symbol) (s seq)
		   &key key (initial-value nil init?)
		   (start 0) (end (size s)) (from-end nil))
  (seq-reduce (coerce-to-function fn) s initial-value (and key (coerce-to-function key))
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
  (let ((test (coerce-to-function-or-equal? test))
        (start (or start 0))
	(end (or end (size s))))
    (if key
	(let ((key (coerce-to-function key)))
          (do-seq (x s :start start :end end :from-end? from-end :value nil)
            (when (funcall test item (funcall key x))
              (return x))))
        (do-seq (x s :start start :end end :from-end? from-end :value nil)
          (when (funcall test item x)
            (return x))))))

(defmethod find-if (pred (s seq) &key key start end from-end)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce-to-function pred))
	(start (or start 0))
	(end (or end (size s))))
    (if key
	(let ((key (coerce-to-function key)))
	  (do-seq (x s :start start :end end :from-end? from-end :value nil)
	    (when (funcall pred (funcall key x))
	      (return x))))
      (do-seq (x s :start start :end end :from-end? from-end :value nil)
	(when (funcall pred x)
	  (return x))))))

(defmethod find-if-not (pred (s seq) &key key start end from-end)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce-to-function pred)))
    (find-if #'(lambda (x) (not (funcall pred x))) s
	     :key key :start start :end end :from-end from-end)))

(defmethod count (item (s seq) &key key test start end from-end)
  (declare (optimize (speed 3) (safety 0)))
  (let ((test (coerce-to-function-or-equal? test))
        (total 0)
	(start (or start 0))
	(end (or end (size s))))
    (declare (fixnum total))
    (if key
	(let ((key (coerce-to-function key)))
          (do-seq (x s :start start :end end :from-end? from-end)
            (when (funcall test item (funcall key x))
              (incf total))))
      (do-seq (x s :start start :end end :from-end? from-end)
	(when (funcall test item x)
	  (incf total))))
    total))

(defmethod count-if (pred (s seq) &key key start end from-end)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce-to-function pred))
	(n 0)
	(start (or start 0))
	(end (or end (size s))))
    (declare (fixnum n))
    (if key
	(let ((key (coerce-to-function key)))
	  (do-seq (x s :start start :end end :from-end? from-end)
	    (when (funcall pred (funcall key x))
	      (incf n))))
      (do-seq (x s :start start :end end :from-end? from-end)
	(when (funcall pred x)
	  (incf n))))
    n))

(defmethod count-if-not (pred (s seq) &key key start end from-end)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce-to-function pred)))
    (count-if #'(lambda (x) (not (funcall pred x))) s
	      :key key :start start :end end :from-end from-end)))

(defmethod position (item (s seq) &key key test start end from-end)
  (declare (optimize (speed 3) (safety 0))
           (type (or fixnum null) start end))
  (let ((test default? (coerce-to-function-or-equal? test))
        (start (or start 0))
	((pos start))
	(end (or end (size s))))
    (declare (fixnum start end pos))
    (block done-block
      (flet ((done () (return-from done-block
                        (if from-end (the fixnum (+ start (the fixnum (- end pos 1)))) pos))))
        (if key
            (let ((key (coerce-to-function key)))
              (if default?
                  (do-seq (x s :start start :end end :from-end? from-end)
                    (when (equal? item (funcall key x))
                      (done))
                    (incf pos))
                (do-seq (x s :start start :end end :from-end? from-end)
		  (when (funcall test item (funcall key x))
		    (done))
		  (incf pos))))
            (if default?
                (do-seq (x s :start start :end end :from-end? from-end)
                  (when (equal? item x)
                    (done))
                  (incf pos))
              (do-seq (x s :start start :end end :from-end? from-end)
		(when (funcall test item x)
		  (done))
		(incf pos))))))))

(defmethod position-if (pred (s seq) &key key start end from-end)
  (declare (optimize (speed 3) (safety 0))
           (type (or fixnum null) start end))
  (let ((pred (coerce-to-function pred))
	(start (or start 0))
	((pos start))
	(end (or end (size s))))
    (declare (fixnum start end pos))
    (block done-block
      (flet ((done () (return-from done-block
                        (if from-end (the fixnum (+ start (the fixnum (- end pos 1)))) pos))))
        (if key
            (let ((key (coerce-to-function key)))
              (do-seq (x s :start start :end end :from-end? from-end)
                (when (funcall pred (funcall key x))
                  (done))
                (incf pos)))
          (do-seq (x s :start start :end end :from-end? from-end)
	    (when (funcall pred x)
	      (done))
	    (incf pos)))))))

(defmethod position-if-not (pred (s seq) &key key start end from-end)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce-to-function pred)))
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
	(test (if test (coerce-to-function test) #'equal?))
	(key (and key (coerce-to-function key))))
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
	(pred (coerce-to-function pred))
	(key (and key (coerce-to-function key))))
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
  (let ((pred (coerce-to-function pred)))
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
	(test (if test (coerce-to-function test) #'equal?))
	(key (and key (coerce-to-function key))))
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
	(pred (coerce-to-function pred))
	(key (and key (coerce-to-function key))))
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
  (let ((pred (coerce-to-function pred)))
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

(gmap:def-gmap-arg-type seq (seq)
  "Yields the elements of `seq'."
  `((the function (iterator ,seq))
    #'(lambda (it) (funcall it ':done?))
    #'(lambda (it) (funcall it ':get))))

(gmap:def-gmap-arg-type wb-seq (seq)
  "Yields the elements of `seq'."
  `((Make-WB-Seq-Tree-Iterator-Internal (wb-seq-contents ,seq))
    #'WB-Seq-Tree-Iterator-Done?
    #'WB-Seq-Tree-Iterator-Get))

(gmap:def-gmap-res-type seq (&key filterp)
  "Returns a seq of the values, optionally filtered by `filterp'."
  `(nil
    #'(lambda (a b) (cons b a))
    #'(lambda (s) (convert 'seq (nreverse s)))
    ,filterp))

(gmap:def-gmap-res-type wb-seq (&key filterp)
  "Returns a seq of the values, optionally filtered by `filterp'."
  `(nil
    #'(lambda (a b) (cons b a))
    #'(lambda (s) (convert 'seq (nreverse s)))
    ,filterp))

(gmap:def-gmap-res-type concat (&key filterp)
  "Returns the concatenation of the seq values, optionally filtered by `filterp'."
  `((seq) #'concat nil ,filterp))

(defmethod make-load-form ((s wb-seq) &optional environment)
  (declare (ignore environment))
  `(convert 'wb-seq ',(convert 'list s)))


;;; ================================================================================
;;; CL Sequences and Functions

;;; Convenience methods for some of the FSet generic functions.

(defmethod empty? ((l list))
  (null l))

(defmethod empty? ((s sequence))
  (zerop (length s)))

(defmethod size ((s sequence))
  (length s))

(defmethod lookup ((s sequence) (idx integer))
  (values (elt s idx) t))

(defmethod lookup ((fn function) (v t))
  (funcall fn v))

(defmethod lookup ((fn symbol) (v t))
  (funcall fn v))

(defmethod convert ((to-type (eql 'list)) (v vector) &key)
  (coerce v 'list))

(defmethod convert ((to-type (eql 'vector)) (l list) &key)
  (coerce l 'vector))

(defmethod convert ((to-type (eql 'list)) (s sequence) &key)
  (let ((result nil))
    (dotimes (i (length s))
      (push (elt s i) result))
    (nreverse result)))

(defmethod convert ((to-type (eql 'vector)) (s sequence) &key)
  (let* ((seq-len (length s))
         (result (make-array seq-len)))
    (dotimes (i seq-len result)
      (setf (svref result i) (elt s i)))))

(declaim (inline compose-with-key-fn))
(defun compose-with-key-fn (fn collection)
  (lambda (key) (lookup collection (funcall fn key))))

(defmethod compose ((fn function) (m wb-map))
  (compose-with-key-fn fn m))

(defmethod compose ((fn symbol) (m wb-map))
  (compose-with-key-fn fn m))

(defmethod compose ((fn function) (s seq))
  (compose-with-key-fn fn s))

(defmethod compose ((fn symbol) (s seq))
  (compose-with-key-fn fn s))



;;; ================================================================================
;;; Miscellany

;;; Oooops -- I somehow thought CL already had this.
(define-condition simple-program-error (simple-condition program-error)
  ())

