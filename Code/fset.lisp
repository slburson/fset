;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: fset.lisp
;;; Contents: Top level of FSet, the fast functional set-theoretic datatypes package.
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2025 Scott L. Burson.
;;; FSet is licensed under the 2-clause BSD license; see LICENSE.
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
    "If `collection' is a map, returns three values: if it contains a key equal
to `key', the corresponding value, true, and the key found; otherwise, the map's
default, false, and `nil'.

If `collection' is a seq, takes `key' as an index and returns two values: if
the index is in bounds, the corresponding element and true; otherwise, the
seq's default and false.

If `collection' is a set or bag that contains a member equal to `key', returns
true and the member as two values, else false and `nil'; this is useful for
canonicalization."))
(defgeneric fset2:lookup (collection key)
  (:documentation
    "If `collection' is a map, returns three values: if it contains a key equal
to `key', the corresponding value, true, and the key found; otherwise, the map's
default, false, and `nil'.  If there's no mapping for `key' and the map has no
default, signals an error of type `map-domain-error'.

If `collection' is a seq, takes `key' as an index and returns two values: if
the index is in bounds, the corresponding element and true; otherwise, the
seq's default and false.  If the index is out of bounds and the seq has no
default, signals an error of type `seq-bounds-error'.

If `collection' is a set that contains a member equal to `key', returns true
and the member as two values, else false and `nil'; this is useful for
canonicalization.

If `collection' is a bag that contains a member equal to `key', returns its
multiplicity and the member as two values, else zero and `nil'."))

(defgeneric rank (collection value)
  (:documentation
    "Defined on tree \(WB\) collections only.  See `index'.

If `collection' is a set or bag that contains `value', returns the rank
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
(defgeneric fset2:rank (collection value)
  (:documentation
    "Defined on tree \(WB\) collections only.  See `index'.

If `collection' is a set or bag that contains `value', returns the rank
of `value' in the ordering defined by `compare', and a true second value.
If `collection' is a map whose domain contains `value', returns the rank of
`value' in the domain of the map, and a true second value.  If `value' is
not in the collection, the second value is false, and the first value is the
rank that `value' would have if it were added.  Note that if there are
values/keys that are unequal but equivalent to `value', an arbitrary order
will be imposed on them for this purpose; but another collection that is
`equal?' but not `eq' to this one will in general order them differently.
Also, on a bag, multiplicities are ignored for this purpose."))

(defgeneric at-rank (collection rank)
  (:documentation
    "Defined on tree \(WB\) collections only.  See `at-index'.

On a set, returns the element with rank `rank'; on a bag, returns
that element with its multiplicity as a second value; on a map, returns
the pair with that rank as two values.  Note that if there are values/keys
that are unequal but equivalent in the collection, an arbitrary order will be
imposed on them for this purpose; but another collection that is `equal?'
but not `eq' to this one will in general order them differently.

Also, if the collection is hash-based (`ch-set' etc.), the ordering will be
based on the hash values, so it won't be predictable or usable for any other
purpose, though it will be deterministic."))

(defgeneric index (collection value)
  (:documentation
    "If `collection' contains `value' \(or for a map, if its domain does\),
returns the index of `value' in the collection's iteration order; otherwise
null.  On `wb-set', `wb-map', and `wb-bag', the index is the same as the rank
\(see `rank'\)."))

(defgeneric at-index (collection index)
  (:documentation
    "Returns the element/pair whose index in the iteration order is `index'."))

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

(defgeneric split-from (collection value)
  (:documentation
    "Defined on tree \(WB\) sets, maps, and bags only.  Returns the
subcollection with elements/keys greater than or equal to `value'."))

(defgeneric split-above (collection value)
  (:documentation
    "Defined on tree \(WB\) sets, maps, and bags only.  Returns the
subcollection with elements/keys greater than `value'."))

(defgeneric split-through (collection value)
  (:documentation
    "Defined on tree \(WB\) sets, maps, and bags only.  Returns the
subcollection with elements/keys less than or equal to `value'."))

(defgeneric split-below (collection value)
  (:documentation
    "Defined on tree \(WB\) sets, maps, and bags only.  Returns the
subcollection with elements/keys less than `value'."))

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

(define-generics (filter fset2:filter) (fn collection)
  (:documentation
    "Returns a new collection containing those members or pairs of `collection'
for which `fn' returns true.  If `collection' is a set, bag, or seq, `fn' is
called with one argument; if a map, `fn' is called with two arguments, the key
and the value (the map-default of the result is that of `collection').  As well
as a Lisp function, `fn' can be a map, or a set (which is treated as mapping
its members to true and everything else to false)."))

(define-methods (filter fset2:filter) (fn (s sequence))
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

(defgeneric image (fn collection &key)
  (:documentation
    "Returns a new collection containing the result of applying `fn' to each
member of `collection', which may be a set, bag, map, or seq.  In the bag case,
the multiplicity of each member of the result is the sum of the multiplicities
of the values that `fn' maps to it.  In the map case, `fn' will be called with
two arguments, the domain and range values, and is expected to return two
values, which become the new domain and range values.  In the map and seq
cases, the returned collection has the same default as the argument.

Except in the seq case, you may wish to specify the organization of the result,
so the set and bag methods take keyword argument `:compare-fn-name', and the
map method takes `:key-compare-fn-name' and `:val-compare-fn-name'.

If `collection' is a map or seq, the default of the result is the same.

As well as a Lisp function, `fn' can be a map, or a set (which is treated as
mapping its members to true and everything else to false)."))

(defgeneric fset2:image (fn collection &key)
  (:documentation
    "Returns a new collection containing the result of applying `fn' to each
member of `collection', which may be a set, bag, or seq.  In the bag case,
the multiplicity of each member of the result is the sum of the multiplicities
of the values that `fn' maps to it.  In the seq case, if the argument seq has
a default, the default of the result will be the result of applying `fn' to it.

Except in the seq case, you may wish to specify the organization of the result,
so the set and bag methods take keyword argument `:compare-fn-name'.

As well as a Lisp function, `fn' can be a map, or a set (which is treated as
mapping its members to true and everything else to false).

For imaging a map, see `compose' and `map-image'."))

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

(defgeneric range (map)
  (:documentation
    "Returns the range of the map, that is, the set of all values to which keys
are mapped by the map."))

(defgeneric default (collection)
  (:documentation
    "Returns two values.  If the map or seq has a default \(the value returned
by `lookup' when the supplied key or index is not in the domain\), returns the
default and a true second value; otherwise, the second value is false."))

(defgeneric with-default (collection new-default)
  (:documentation
    "Returns a new map or seq with the same contents as `collection' but whose
default is now `new-default'."))

(defgeneric fset2:without-default (collection)
  (:documentation
    "Returns a new map, replay map, or seq with the same contents as `collection'
but with no default.  Subsequent `lookup' operations on a key not in the map,
or a index outside the seq's bounds, will signal an error."))

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

(defgeneric fset2:map-union (map1 map2 &optional val-fn)
  (:documentation
    "Returns a map containing all the keys of `map1' and `map2', where the
value for each key contained in only one map is the value from that map, and
the value for each key contained in both maps is the result of calling
`val-fn' on the value from `map1' and the value from `map2'.  `val-fn'
defaults to simply returning its second argument, so the entries in `map2'
simply shadow those in `map1'.  The default for the new map is: if both maps
have defaults, the result of calling `val-fn' on those defaults; if only one
map has a default, that default; otherwise none.

`map-union' assumes that `val-fn' is idempotent, i.e., if the two values
passed to `val-fn' are equal, `val-fn' must return the same value; it may
elide calls to `val-fn' on that basis.

If `val-fn' returns `:no-value' as a second value, the result will contain
no pair with the corresponding key."))

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

(defgeneric fset2:map-intersection (map1 map2 &optional val-fn)
  (:documentation
    "Returns a map containing all the keys that are in the domains of both
`map1' and `map2', where the value for each key is the result of calling
`val-fn' on the value from `map1' and the value from `map2'.  `val-fn'
defaults to simply returning its second argument, so the entries in `map2'
simply shadow those in `map1'.  The default for the new map is the result
of calling `val-fn' on the defaults for the two maps, if they both have
defaults; else none.

`map-intersection' assumes that `val-fn' is idempotent, i.e., if the two
values passed to `val-fn' are equal, `val-fn' must return the same value;
it may elide calls to `val-fn' on that basis.

If `val-fn' returns `:no-value' as a second value, the result will contain
no pair with the corresponding key."))

(defgeneric map-difference-2 (map1 map2)
  (:documentation
    "Returns, as two values: a map containing all the pairs that are in `map1'
but not `map2', with the same default as `map1'; and one containing all the
pairs that are in `map2' but not `map1', with the same default as `map2'."))
(defgeneric fset2:map-difference-2 (map1 map2)
  (:documentation
    "Returns, as two values: a map containing all the pairs that are in `map1'
but not `map2', and one containing all the pairs that are in `map2' but not
`map1'.

The default for each returned map is that of its corresponding argument, if
it has a default, except if the defaults are equal, in which case neither
returned map has a default."))

;;; I split this out from `image' in FSet 2 because `fn' is called with two arguments, so
;;; that (a) we can't call `fn' to get the new default because we don't have a domain
;;; value corresponding to the range value, and (b) `fn' can't be a map, set, or bag.
;;; `compose' is probably much more useful.
(defgeneric map-image (fn collection
			&key key-compare-fn-name val-compare-fn-name
			default no-default?)
  (:documentation
    "Returns a new map containing the result of applying `fn' to each pair of
`collection'.  That is, `fn' is called with both the domain and range values
as arguments; it is expected to return two values, which become the new domain
and range values.  The default of the returned map is `nil' unless a different
default is specified by `:default', or `:no-default?' is true.

See also `compose', which may fit your needs better."))

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

(defgeneric compose (map1 map2-or-fn &key)
  (:documentation
    "Returns a new map with the same domain as `map1', which maps each member
of that domain to the result of applying first `map1' to it, then applying
`map2-or-fn' to the result.  `map2-or-fn' can also be a sequence, which is
treated as a map from indices to members."))
(defgeneric fset2:compose (map1 map2-or-fn &key)
  (:documentation
    "Returns a new map with the same domain as `map1', which maps each member
of that domain to the result of applying first `map1' to it, then applying
`map2-or-fn' to the result.  `map2-or-fn' can also be a sequence, which is
treated as a map from indices to members.  If `map1' has a default, the value
of `map2-or-fn' applied to that default becomes the default of the returned
map."))

(defgeneric char-seq? (seq)
  (:documentation
    "Returns true iff `seq' contains only characters."))

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
    "Returns a new sequence like `seq' but with `val' inserted at `idx' \(the seq
is extended in either direction if needed prior to the insertion; previously
uninitialized indices are filled with the seq's default\)."))

(defgeneric splice (seq idx subseq)
  (:documentation
    "Returns a new sequence like `seq' but with the elements of `subseq' inserted
at `idx' \(the seq is extended in either direction if needed prior to the insertion;
previously uninitialized indices are filled with the seq's default\)."))

(defgeneric concat (seq1 &rest seqs)
  (:documentation
    "Returns the concatenation of `seq1' with each of `seqs'.  The result has the
same default as `seq1'."))

;;; This is the opposite order from `cl:coerce', but I like it better, because I
;;; think the calls are easier to read with the type first.  It's also consistent
;;; with `cl:concatenate' -- the inconsistency between `coerce' and `concatenate'
;;; has long bugged me.
(defgeneric convert (to-type collection &key)
  (:documentation "Converts the collection to the specified type.  Some methods
have additional keyword parameters to further specify the kind of conversion.
\(`sequence' here refers to a CL sequence -- either a list or a vector, unless
the implementation has more sequence subtypes.  FSet's `seq' is not a subtype of
`sequence'.\)

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
                     set                list              18, 2
                     wb-set             list              2, 15
                     list               set
                     set                seq               18, 2
                     wb-set             seq               2, 15
                     set                sequence          18, 2
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
                     wb-map             wb-map            0, 16, 21
                     wb-map             map               16, 21
                     ch-map             ch-map            0, 16, 21
                     ch-map             map               16, 21
                     list               map               5
                     alist              map
                     seq                map               5
                     vector             map               5
                     set                map               5
                     map                list              2, 6, 7, 18, 20
                     wb-map             list              2, 6, 7, 16, 20
                     ch-map             list              6, 7, 16
                     map                seq               18, 2, 6, 7, 17
                     wb-map             seq               2, 6, 7, 16, 17
                     ch-map             seq               6, 7, 16
                     map                sequence          2, 6, 7, 18, 20
                     wb-map             sequence          2, 6, 7, 16, 20
                     ch-map             sequence          6, 7, 16
                     map                bag               18, 20
                     wb-map             bag               16, 20
                     ch-map             bag               16, 20
                     map                hash-table        18, 20
                     wb-map             hash-table        16, 20
                     ch-map             hash-table        16, 20
                     hash-table         map               8

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
                     base-string        wb-seq            14
                     seq                bag               1, 5
                     wb-seq             bag               5
                     seq                map               1, 5, 17
                     wb-seq             map               5, 17

                     2-relation         2-relation        0
                     wb-2-relation      wb-2-relation     0, 16
                     ch-2-relation      ch-2-relation     0, 16
                     2-relation         set               6, 19
                     wb-2-relation      set               6, 16
                     ch-2-relation      set               6, 16
                     set                wb-2-relation     1, 5
                     set                ch-2-relation     19, 5
                     wb-set             2-relation        5
                     ch-set             2-relation        5
                     2-relation         wb-map            1, 10
                     2-relation         ch-map            19, 10
                     wb-2-relation      map               10
                     2-relation         list              19, 6
                     wb-2-relation      list              6, 16
                     ch-2-relation      list              6, 16
                     2-relation         seq               19, 6
                     wb-2-relation      seq               6, 16
                     ch-2-relation      seq               6, 16
                     map                wb-2-relation     1, 11, 20
                     map                ch-2-relation     19, 11, 20
                     wb-map             wb-2-relation     11, 20
                     ch-map             ch-2-relation     11, 20
                     map-to-sets        wb-2-relation     12
                     map-to-sets        ch-2-relation     12

                     tuple              tuple             0
                     dyn-tuple          dyn-tuple         0
                     map                tuple             1
                     wb-map             tuple
                     list               tuple             5
                     tuple              list              6, 7, 13
                     dyn-tuple          list              7, 13

                     replay-set         replay-set        0
                     wb-replay-set      wb-replay-set     0
                     ch-replay-set      ch-replay-set     0
                     list               wb-replay-set
                     vector             wb-replay-set
                     seq                wb-replay-set     1
                     wb-seq             wb-replay-set
                     set                wb-replay-set     1
                     wb-set             wb-replay-set
                     replay-set         list              19
                     wb-replay-set      list
                     replay-set         seq               19
                     wb-replay-set      seq
                     replay-set         sequence          19
                     wb-replay-set      sequence

                     replay-map         replay-map        0
                     wb-replay-map      wb-replay-map     0
                     ch-replay-map      ch-replay-map     0
                     map                wb-replay-map     1
                     map                ch-replay-map     19
                     wb-map             wb-replay-map
                     ch-map             ch-replay-map
                     list               wb-replay-map     5
                     vector             wb-replay-map     5
                     seq                wb-replay-map     5
                     replay-map         list              19, 6
                     wb-replay-map      list              6
                     replay-map         seq               19, 6
                     wb-replay-map      seq               6
                     replay-map         sequence          19, 6
                     wb-replay-map      sequence          6

                     list               list              0
                     vector             vector            0
                     vector             list
                     list               sequence
                     string             sequence
                     base-string        sequence

In the below, whether the FSet 1 or FSet 2 rule applies is determined by the
package of `to-type' (e.g., `fset:set' or `fset2:set').

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
5. Has keyword parameter `pair-fn', defaulting to `#'cons', specifies the
   function used to combine each key/value pair.
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
    values.  The default of the result is the empty set.
13. Constructs a `dyn-tuple'.
14. Always returns a string (or base-string, if specified).  Signals an error
    if the seq contains a non-character.  Signals `type-error' if a base-string
    is specified and it encounters a non-base character.
15. Has keyword parameter `compare-fn-name'.  If nonnull, must be a symbol, the
    name of the comparison function to be used.  The returned set will be
    converted, if necessary, to use the specified function, or to use `compare'
    if one is not specified.
16. Has keyword parameters `key-compare-fn-name' and `val-compare-fn-name'.  If
    nonnull, these must be symbols, the names of the key and value comparison
    functions, respectively, to be used.  The returned map will be converted,
    if necessary, to use the specified functions, or to use `compare' for those
    not specified.
17. FSet 1: has keyword parameter `default', which specifies the default of the
    result; if not supplied, the default is the same as that of the argument.
    FSet 2: has keyword parameters `default' and `no-default?', which specify
    the default of the result; if not supplied, the default is `nil'.
18. FSet 1: constructs the WB (weight-balanced tree) implementation of its type
    (`wb-set' etc.).  FSet 2: constructs the CHAMP implementation (`ch-set'
    etc.).
19. Constructs the CHAMP implementation of its type (`ch-set' etc.).
20. FSet 1: has keyword parameter `default', which specifies the default of the
    result; if not supplied, the default is `nil'.  FSet 2: also has keyword
    parameter `no-default?' to specify no default.
21. FSet 1: has keyword parameter `default', which specifies the default of the
    result; if not supplied, the default is the same as that of the argument.
    FSet 2: also has keyword parameter `no-default?' to specify no default."))


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
only once, with its multiplicity as the second value, as for a map.

The seq method takes `start' and `end' keyword arguments to restrict the range
of the iteration, and `from-end?' to reverse its direction.  `start' is
inclusive and defaults to 0; `end' is exclusive and defaults to the size of the
seq."))

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

(defgeneric search (sequence-1 sequence-2 &rest keyword-args
		    &key from-end test key start1 start2 end1 end2)
  (:documentation
    "If `sequence-1' and `sequence-2' are Lisp sequences, this simply calls
`cl:search'.  On FSet seqs, the default for `test' is `equal?', and the
`:test-not' keyword is not accepted."))

(defmethod search ((sequence-1 sequence) (sequence-2 sequence) &rest keyword-args
		   &key from-end test key start1 start2 end1 end2)
  (declare (dynamic-extent keyword-args)
	   (ignore from-end test key start1 start2 end1 end2))
  (apply #'cl:search sequence-1 sequence-2 keyword-args))

(defgeneric mismatch (sequence-1 sequence-2 &rest keyword-args
		      &key from-end test key start1 start2 end1 end2)
  (:documentation
    "If `sequence-1' and `sequence-2' are Lisp sequences, this simply calls
`cl:mismatch'.  On FSet seqs, the default for `test' is `equal?', and the
`:test-not' keyword is not accepted."))

(defmethod mismatch ((sequence-1 sequence) (sequence-2 sequence) &rest keyword-args
		     &key from-end test key start1 start2 end1 end2)
  (declare (dynamic-extent keyword-args)
	   (ignore from-end test key start1 start2 end1 end2))
  (apply #'cl:mismatch sequence-1 sequence-2 keyword-args))

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

;;; `(gmap :or ...)' is faster.
(defun some (pred sequence0 &rest more-sequences)
  "FSet generic version of `cl:some'."
  (let ((it0 (the function (iterator sequence0)))
	(more-its (mapcar #'iterator more-sequences))
	(pred (coerce-to-function pred)))
    (do ()
	((or (funcall it0 ':done?)
	     (gmap :or (fn (it) (funcall it ':done?))
		   (:arg list more-its)))
	 nil)
      (let ((val (apply pred (funcall it0 ':get)
			(mapcar (fn (it) (funcall it ':get)) more-its))))
	(when val
	  (return val))))))

;;; `(gmap :and ...)' is faster.
(defun every (pred sequence0 &rest more-sequences)
  "FSet generic version of `cl:every'."
  (let ((it0 (the function (iterator sequence0)))
	(more-its (mapcar #'iterator more-sequences))
	(pred (coerce-to-function pred)))
    (do ()
	((or (funcall it0 ':done?)
	     (gmap :or (fn (it) (funcall it ':done?))
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

(define-methods (filter fset2:filter) ((pred symbol) (ls list))
  (remove-if-not (coerce pred 'function) ls))

(define-methods (filter fset2:filter) ((pred function) (ls list))
  (remove-if-not pred ls))

(defmethod convert ((to-type (eql 'vector)) (v vector) &key)
  v)

(define-methods (partition fset2:partition) ((pred symbol) (ls list))
  (list-partition (coerce-to-function pred) ls))

(define-methods (partition fset2:partition) ((pred function) (ls list))
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

(define-methods (image fset2:image) ((fn function) (l list) &key)
  (mapcar fn l))

(define-methods (image fset2:image) ((fn symbol) (l list) &key)
  (mapcar (coerce fn 'function) l))

(defmethod image ((fn map) (l list) &key)
  (mapcar (lambda (x) (lookup fn x)) l))
(defmethod fset2:image ((fn map) (l list) &key)
  (mapcar (lambda (x) (fset2:lookup fn x)) l))

(defmethod image ((fn set) (l list) &key)
  (mapcar (lambda (x) (lookup fn x)) l))
(defmethod fset2:image ((fn set) (l list) &key)
  (mapcar (lambda (x) (fset2:lookup fn x)) l))

(defmethod image ((fn bag) (l list) &key)
  (mapcar (lambda (x) (lookup fn x)) l))
(defmethod fset2:image ((fn bag) (l list) &key)
  (mapcar (lambda (x) (fset2:lookup fn x)) l))

(define-methods (image fset2:image) ((fn function) (l vector) &key)
  (cl:map 'vector fn l))

(define-methods (image fset2:image) ((fn symbol) (l vector) &key)
  (cl:map 'vector (coerce fn 'function) l))

(defmethod image ((fn map) (l vector) &key)
  (cl:map 'vector (lambda (x) (lookup fn x)) l))
(defmethod fset2:image ((fn map) (l vector) &key)
  (cl:map 'vector (lambda (x) (fset2:lookup fn x)) l))

(defmethod image ((fn set) (l vector) &key)
  (cl:map 'vector (lambda (x) (lookup fn x)) l))
(defmethod fset2:image ((fn set) (l vector) &key)
  (cl:map 'vector (lambda (x) (fset2:lookup fn x)) l))

(defmethod image ((fn bag) (l vector) &key)
  (cl:map 'vector (lambda (x) (lookup fn x)) l))
(defmethod fset2:image ((fn bag) (l vector) &key)
  (cl:map 'vector (lambda (x) (fset2:lookup fn x)) l))


;;; ----------------
;;; This series of methods provides an elegant way to do functional update on small lists.
;;; E.g., (incf (@ x 'first)) --> (setq x (cons (1+ (car x)) (cdr x))).
;;;
;;; I've defined all ten of these because CL has them, but as a stylistic recommendation, if
;;; your list is longer than three or maybe four, you should probably use a dynamic tuple;
;;; see `tuples.lisp'.

(define-methods (lookup fset2:lookup) ((ls list) (key (eql 'first)))
  (cl:first ls))
(define-methods (lookup fset2:lookup) ((ls list) (key (eql 'cl:first))) ; in case of no shadowing-import of `fset:first'
  (cl:first ls))

(defmethod with ((ls list) (key (eql 'first)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (cons val (cdr ls)))
(defmethod with ((ls list) (key (eql 'cl:first)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (cons val (cdr ls)))

(define-methods (lookup fset2:lookup) ((ls list) (key (eql 'second)))
  (second ls))

(defmethod with ((ls list) (key (eql 'second)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (list* (cl:first ls) val (cddr ls)))

(define-methods (lookup fset2:lookup) ((ls list) (key (eql 'third)))
  (third ls))

(defmethod with ((ls list) (key (eql 'third)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (list* (cl:first ls) (second ls) val (cdddr ls)))

(define-methods (lookup fset2:lookup) ((ls list) (key (eql 'fourth)))
  (fourth ls))

(defmethod with ((ls list) (key (eql 'fourth)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (list* (cl:first ls) (second ls) (third ls) val (cddddr ls)))

(define-methods (lookup fset2:lookup) ((ls list) (key (eql 'fifth)))
  (fifth ls))

(defmethod with ((ls list) (key (eql 'fifth)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (list* (cl:first ls) (second ls) (third ls) (fourth ls) val (cdr (cddddr ls))))

(define-methods (lookup fset2:lookup) ((ls list) (key (eql 'sixth)))
  (sixth ls))

(defmethod with ((ls list) (key (eql 'sixth)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (list* (cl:first ls) (second ls) (third ls) (fourth ls) (fifth ls) val (cddr (cddddr ls))))

(define-methods (lookup fset2:lookup) ((ls list) (key (eql 'seventh)))
  (seventh ls))

(defmethod with ((ls list) (key (eql 'seventh)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (list* (cl:first ls) (second ls) (third ls) (fourth ls) (fifth ls) (sixth ls) val (cdddr (cddddr ls))))

(define-methods (lookup fset2:lookup) ((ls list) (key (eql 'eighth)))
  (eighth ls))

(defmethod with ((ls list) (key (eql 'eighth)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (list* (cl:first ls) (second ls) (third ls) (fourth ls) (fifth ls) (sixth ls) (seventh ls) val (cddddr (cddddr ls))))

(define-methods (lookup fset2:lookup) ((ls list) (key (eql 'ninth)))
  (ninth ls))

(defmethod with ((ls list) (key (eql 'ninth)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (list* (cl:first ls) (second ls) (third ls) (fourth ls) (fifth ls) (sixth ls) (seventh ls) (eighth ls)
	 val (cdr (cddddr (cddddr ls)))))

(define-methods (lookup fset2:lookup) ((ls list) (key (eql 'tenth)))
  (tenth ls))

(defmethod with ((ls list) (key (eql 'tenth)) &optional (val nil val?))
  (check-three-arguments val? 'with 'list)
  (list* (cl:first ls) (second ls) (third ls) (fourth ls) (fifth ls) (sixth ls) (seventh ls) (eighth ls)
	 (ninth ls) val (cddr (cddddr (cddddr ls)))))


;;; ================================================================================
;;; Functional deep update

(defun update (fn coll &rest keys)
  (declare (dynamic-extent fn))
  "Returns a new version of `coll' in which the element reached by doing chained
`lookup's on `keys' is updated by `fn'.  An example will help a lot here:
instead of writing

  (incf (@ (@ (@ foo 'a) 3) 7))

you can write, equivalently

  (setq foo (update #'1+ foo 'a 3 7))

This is perhaps most useful in contexts where you don't want to do the `setq'
anyway.  `fn' can be a function object, an fbound symbol, or a map."
  (labels ((rec (fn coll keys)
	     (if (null keys) (@ fn coll)
	       (with coll (car keys) (rec fn (lookup coll (car keys)) (cdr keys))))))
    (rec (if (symbolp fn) (coerce fn 'function) fn) coll keys)))

;;; If the `fn' is nontrivial, binds a variable to it with a `dynamic-extent' declaration.
;;; (Really, should do this for `image', `filter', etc. etc.)
;;; -- Actually, I'm not sure SBCL needs this.  It might be enough to add the declaration to
;;; the `defun' (as I've now done).  Also, it makes a bigger difference here than for `image'
;;; etc., because `fn' is called only once.
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

;;; I wanted to use `defconstant' for these, but the compiler tries to evaluate them at
;;; compile time, and `make-tree-set-org' doesn't yet exist.
(defparameter +fset-default-tree-set-org+ (make-tree-set-org 'compare #'compare))

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

(defparameter +empty-wb-set+ (make-wb-set nil))

(declaim (inline empty-set))
(defun empty-set ()
  "Returns an empty set of the default implementation."
  +empty-wb-set+)
;;; `fset2:empty-set' is below

(declaim (inline empty-wb-set fset2:empty-wb-set))
(defun empty-wb-set (&optional compare-fn-name)
  "Returns an empty wb-set.  By default, it will be ordered by `fset:compare';
to use a custom ordering, supply the comparison function name (a symbol) as
`compare-fn-name'."
  (if (null compare-fn-name)
      +empty-wb-set+
    (empty-wb-custom-set compare-fn-name)))
(defun fset2:empty-wb-set (&key compare-fn-name)
  "Returns an empty wb-set.  By default, it will be ordered by `fset:compare';
to use a custom ordering, supply the comparison function name (a symbol) as
`compare-fn-name'."
  (if (null compare-fn-name)
      +empty-wb-set+
    (empty-wb-custom-set compare-fn-name)))

;;; We cache the last empty `wb-custom-set' instance with a given org, so as to reuse them
;;; in the common case.
(deflex +empty-wb-custom-set-cache+ (make-hash-table :test 'eq))

;;; The goal here is to take the `symbol-function' of the comparison function exactly when
;;; the set is created.  Taking it earlier would mean we wouldn't notice if it were redefined;
;;; taking it later would break existing sets if the function were modified.
(defun empty-wb-custom-set (compare-fn-name)
  "Returns an empty `wb-set' ordered according to `compare-fn-name', which
must be a symbol."
  (assert (and compare-fn-name (symbolp compare-fn-name) (symbol-package compare-fn-name)) ()
	  "compare-fn-name must be a nonnull interned symbol")
  (if (eq compare-fn-name 'compare)
      +empty-wb-set+
    (let ((prev-instance (gethash compare-fn-name +empty-wb-custom-set-cache+))
	  (compare-fn (symbol-function compare-fn-name)))
      (if (and prev-instance
	       (eq compare-fn (wb-set-compare-fn prev-instance)))
	  prev-instance
	(setf (gethash compare-fn-name +empty-wb-custom-set-cache+)
	      (make-wb-custom-set nil (make-tree-set-org compare-fn-name compare-fn)))))))

(defgeneric empty-set-like (s)
  (:documentation
    "Returns an empty set of the same implementation, and using the same compare
or hash function, as `s'.  `s' can also be a bag."))

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

(define-wb-set-method empty? ((s wb-set))
  (null (contents s)))

(define-wb-set-method size ((s wb-set))
  (WB-Set-Tree-Size (contents s)))

(define-wb-set-method set-size ((s wb-set))
  (WB-Set-Tree-Size (contents s)))

(define-wb-set-method arb ((s wb-set))
  (let ((tree (contents s)))
    (if tree (values (WB-Set-Tree-Arb tree) t)
      (values nil nil))))

(define-wb-set-method contains? ((s wb-set) x &optional (y nil y?))
  (declare (ignore y))
  (check-two-arguments y? 'contains? 'wb-set)
  (WB-Set-Tree-Contains? (contents s) x (compare-fn s)))

;;; Note, first value is `t' or `nil'.
(define-wb-set-methods (lookup fset2:lookup) ((s wb-set) key)
  (WB-Set-Tree-Find-Equal (contents s) key (compare-fn s)))

(define-wb-set-method rank ((s wb-set) x)
  (let ((found? rank (WB-Set-Tree-Rank (contents s) x (compare-fn s))))
    (values (if found? rank (1- rank)) found?)))
(define-wb-set-method fset2:rank ((s wb-set) x)
  (let ((found? rank (WB-Set-Tree-Rank (contents s) x (compare-fn s))))
    (values rank found?)))

(define-wb-set-method at-rank ((s wb-set) rank)
  (let ((contents (contents s))
	((size (WB-Set-Tree-Size contents))))
    (unless (and (>= rank 0) (< rank size))
      (error 'simple-type-error :datum rank :expected-type `(integer 0 (,size))
	     :format-control "Rank ~D out of bounds on ~A"
	     :format-arguments (list rank s)))
    (WB-Set-Tree-Rank-Element contents rank)))

(defmethod index ((s wb-set) x)
  (let ((rank found? (rank s x)))
    (and found? rank)))

(defmethod at-index ((s wb-set) idx)
  (at-rank s idx))

(define-wb-set-method least ((s wb-set))
  (let ((tree (contents s)))
    (if tree (values (WB-Set-Tree-Least tree) t)
      (values nil nil))))

(define-wb-set-method greatest ((s wb-set))
  (let ((tree (contents s)))
    (if tree (values (WB-Set-Tree-Greatest tree) t)
        (values nil nil))))

(define-wb-set-method with ((s wb-set) value &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'with 'wb-set)
  (let ((contents (contents s))
	((new-contents (WB-Set-Tree-With contents value (compare-fn s)))))
    (if (eq new-contents contents)
	s
      (make s new-contents))))

(define-wb-set-method less ((s wb-set) value &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'wb-set)
  (let ((contents (contents s))
	((new-contents (WB-Set-Tree-Less contents value (compare-fn s)))))
    (if (eq new-contents contents)
	s
      (make s new-contents))))

(define-wb-set-method split-from ((s wb-set) value)
  (let ((new-contents (WB-Set-Tree-Split-Above (contents s) value (compare-fn s))))
    (make s (if (WB-Set-Tree-Contains? (contents s) value (compare-fn s))
		(WB-Set-Tree-With new-contents value (compare-fn s))
	      new-contents))))

(define-wb-set-method split-above ((s wb-set) value)
  (make s (WB-Set-Tree-Split-Above (contents s) value (compare-fn s))))

(define-wb-set-method split-through ((s wb-set) value)
  (let ((new-contents (WB-Set-Tree-Split-Below (contents s) value (compare-fn s))))
    (make s (if (WB-Set-Tree-Contains? (contents s) value (compare-fn s))
		(WB-Set-Tree-With new-contents value (compare-fn s))
	      new-contents))))

(define-wb-set-method split-below ((s wb-set) value)
  (make s (WB-Set-Tree-Split-Below (contents s) value (compare-fn s))))

(defmethod union ((s1 set) (s2 set) &key)
  "Fallback method for mixed implementations."
  (let ((result s1))
    (do-set (x s2)
      (includef result x))
    result))

(define-wb-set-method union ((s1 wb-set) (s2 wb-set) &key)
  (if-same-compare-fns (s1 s2)
      (make s1 (WB-Set-Tree-Union (contents s1) (contents s2) (compare-fn s1)))
    (call-next-method)))

(defmethod intersection ((s1 set) (s2 set) &key)
  "Fallback method for mixed implementations."
  (let ((result (empty-set-like s1)))
    (do-set (x s1)
      (when (contains? s2 x)
	(includef result x)))
    result))

(define-wb-set-method intersection ((s1 wb-set) (s2 wb-set) &key)
  (if-same-compare-fns (s1 s2)
      (make s1 (WB-Set-Tree-Intersect (contents s1) (contents s2) (compare-fn s1)))
    (call-next-method)))

(defmethod set-difference ((s1 set) (s2 set) &key)
  "Fallback method for mixed implementations."
  (let ((result s1))
    (do-set (x s2)
      (excludef result x))
    result))

(define-wb-set-method set-difference ((s1 wb-set) (s2 wb-set) &key)
  (if-same-compare-fns (s1 s2)
      (make s1 (WB-Set-Tree-Diff (contents s1) (contents s2) (compare-fn s1)))
    (call-next-method)))

;;; Intended for internal use by `complement-sets.lisp'.  This returns `s2 - s1', but the
;;; result is like `s1', not `s2'.
(defmethod set-difference-rev ((s1 set) (s2 set))
  "Fallback method for mixed implementations."
  (let ((result (empty-set-like s1)))
    (do-set (x s2)
      (unless (contains? s1 x)
	(includef result x)))
    result))

(define-wb-set-method set-difference-rev ((s1 wb-set) (s2 wb-set))
  (if-same-compare-fns (s1 s2)
      (make s1 (WB-Set-Tree-Diff (contents s2) (contents s1) (compare-fn s1)))
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

(define-wb-set-method set-difference-2 ((s1 wb-set) (s2 wb-set))
  (if-same-compare-fns (s1 s2)
      (let ((newc1 newc2 (WB-Set-Tree-Diff-2 (contents s1) (contents s2) (compare-fn s1))))
	(values (make s1 newc1) (make s2 newc2)))
    (call-next-method)))

(defmethod subset? ((s1 set) (s2 set))
  "Fallback method for mixed implementations."
  (do-set (x s1 t)
    (unless (contains? s2 x)
      (return nil))))

(define-wb-set-method subset? ((s1 wb-set) (s2 wb-set))
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

(define-wb-set-method disjoint? ((s1 wb-set) (s2 wb-set))
  (if-same-compare-fns (s1 s2)
      (WB-Set-Tree-Disjoint? (contents s1) (contents s2) (compare-fn s1))
    (call-next-method)))

(defmethod compare ((s1 wb-set) (s2 wb-set))
  ;; Just as comparing sets of different classes just compares the classes, so comparing WB-sets
  ;; with different compare-fns just compares the compare-fns.
  (let ((name-comp (compare (wb-set-compare-fn-name s1) (wb-set-compare-fn-name s2))))
    (ecase name-comp
      ((:less :greater)
	name-comp)
      (:equal
	;; Hoo boy.  The compare-fn has been redefined since one of the sets was created.
	;; Update them and try again.
	(compare (convert 'wb-set s1 :compare-fn-name (wb-set-compare-fn-name s1))
		 (convert 'wb-set s2 :compare-fn-name (wb-set-compare-fn-name s1))))
      (:unequal
	;; Shouldn't be possible, since we check the symbol-package in `empty-wb-custom-set'.
	(error "Can't compare wb-sets with uninterned compare-fn-names with same symbol-name")))))

;;; This may appear to replace the previous definition, but it does not, because it
;;; defines methods on (wb-default-set wb-default-set) and (wb-custom-set wb-custom-set).
;;; I.e., it doesn't cover the mixed cases.
(define-wb-set-method compare ((s1 wb-set) (s2 wb-set))
  (if-same-compare-fns (s1 s2)
      (WB-Set-Tree-Compare (contents s1) (contents s2) (compare-fn s1))
    (call-next-method)))

(define-wb-set-method hash-value ((s wb-set))
  ;; I expect this to be rarely used, so I don't want to make everybody pay the cost of a
  ;; cache slot (I already went to a lot of trouble not to make everybody pay the cost of
  ;; a compare-fn slot).  So instead, I bound the number of elements to hash ... let's say 32.
  (let ((result 0)
	(i 0)
	(mult 1))
    (do-wb-set-tree-members (x (contents s))
      (hash-mixf result (hash-multiply mult (hash-value-fixnum x)))
      (setq mult (hash-multiply mult 13))
      (when (= (incf i) 32)
	(return)))
    result))

(defgeneric internal-do-set (set elt-fn value-fn)
  (:documentation
    "Calls `elt-fn' on successive elements of the set; when done, calls `value-fn'
on no arguments and returns the result(s).  This is called by `do-set' to provide
for the possibility of different set implementations; it is not for public use.
`elt-fn' and `value-fn' must be function objects, not symbols."))

(define-wb-set-method internal-do-set ((s wb-set) elt-fn value-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  (Do-WB-Set-Tree-Members (x (contents s) (funcall value-fn))
    (funcall elt-fn x)))

(define-wb-set-method iterator ((s wb-set) &key)
  (Make-WB-Set-Tree-Iterator (contents s)))

(define-wb-set-method fun-iterator ((s wb-set) &key from-end?)
  (if from-end?
      (WB-Set-Tree-Rev-Fun-Iter (contents s))
    (WB-Set-Tree-Fun-Iter (contents s))))

(define-wb-set-methods (filter fset2:filter) ((pred function) (s wb-set))
  (make s (wb-set-filter pred (contents s) (compare-fn s))))

(define-wb-set-methods (filter fset2:filter) ((pred symbol) (s wb-set))
  (make s (wb-set-filter (coerce-to-function pred) (contents s) (compare-fn s))))

(define-wb-set-method filter ((pred map) (s wb-set))
  (make s (wb-set-filter #'(lambda (x) (lookup pred x)) (contents s) (compare-fn s))))
(define-wb-set-method fset2:filter ((pred map) (s wb-set))
  (make s (wb-set-filter #'(lambda (x) (fset2:lookup pred x)) (contents s) (compare-fn s))))

(defun wb-set-filter (pred contents compare-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function pred))
  (let ((result nil))
    (do-wb-set-tree-members (x contents)
      (when (funcall pred x)
	(setq result (WB-Set-Tree-With result x compare-fn))))
    result))

;;; A set is another kind of boolean-valued map.
(define-methods (filter fset2:filter) ((pred set) (s set))
  (intersection pred s))

;;; A bag is yet another kind of boolean-valued map.
(define-methods (filter fset2:filter) ((pred bag) (s set))
  (intersection pred s))

(define-wb-set-methods (partition fset2:partition) ((pred function) (s wb-set))
  (let ((res1 res2 (wb-set-partition pred (contents s) (compare-fn s))))
    (values (make s res1) (make s res2))))

(define-wb-set-methods (partition fset2:partition) ((pred symbol) (s wb-set))
  (let ((res1 res2 (wb-set-partition (coerce-to-function pred) (contents s) (compare-fn s))))
    (values (make s res1) (make s res2))))

(define-wb-set-method partition ((pred map) (s wb-set))
  (let ((res1 res2 (wb-set-partition #'(lambda (x) (lookup pred x)) (contents s) (compare-fn s))))
    (values (make s res1) (make s res2))))
(define-wb-set-method fset2:partition ((pred map) (s wb-set))
  (let ((res1 res2 (wb-set-partition #'(lambda (x) (fset2:lookup pred x)) (contents s) (compare-fn s))))
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

(define-wb-set-methods (image fset2:image) ((fn function) (s wb-set) &key compare-fn-name)
  (wb-set-image fn (contents s) (or compare-fn-name (compare-fn-name s))))

(define-wb-set-methods (image fset2:image) ((fn symbol) (s wb-set) &key compare-fn-name)
  (wb-set-image (coerce-to-function fn) (contents s)
		(or compare-fn-name (compare-fn-name s))))

(define-wb-set-method image ((fn map) (s wb-set) &key compare-fn-name)
  (wb-set-image (fn (x) (lookup fn x)) (contents s) (or compare-fn-name (compare-fn-name s))))
(define-wb-set-method fset2:image ((fn map) (s wb-set) &key compare-fn-name)
  (wb-set-image (fn (x) (fset2:lookup fn x)) (contents s) (or compare-fn-name (compare-fn-name s))))

(define-wb-set-methods (image fset2:image) ((fn set) (s wb-set) &key compare-fn-name)
  (wb-set-image (fn (x) (lookup fn x)) (contents s) (or compare-fn-name (compare-fn-name s))))

(define-wb-set-methods (image fset2:image) ((fn bag) (s wb-set) &key compare-fn-name)
  (wb-set-image (fn (x) (lookup fn x)) (contents s) (or compare-fn-name (compare-fn-name s))))

(defun wb-set-image (fn contents compare-fn-name)
  (declare (type function fn))
  (let ((org (wb-set-org (empty-wb-set compare-fn-name)))
	(result nil))
    (do-wb-set-tree-members (x contents)
      (setq result (WB-Set-Tree-With result (funcall fn x) (tree-set-org-compare-fn org))))
    (make-wb-set result org)))

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

(define-convert-methods (set fset2:set) ((s set) &key)
  s)

(defmethod convert ((to-type (eql 'wb-set)) (s set) &key compare-fn-name)
  (convert-to-wb-set s nil
    (let ((tree nil))
      (do-set (x s tree)
	(setq tree (WB-Set-Tree-With tree x compare-fn))))))

(define-wb-set-method convert ((to-type (eql 'wb-set)) (s wb-set) &key compare-fn-name)
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
  (let ((test default? (coerce-to-function-or-equal? test)))
    (if key
        (let ((key (coerce-to-function key)))
          (do-set (x s)
            (when (funcall test item (funcall key x))
              (return x))))
      (if (and default? (eq (compare-fn s) #'compare))
	  (nth-value 1 (lookup s item))
	(do-set (x s)
	  (when (funcall test item x)
	    (return x)))))))

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
      (if (and default? (eq (compare-fn s) #'compare))
          (if (contains? s item) 1 0)
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
      (pprint-newline ':fill stream)
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
      (pprint-newline ':fill stream)
      (write x :stream stream))))

(defmethod make-load-form ((s wb-default-set) &optional environment)
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

(defparameter +fset-default-hash-set-org+ (make-hash-set-org 'compare #'compare #'hash-value))

(declaim (inline make-ch-set))

(defstruct (ch-set
	     (:include set)
	     (:constructor make-ch-set (contents org))
	     (:predicate ch-set?)
	     (:print-function print-ch-set)
	     (:copier nil))
  (contents nil :read-only t)
  (org nil :type hash-set-org :read-only t))

(defparameter +empty-ch-set+ (make-ch-set nil +fset-default-hash-set-org+))

(declaim (inline fset2:empty-set))
(defun fset2:empty-set ()
  "Returns an empty set of the default implementation."
  +empty-ch-set+)

(declaim (inline empty-ch-set fset2:empty-ch-set))
(defun empty-ch-set (&optional compare-fn-name)
  "Returns an empty ch-set.  By default, it will use `fset:compare' to compare
values, and `fset:hash-value' to hash them; to use a custom comparison and hash,
use `define-hash-function' to associate them, and then supply the name of the
comparison function as `compare-fn-name'."
  (if (null compare-fn-name)
      +empty-ch-set+
    (empty-ch-custom-set compare-fn-name)))
(defun fset2:empty-ch-set (&key compare-fn-name)
  "Returns an empty ch-set.  By default, it will use `fset:compare' to compare
values, and `fset:hash-value' to hash them; to use a custom comparison and hash,
use `define-hash-function' to associate them, and then supply the name of the
comparison function as `compare-fn-name'."
  (if (null compare-fn-name)
      +empty-ch-set+
    (empty-ch-custom-set compare-fn-name)))

(deflex +empty-ch-custom-set-cache+ (make-hash-table :test 'eq))

(defun empty-ch-custom-set (compare-fn-name)
  (assert (and compare-fn-name (symbolp compare-fn-name) (symbol-package compare-fn-name)) ()
	  "compare-fn-name must be a nonnull interned symbol")
  (if (eq compare-fn-name 'compare)
      +empty-ch-set+
    (let ((prev-instance (gethash compare-fn-name +empty-ch-custom-set-cache+))
	  (compare-fn (symbol-function compare-fn-name))
	  (hash-fn-name (or (get compare-fn-name 'hash-function)
			    (error "compare-fn-name `~S' not defined for hashing -- see `define-hash-function'"
				   compare-fn-name)))
	  ((hash-fn (symbol-function hash-fn-name))))
      (if (and prev-instance
	       (let ((prev-org (ch-set-org prev-instance)))
		 (and (eq compare-fn (hash-set-org-compare-fn prev-org))
		      (eq hash-fn (hash-set-org-hash-fn prev-org)))))
	  prev-instance
	(setf (gethash compare-fn-name +empty-ch-custom-set-cache+)
	      (make-ch-set nil (make-hash-set-org compare-fn-name compare-fn hash-fn)))))))

(defmethod empty-set-like ((s ch-set))
  (empty-ch-set (hash-set-org-compare-fn-name (ch-set-org s))))

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

(define-methods (lookup fset2:lookup) ((s ch-set) key)
  (let ((hsorg (ch-set-org s)))
    (ch-set-tree-contains? (ch-set-contents s) key (hash-set-org-hash-fn hsorg)
			   (hash-set-org-compare-fn hsorg))))

(defmethod index ((s ch-set) x)
  (let ((hsorg (ch-set-org s)))
    (ch-set-tree-index (ch-set-contents s) x (hash-set-org-hash-fn hsorg)
		       (hash-set-org-compare-fn hsorg))))

(defmethod at-index ((s ch-set) index)
  (let ((contents (ch-set-contents s))
	((size (ch-set-tree-size contents))))
    (unless (and (>= index 0) (< index size))
      (error 'simple-type-error :datum index :expected-type `(integer 0 (,size))
				:format-control "Index ~D out of bounds on ~A"
				:format-arguments (list index s)))
    (ch-set-tree-index-element contents index)))

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

(defmethod set-difference ((s1 ch-set) (s2 ch-set) &key)
  (if-same-ch-set-orgs (s1 s2 hsorg)
      (make-ch-set (ch-set-tree-diff (ch-set-contents s1) (ch-set-contents s2)
				     (hash-set-org-hash-fn hsorg)
				     (hash-set-org-compare-fn hsorg))
		   hsorg)
    (call-next-method)))

(defmethod set-difference-rev ((s1 ch-set) (s2 ch-set))
  (if-same-ch-set-orgs (s1 s2 hsorg)
      (make-ch-set (ch-set-tree-diff (ch-set-contents s2) (ch-set-contents s1)
				     (hash-set-org-hash-fn hsorg)
				     (hash-set-org-compare-fn hsorg))
		   hsorg)
    (call-next-method)))

(defmethod set-difference-2 ((s1 ch-set) (s2 ch-set))
  (if-same-ch-set-orgs (s1 s2 hsorg)
      (let ((s1-diff-s2 s2-diff-s1 (ch-set-tree-diff-2 (ch-set-contents s1) (ch-set-contents s2)
						       (hash-set-org-hash-fn hsorg)
						       (hash-set-org-compare-fn hsorg))))
	(values (make-ch-set s1-diff-s2 hsorg) (make-ch-set s2-diff-s1 hsorg)))
    (call-next-method)))

(defmethod subset? ((s1 ch-set) (s2 ch-set))
  (if-same-ch-set-orgs (s1 s2 hsorg)
      (ch-set-tree-subset? (ch-set-contents s1) (ch-set-contents s2)
			   (hash-set-org-hash-fn hsorg) (hash-set-org-compare-fn hsorg))
    (call-next-method)))

(defmethod disjoint? ((s1 ch-set) (s2 ch-set))
  (if-same-ch-set-orgs (s1 s2 hsorg)
      (ch-set-tree-disjoint? (ch-set-contents s1) (ch-set-contents s2)
			     (hash-set-org-hash-fn hsorg) (hash-set-org-compare-fn hsorg))
    (call-next-method)))

(define-methods (partition fset2:partition) ((pred function) (s ch-set))
  (let ((hsorg (ch-set-org s))
	((res1 res2 (ch-set-partition pred (ch-set-contents s) hsorg))))
    (values (make-ch-set res1 hsorg) (make-ch-set res2 hsorg))))

(define-methods (partition fset2:partition) ((pred symbol) (s ch-set))
  (let ((hsorg (ch-set-org s))
	((res1 res2 (ch-set-partition (coerce-to-function pred) (ch-set-contents s) hsorg))))
    (values (make-ch-set res1 hsorg) (make-ch-set res2 hsorg))))

(defmethod partition ((pred map) (s ch-set))
  (let ((hsorg (ch-set-org s))
	((res1 res2 (ch-set-partition #'(lambda (x) (lookup pred x)) (ch-set-contents s) hsorg))))
    (values (make-ch-set res1 hsorg) (make-ch-set res2 hsorg))))
(defmethod fset2:partition ((pred map) (s ch-set))
  (let ((hsorg (ch-set-org s))
	((res1 res2 (ch-set-partition #'(lambda (x) (fset2:lookup pred x)) (ch-set-contents s) hsorg))))
    (values (make-ch-set res1 hsorg) (make-ch-set res2 hsorg))))

(defun ch-set-partition (pred contents hsorg)
  (declare (optimize (speed 3) (safety 0))
	   (type function pred))
  (let ((result-1 nil)
	(result-2 nil)
	(hash-fn (hash-set-org-hash-fn hsorg))
	(compare-fn (hash-set-org-compare-fn hsorg)))
    (do-ch-set-tree-members (x contents)
      (if (funcall pred x)
	  (setq result-1 (ch-set-tree-with result-1 x hash-fn compare-fn))
	(setq result-2 (ch-set-tree-with result-2 x hash-fn compare-fn))))
    (values result-1 result-2)))

(define-methods (filter fset2:filter) ((pred function) (s ch-set))
  (ch-set-filter pred s))

(define-methods (filter fset2:filter) ((pred symbol) (s ch-set))
  (ch-set-filter (coerce-to-function pred) s))

(defmethod filter ((pred map) (s ch-set))
  (ch-set-filter #'(lambda (x) (lookup pred x)) s))
(defmethod fset2:filter ((pred map) (s ch-set))
  (ch-set-filter #'(lambda (x) (fset2:lookup pred x)) s))

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

(define-methods (image fset2:image) ((fn function) (s ch-set) &key compare-fn-name)
  (ch-set-image fn s compare-fn-name))

(define-methods (image fset2:image) ((fn symbol) (s ch-set) &key compare-fn-name)
  (ch-set-image (coerce-to-function fn) s compare-fn-name))

(defmethod image ((fn map) (s ch-set) &key compare-fn-name)
  (ch-set-image (fn (x) (lookup fn x)) s compare-fn-name))
(defmethod fset2:image ((fn map) (s ch-set) &key compare-fn-name)
  (ch-set-image (fn (x) (fset2:lookup fn x)) s compare-fn-name))

(define-methods (image fset2:image) ((fn set) (s ch-set) &key compare-fn-name)
  (ch-set-image (fn (x) (lookup fn x)) s compare-fn-name))

(define-methods (image fset2:image) ((fn bag) (s ch-set) &key compare-fn-name)
  (ch-set-image (fn (x) (lookup fn x)) s compare-fn-name))

(defun ch-set-image (fn s compare-fn-name)
  (declare (type function fn))
  (let ((result nil)
	(hsorg (ch-set-org (if compare-fn-name (empty-ch-set compare-fn-name) s))))
    (do-ch-set-tree-members (x (ch-set-contents s))
      (setq result (ch-set-tree-with result (funcall fn x) (hash-set-org-hash-fn hsorg)
				     (hash-set-org-compare-fn hsorg))))
    (make-ch-set result hsorg)))

(defmethod compare ((s1 ch-set) (s2 ch-set))
  (if-same-ch-set-orgs (s1 s2 hsorg)
      (ch-set-tree-compare (ch-set-contents s1) (ch-set-contents s2)
			   (hash-set-org-compare-fn hsorg))
    ;; See `define-wb-set-method compare' above.
    (let ((s1-cfn-name (hash-set-org-compare-fn-name (ch-set-org s1)))
	  (s2-cfn-name (hash-set-org-compare-fn-name (ch-set-org s2)))
	  ((name-comp (compare s1-cfn-name s2-cfn-name))))
      (ecase name-comp
	((:less :greater)
	  name-comp)
	(:equal
	  (compare (convert 'ch-set s1 :compare-fn-name s1-cfn-name)
		   (convert 'ch-set s2 :compare-fn-name s1-cfn-name)))
	(:unequal
	  (error "Can't compare ch-sets with uninterned compare-fn-names with same symbol-name"))))))

(defmethod hash-value ((s ch-set))
  (ch-set-tree-hash-value (ch-set-contents s)))

(defmethod internal-do-set ((s ch-set) elt-fn value-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  (do-ch-set-tree-members (x (ch-set-contents s) (funcall value-fn))
    (funcall elt-fn x)))

(defmethod iterator ((s ch-set) &key)
  (make-ch-set-tree-iterator (ch-set-contents s)))

(defmethod fun-iterator ((s ch-set) &key from-end?)
  (if from-end?
      (ch-set-tree-rev-fun-iter (ch-set-contents s))
    (ch-set-tree-fun-iter (ch-set-contents s))))

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

(define-convert-methods (ch-set fset2:set) ((l list) &key compare-fn-name)
  (convert-to-ch-set l nil
    (let ((tree nil))
      (dolist (x l)
	(setq tree (ch-set-tree-with tree x hash-fn compare-fn)))
      tree)))

(define-convert-methods (ch-set fset2:set) ((s seq) &key compare-fn-name)
  (convert-to-ch-set s nil
    (let ((tree nil))
      (do-seq (x s)
	(setq tree (ch-set-tree-with tree x hash-fn compare-fn)))
      tree)))

(define-convert-methods (ch-set fset2:set) ((s sequence) &key compare-fn-name)
  (convert-to-ch-set s nil
    (let ((tree nil))
      (dotimes (i (length s))
	(setq tree (ch-set-tree-with tree (elt s i) hash-fn compare-fn)))
      tree)))

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
      (pprint-newline ':fill stream)
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

(defparameter +empty-wb-bag+ (make-wb-bag nil +fset-default-tree-set-org+))

(declaim (inline empty-bag))
(defun empty-bag ()
  "Returns an empty bag of the default implementation and type."
  +empty-wb-bag+)

(declaim (inline empty-wb-bag fset2:empty-wb-bag))
(defun empty-wb-bag (&optional compare-fn-name)
  "Returns an empty `wb-bag' ordered according to `compare-fn-name', which
must be a symbol."
  (if (null compare-fn-name)
      +empty-wb-bag+
    (empty-wb-custom-bag compare-fn-name)))
(defun fset2:empty-wb-bag (&key compare-fn-name)
  "Returns an empty `wb-bag' ordered according to `compare-fn-name', which
must be a symbol."
  (if (null compare-fn-name)
      +empty-wb-bag+
    (empty-wb-custom-bag compare-fn-name)))

(deflex +empty-wb-custom-bag-cache+ (make-hash-table :test 'eq))

(defun empty-wb-custom-bag (compare-fn-name)
  (assert (and compare-fn-name (symbolp compare-fn-name) (symbol-package compare-fn-name)) ()
	  "compare-fn-name must be a nonnull interned symbol")
  (if (eq compare-fn-name 'compare)
      +empty-wb-bag+
    (let ((prev-instance (gethash compare-fn-name +empty-wb-custom-bag-cache+))
	  (compare-fn (symbol-function compare-fn-name)))
      (if (and prev-instance
	       (eq compare-fn (tree-set-org-compare-fn (wb-bag-org prev-instance))))
	  prev-instance
	(setf (gethash compare-fn-name +empty-wb-custom-bag-cache+)
	      (make-wb-bag nil (make-tree-set-org compare-fn-name compare-fn)))))))

(defmethod empty-set-like ((b wb-bag))
  (let ((tsorg (wb-bag-org b)))
    (empty-wb-custom-set (tree-set-org-compare-fn-name tsorg))))

(defgeneric empty-bag-like (b)
  (:documentation
    "Returns an empty bag of the same implementation, and using the same compare
or hash function, as `b'.  `b' can also be a set."))

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
(defmethod fset2:lookup ((b wb-bag) x)
  (WB-Bag-Tree-Multiplicity (wb-bag-contents b) x
			    (tree-set-org-compare-fn (wb-bag-org b))))

(defmethod rank ((b wb-bag) x)
  "Returns the rank in the set ordering, i.e. the upper bound is the `set-size'."
  (let ((found? rank (WB-Bag-Tree-Rank (wb-bag-contents b) x (tree-set-org-compare-fn (wb-bag-org b)))))
    (values (if found? rank (1- rank)) found?)))
(defmethod fset2:rank ((b wb-bag) x)
  "Returns the rank in the set ordering, i.e. the upper bound is the `set-size'."
  (let ((found? rank (WB-Bag-Tree-Rank (wb-bag-contents b) x (tree-set-org-compare-fn (wb-bag-org b)))))
    (values rank found?)))

(defmethod at-rank ((s wb-bag) rank)
  "Takes the rank in the set ordering, i.e. the upper bound is the `set-size'."
  (let ((contents (wb-bag-contents s))
	((size (WB-Bag-Tree-Size contents))))
    (unless (and (>= rank 0) (< rank size))
      (error 'simple-type-error :datum rank :expected-type `(integer 0 (,size))
	     :format-control "Rank ~D out of bounds on ~A"
	     :format-arguments (list rank s)))
    (WB-Bag-Tree-Rank-Pair contents rank)))

(defmethod index ((b wb-bag) x)
  (let ((rank found? (rank b x)))
    (and found? rank)))

(defmethod at-index ((b wb-bag) idx)
  (at-rank b idx))

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

(defmethod split-from ((b wb-bag) value)
  (let ((compare-fn (tree-set-org-compare-fn (wb-bag-org b)))
	((new-contents (WB-Bag-Tree-Split-Above (wb-bag-contents b) value compare-fn))
	 (count (WB-Bag-Tree-Multiplicity (wb-bag-contents b) value compare-fn))))
    (make-wb-bag (if (plusp count)
		     (WB-Bag-Tree-With new-contents value compare-fn count)
		   new-contents)
		 (wb-bag-org b))))

(defmethod split-above ((b wb-bag) value)
  (make-wb-bag (WB-Bag-Tree-Split-Above (wb-bag-contents b) value (tree-set-org-compare-fn (wb-bag-org b)))
	       (wb-bag-org b)))

(defmethod split-through ((b wb-bag) value)
  (let ((compare-fn (tree-set-org-compare-fn (wb-bag-org b)))
	((new-contents (WB-Bag-Tree-Split-Below (wb-bag-contents b) value compare-fn))
	 (count (WB-Bag-Tree-Multiplicity (wb-bag-contents b) value compare-fn))))
    (make-wb-bag (if (plusp count)
		     (WB-Bag-Tree-With new-contents value compare-fn count)
		   new-contents)
		 (wb-bag-org b))))

(defmethod split-below ((b wb-bag) value)
  (make-wb-bag (WB-Bag-Tree-Split-Below (wb-bag-contents b) value (tree-set-org-compare-fn (wb-bag-org b)))
	       (wb-bag-org b)))

(defmethod union ((b1 bag) (b2 bag) &key)
  "Fallback method for mixed implementations."
  (let ((result b1))
    (do-bag-pairs (x2 n2 b2)
      (let ((n1 x1 (multiplicity result x2)))
	(cond ((zerop n1)
	       (includef result x2 n2))
	      ((< n1 n2)
	       (includef result x1 (- n2 n1))))))
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
	(make-wb-bag (WB-Bag-Tree-Union (wb-bag-contents b) (WB-Set-Tree-To-Bag-Tree (wb-set-contents s))
					bcmp)
		     (wb-bag-org b))
      (call-next-method))))

(defmethod union ((s set) (b bag) &key)
  "Fallback method for mixed implementations."
  (let ((result (convert 'bag s)))
    (do-bag-pairs (xb n b)
      (let ((xs? xs (lookup s xb)))
	(cond ((not xs?)
	       (includef result xb n))
	      ((> n 1)
	       (includef result xs (1- n))))))
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
    (do-bag-pairs (x2 n2 b2)
      (let ((n1 x1 (multiplicity b1 x2)))
	(if (zerop n1)
	    (includef result x2 n2)
	  (includef result x1 n2))))
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
      (let ((nb xb (multiplicity b x)))
	(includef result (if (zerop nb) x xb))))
    result))

(defmethod bag-sum ((b wb-bag) (s wb-set))
  (let ((scmp (wb-set-compare-fn s))
	(bcmp (tree-set-org-compare-fn (wb-bag-org b))))
    (if (eq scmp bcmp)
	(make-wb-bag (WB-Bag-Tree-Sum (wb-bag-contents b)
				      (WB-Set-Tree-To-Bag-Tree (wb-set-contents s)) bcmp)
		     (wb-bag-org b))
      (call-next-method))))

(defmethod bag-sum ((s set) (b bag))
  "Fallback method for mixed implementations."
  (let ((result (convert 'bag s)))
    (do-bag-pairs (x n b)
      (let ((xs? xs (lookup s x)))
	(if xs? (includef result xs n)
	  (includef result x n))))
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
    (do-bag-pairs (x1 n1 b1)
      (includef result x1 (min (multiplicity b2 x1) n1)))
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
    (do-set (x s)
      (when (contains? b x)
	(includef result x)))
    result))

(defmethod intersection ((s wb-set) (b wb-bag) &key)
  (let ((scmp (wb-set-compare-fn s))
	(bcmp (tree-set-org-compare-fn (wb-bag-org b))))
    (if (eq scmp bcmp)
	(make-wb-set (WB-Set-Tree-Intersect (wb-set-contents s) (WB-Bag-Tree-To-Set-Tree (wb-bag-contents b))
					    bcmp)
		     (wb-set-org s))
      (call-next-method))))

(defmethod bag-product ((b1 bag) (b2 bag))
  "Fallback method for mixed implementations."
  (let ((result (empty-bag-like b1)))
    (do-bag-pairs (x2 n2 b2)
      (let ((n1 x1 (multiplicity b1 x2)))
	(unless (zerop n1)
	  (includef result x1 (* n1 n2)))))
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
	(make-wb-bag (WB-Bag-Tree-Product (wb-bag-contents b) (WB-Set-Tree-To-Bag-Tree (wb-set-contents s))
					  bcmp)
		     (wb-bag-org b))
      (call-next-method))))

(defmethod bag-product ((s set) (b bag))
  "Fallback method for mixed implementations."
  (let ((result (convert 'bag (empty-set-like s))))
    (do-bag-pairs (xb n b)
      (let ((xs? xs (lookup s xb)))
	(when xs?
	  (includef result xs n))))
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
  (if-same-wb-bag-orgs (b1 b2 tsorg)
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

(defmethod disjoint? ((b wb-bag) (s ch-set))
  (bag-set-disjoint? b s))

(defmethod disjoint? ((s wb-set) (b wb-bag))
  (bag-set-disjoint? b s))
(defmethod disjoint? ((s ch-set) (b wb-bag))
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

(defmethod compare ((b1 wb-bag) (b2 wb-bag))
  (if-same-wb-bag-orgs (b1 b2 tsorg)
      (WB-Bag-Tree-Compare (wb-bag-contents b1) (wb-bag-contents b2) (tree-set-org-compare-fn tsorg))
    ;; See `define-wb-set-method compare' above.
    (let ((b1-cfn-name (tree-set-org-compare-fn-name (wb-bag-org b1)))
	  (b2-cfn-name (tree-set-org-compare-fn-name (wb-bag-org b2)))
	  ((name-comp (compare b1-cfn-name b2-cfn-name))))
      (ecase name-comp
	((:less :greater)
	  name-comp)
	(:equal
	  (compare (convert 'wb-bag b1 :compare-fn-name b1-cfn-name)
		   (convert 'wb-bag b2 :compare-fn-name b1-cfn-name)))
	(:unequal
	  (error "Can't compare wb-bags with uninterned compare-fn-names with same symbol-name"))))))

(defmethod hash-value ((b wb-bag))
  ;; I expect this to be rarely used, so I don't want to make everybody pay the cost of a
  ;; cache slot.  So instead, I bound the number of elements to hash ... let's say 32.
  (let ((result 0)
	(i 0)
	(mult 1))
    (do-wb-bag-tree-pairs (x n (wb-bag-contents b))
      (hash-mixf result (hash-multiply (hash-multiply n mult) (hash-value-fixnum x)))
      (setq mult (hash-multiply mult 13))
      (when (= (incf i) 32)
	(return)))
    result))

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

(define-methods (filter fset2:filter) ((pred function) (b bag))
  (bag-filter pred b))

(define-methods (filter fset2:filter) ((pred symbol) (b bag))
  (bag-filter (coerce-to-function pred) b))

(defmethod filter ((pred map) (b bag))
  (bag-filter (fn (x) (lookup pred x)) b))
(defmethod fset2:filter ((pred map) (b bag))
  (bag-filter (fn (x) (fset2:lookup pred x)) b))

(defun bag-filter (pred b)
  (let ((result (empty-bag-like b)))
    (do-bag-pairs (x n b)
      (when (funcall pred x)
	(setq result (with result x n))))
    result))

(define-methods (filter fset2:filter) ((pred set) (b bag))
  (bag-product (convert 'bag pred) b))

(define-methods (filter fset2:filter) ((pred bag) (b bag))
  ;; Not quite the same as bag intersection.
  (bag-filter (fn (x) (contains? pred x)) b))

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

(define-methods (image fset2:image) ((fn function) (b bag) &key compare-fn-name)
  (bag-image fn b compare-fn-name))

(define-methods (image fset2:image) ((fn symbol) (b bag) &key compare-fn-name)
  (bag-image (coerce-to-function fn) b compare-fn-name))

(defmethod image ((fn map) (b bag) &key compare-fn-name)
  (bag-image (fn (x) (lookup fn x)) b compare-fn-name))
(defmethod fset2:image ((fn map) (b bag) &key compare-fn-name)
  (bag-image (fn (x) (fset2:lookup fn x)) b compare-fn-name))

(define-methods (image fset2:image) ((fn set) (b bag) &key compare-fn-name)
  (bag-image (fn (x) (lookup fn x)) b compare-fn-name))

(define-methods (image fset2:image) ((fn bag) (b bag) &key compare-fn-name)
  (bag-image (fn (x) (lookup fn x)) b compare-fn-name))

(defun bag-image (fn b compare-fn-name)
  (declare (type function fn))
  (let ((org (wb-bag-org (if compare-fn-name (empty-wb-bag compare-fn-name) b)))
	(result nil))
    (do-bag-pairs (x n b)
      (setq result (WB-Bag-Tree-With result (funcall fn x) (tree-set-org-compare-fn org) n)))
    (make-wb-bag result org)))

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

(define-convert-methods (set fset2:set) ((b wb-bag) &key)
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
  (let ((test default? (coerce-to-function-or-equal? test)))
    (if key
        (let ((key (coerce-to-function key)))
          (do-bag-pairs (x n b nil)
            (declare (ignore n))
            (when (funcall test item (funcall key x))
              (return x))))
        (if (and default? (eq (compare-fn b) #'compare))
            (nth-value 1 (lookup b item))
          (do-bag-pairs (x n b nil)
	    (declare (ignore n))
	    (when (funcall test item x)
	      (return x)))))))

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
        (if (and default? (eq (compare-fn b) #'compare))
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
        (pprint-newline ':fill stream)
        (incf i)
        (if (> n 1)
	    ;; There might be a bag entry for 'quote or 'function...
	    (let (#+sbcl (sb-pretty:*pprint-quote-with-syntactic-sugar* nil))
	      (princ "#%" stream)
	      (write `(,x ,n) :stream stream))
          (write x :stream stream))))))

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

(defparameter +fset-default-tree-map-org+ (make-tree-map-org 'compare #'compare 'compare #'compare))

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

(defparameter +empty-wb-map+ (make-wb-map nil +fset-default-tree-map-org+ nil))

(defparameter +empty-wb-map/no-default+ (make-wb-map nil +fset-default-tree-map-org+ 'no-default))

(declaim (inline fset2-default))
(defun fset2-default (default? default no-default? &optional default-default)
  (if (and default? no-default?)
      (error "Both a default and `no-default?' specified")
    (cond (default? default)
	  (no-default? 'no-default)
	  (t default-default))))

(declaim (inline empty-map))
(defun empty-map (&optional default)
  "Returns an empty map of the default implementation."
  (if default (make-wb-map nil +fset-default-tree-map-org+ default)
    +empty-wb-map+))
;;; `fset2:empty-map' is below

(declaim (inline empty-wb-map fset2:empty-wb-map))
(defun empty-wb-map (&optional default key-compare-fn-name val-compare-fn-name)
  "Returns an empty wb-map with the specified default and comparison functions."
  (if (and (null key-compare-fn-name) (null val-compare-fn-name))
      (if (null default)
	  +empty-wb-map+
	(make-wb-map nil +fset-default-tree-map-org+ default))
    (empty-wb-custom-map default (or key-compare-fn-name 'compare) (or val-compare-fn-name 'compare))))
(defun fset2:empty-wb-map (&key (default nil default?) no-default? key-compare-fn-name val-compare-fn-name)
  "Returns an empty wb-map with the specified default and comparison functions.
The map's default is `nil' unless a different default is supplied, or
`no-default?' is true."
  ;; The idea here is to inline the keyword argument processing and the default selection, hopefully
  ;; all of which will constant-fold down to just the default in almost all cases.
  (empty-wb-map-internal (fset2-default default? default no-default?) key-compare-fn-name val-compare-fn-name))

(defun empty-wb-map-internal (default key-compare-fn-name val-compare-fn-name)
  ;; This has gotten too big to inline.
  (cond ((or key-compare-fn-name val-compare-fn-name)
	 (empty-wb-custom-map default (or key-compare-fn-name 'compare) (or val-compare-fn-name 'compare)))
	((null default)
	 +empty-wb-map+)
	((eq default 'no-default)
	 +empty-wb-map/no-default+)
	(t (make-wb-map nil +fset-default-tree-map-org+ default))))

(deflex +empty-wb-custom-map-cache+ (make-hash-table :test 'equal))

(defun empty-wb-custom-map (default key-compare-fn-name val-compare-fn-name)
  (assert (and key-compare-fn-name (symbolp key-compare-fn-name)
	       (symbol-package key-compare-fn-name))
	  () "key-compare-fn-name must be a nonnull interned symbol")
  (assert (and val-compare-fn-name (symbolp val-compare-fn-name)
	       (symbol-package val-compare-fn-name))
	  () "val-compare-fn-name must be a nonnull interned symbol")
  (if (and (eq key-compare-fn-name 'compare) (eq val-compare-fn-name 'compare))
      (if (null default) +empty-wb-map+
	(make-wb-map nil +fset-default-tree-map-org+ default))
    ;; &&& This caches one default per type.  We could use a two-level map to cache multiple defaults,
    ;; but the inner maps would have to be custom wb-maps, whose creation couldn't call this function (!).
    ;; Alternatively, the cached default could always be nil, as it is for the default org.
    (let ((cache-key (list key-compare-fn-name val-compare-fn-name))
	  ((prev-instance (gethash cache-key +empty-wb-custom-map-cache+)))
	  (key-compare-fn (symbol-function key-compare-fn-name))
	  (val-compare-fn (symbol-function val-compare-fn-name)))
      (if (and prev-instance
	       (let ((prev-org (wb-map-org prev-instance)))
		 (and (eq key-compare-fn (tree-map-org-key-compare-fn prev-org))
		      (eq val-compare-fn (tree-map-org-val-compare-fn prev-org)))))
	  (if (equal?-cmp default (map-default prev-instance) val-compare-fn)
	      prev-instance
	    (setf (gethash cache-key +empty-wb-custom-map-cache+)
		  (make-wb-map nil (wb-map-org prev-instance) default)))
	(setf (gethash cache-key +empty-wb-custom-map-cache+)
	      (make-wb-map nil (make-tree-map-org key-compare-fn-name key-compare-fn
						  val-compare-fn-name val-compare-fn)
			   default))))))

(defgeneric empty-map-like (m)
  (:documentation
    "Returns an empty map of the same implementation, and using the same compare
or hash functions, as `m'."))

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
  (let ((dflt (map-default m)))
    (if (eq dflt 'no-default) (values nil nil)
      (values dflt t))))

(defmethod with-default ((m wb-map) new-default)
  (make-wb-map (wb-map-contents m) (wb-map-org m) new-default))

(defmethod fset2:without-default ((m wb-map))
  (make-wb-map (wb-map-contents m) (wb-map-org m) 'no-default))

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

(define-condition fset2:lookup-error (error)
    ())

(define-condition fset2:map-domain-error (fset2:lookup-error)
    ((map :initarg :map :reader fset2:map-domain-error-map)
     (key :initarg :key :reader fset2:map-domain-error-key))
  (:report (lambda (mde stream)
	     (let ((*print-length* 8)
		   (*print-level* 3))
	       (format stream "Key ~S not found in map ~A, which has no default"
		       (fset2:map-domain-error-key mde) (fset2:map-domain-error-map mde))))))

;;; Even though FSet 1 code will never generate a map with no default, we could have a mixed
;;; FSet 1/2 codebase, so we should still check for that in `fset:lookup'.
(define-methods (lookup fset2:lookup) ((m wb-map) key)
  (let ((val? val mkey (WB-Map-Tree-Lookup (wb-map-contents m) key
					   (tree-map-org-key-compare-fn (wb-map-org m)))))
    ;; Our internal convention is the reverse of the external one.
    (values (if val? val
	      (let ((dflt (map-default m)))
		(if (eq dflt 'no-default)
		    (error 'fset2:map-domain-error :map m :key key)
		  dflt)))
	    val? mkey)))

(defmethod rank ((m wb-map) x)
  (let ((found? rank (WB-Map-Tree-Rank (wb-map-contents m) x
				       (tree-map-org-key-compare-fn (wb-map-org m)))))
    (values (if found? rank (1- rank)) found?)))
(defmethod fset2:rank ((m wb-map) x)
  (let ((found? rank (WB-Map-Tree-Rank (wb-map-contents m) x
				       (tree-map-org-key-compare-fn (wb-map-org m)))))
    (values rank found?)))

(defmethod at-rank ((m wb-map) rank)
  (let ((contents (wb-map-contents m))
	((size (WB-Map-Tree-Size contents))))
    (unless (and (>= rank 0) (< rank size))
      (error 'simple-type-error :datum rank :expected-type `(integer 0 (,size))
	     :format-control "Rank ~D out of bounds on ~A"
	     :format-arguments (list rank m)))
    (WB-Map-Tree-Rank-Pair contents rank)))

(defmethod index ((m wb-map) x)
  (let ((rank found? (rank m x)))
    (and found? rank)))

(defmethod at-index ((m wb-map) idx)
  (at-rank m idx))

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
	((new-contents range-val (WB-Map-Tree-Less contents key (tree-map-org-key-compare-fn tmorg)))))
    (if (eq new-contents contents)
	m
      (values (make-wb-map new-contents tmorg (map-default m)) range-val))))

(defmethod split-from ((m wb-map) key)
  (let ((tmorg (wb-map-org m))
	((key-compare-fn (tree-map-org-key-compare-fn tmorg))
	 ((new-contents (WB-Map-Tree-Split-Above (wb-map-contents m) key key-compare-fn))
	  (val? val (WB-Map-Tree-Lookup (wb-map-contents m) key key-compare-fn)))))
    (make-wb-map (if val? (WB-Map-Tree-With new-contents key val key-compare-fn (tree-map-org-val-compare-fn tmorg))
		   new-contents)
		 tmorg (map-default m))))

(defmethod split-above ((m wb-map) key)
  (make-wb-map (WB-Map-Tree-Split-Above (wb-map-contents m) key (tree-map-org-key-compare-fn (wb-map-org m)))
	       (wb-map-org m) (map-default m)))

(defmethod split-through ((m wb-map) key)
  (let ((tmorg (wb-map-org m))
	((key-compare-fn (tree-map-org-key-compare-fn tmorg))
	 ((new-contents (WB-Map-Tree-Split-Below (wb-map-contents m) key key-compare-fn))
	  (val? val (WB-Map-Tree-Lookup (wb-map-contents m) key key-compare-fn)))))
    (make-wb-map (if val? (WB-Map-Tree-With new-contents key val key-compare-fn (tree-map-org-val-compare-fn tmorg))
		   new-contents)
		 tmorg (map-default m))))

(defmethod split-below ((m wb-map) key)
  (make-wb-map (WB-Map-Tree-Split-Below (wb-map-contents m) key (tree-map-org-key-compare-fn (wb-map-org m)))
	       (wb-map-org m) (map-default m)))

(defmethod domain ((m wb-map))
  (let ((tmorg (wb-map-org m)))
    (make-wb-set (WB-Map-Tree-Domain (wb-map-contents m))
		 ;; We don't go through `empty-wb-set' here, because that would retrieve the current
		 ;; `symbol-function' of the `key-compare-fn-name', which might have been changed since
		 ;; the map was created.
		 (make-tree-set-org (tree-map-org-key-compare-fn-name tmorg)
				    (tree-map-org-key-compare-fn tmorg)))))

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
    ;; See `define-wb-set-method compare' above.
    (let ((m1-kcfn-name (tree-map-org-key-compare-fn-name (wb-map-org map1)))
	  (m1-vcfn-name (tree-map-org-val-compare-fn-name (wb-map-org map1)))
	  (m2-kcfn-name (tree-map-org-key-compare-fn-name (wb-map-org map2)))
	  (m2-vcfn-name (tree-map-org-val-compare-fn-name (wb-map-org map2)))
	  ((name-comp (compare (list m1-kcfn-name m1-vcfn-name) (list m2-kcfn-name m2-vcfn-name)))))
      (ecase name-comp
	((:less :greater)
	  name-comp)
	(:equal
	  (compare (convert 'wb-map map1 :key-compare-fn-name m1-kcfn-name :val-compare-fn-name m1-vcfn-name)
		   (convert 'wb-map map2 :key-compare-fn-name m1-kcfn-name :val-compare-fn-name m1-vcfn-name)))
	(:unequal
	  (error "Can't compare wb-maps with uninterned compare-fn-names with same symbol-name"))))))

(defmethod hash-value ((m wb-map))
  ;; I expect this to be rarely used, so I don't want to make everybody pay the cost of a
  ;; cache slot.  So instead, I bound the number of pairs to hash ... let's say 32.
  (let ((result 0)
	(i 0)
	(mult 1))
    (do-wb-map-tree-pairs (k v (wb-map-contents m))
      (hash-mixf result (hash-multiply mult (hash-mix (hash-value-fixnum k) (hash-value-fixnum v))))
      (setq mult (hash-multiply mult 13))
      (when (= (incf i) 32)
	(return)))
    result))

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
  (Do-WB-Map-Tree-Pairs (x y (wb-map-contents m) (funcall value-fn))
    (funcall elt-fn x y)))

(defmethod iterator ((m wb-map) &key)
  (Make-WB-Map-Tree-Iterator (wb-map-contents m)))

(defmethod fun-iterator ((s wb-map) &key from-end?)
  (if from-end?
      (WB-Map-Tree-Rev-Fun-Iter (wb-map-contents s))
    (WB-Map-Tree-Fun-Iter (wb-map-contents s))))

(define-methods (filter fset2:filter) ((pred function) (m wb-map))
  (wb-map-filter pred m))

(define-methods (filter fset2:filter) ((pred symbol) (m wb-map))
  (wb-map-filter (coerce-to-function pred) m))

(defun wb-map-filter (pred m)
  (declare (optimize (speed 3) (safety 0)) (type function pred))
  (let ((tmorg (wb-map-org m))
	((key-compare-fn (tree-map-org-key-compare-fn tmorg))
	 (val-compare-fn (tree-map-org-val-compare-fn tmorg)))
	(result nil))
    (do-map (x y m)
      (when (funcall pred x y)
	(setq result (WB-Map-Tree-With result x y key-compare-fn val-compare-fn))))
    (make-wb-map result tmorg (map-default m))))

(defmethod image ((fn function) (m wb-map) &key key-compare-fn-name val-compare-fn-name)
  (wb-map-image fn m key-compare-fn-name val-compare-fn-name (map-default m)))
(defmethod map-image ((fn function) (m wb-map)
		      &key key-compare-fn-name val-compare-fn-name (default nil default?) no-default?)
  (wb-map-image fn m key-compare-fn-name val-compare-fn-name (fset2-default default? default no-default?)))

(defmethod image ((fn symbol) (m wb-map) &key key-compare-fn-name val-compare-fn-name)
  (wb-map-image (coerce-to-function fn) m key-compare-fn-name val-compare-fn-name (map-default m)))
(defmethod map-image ((fn symbol) (m wb-map)
		      &key key-compare-fn-name val-compare-fn-name (default nil default?) no-default?)
  (wb-map-image (coerce-to-function fn) m key-compare-fn-name val-compare-fn-name
		(fset2-default default? default no-default?)))

(defun wb-map-image (fn m key-compare-fn-name val-compare-fn-name default)
  (declare (type function fn))
  (let ((m-org (wb-map-org m))
	((res-org (wb-map-org (empty-wb-map nil (or key-compare-fn-name (tree-map-org-key-compare-fn-name m-org))
					    (or val-compare-fn-name (tree-map-org-val-compare-fn-name m-org))))))
	(result nil))
    (do-wb-map-tree-pairs (x y (wb-map-contents m))
      (let ((new-x new-y (funcall fn x y)))
	(setq result (WB-Map-Tree-With result new-x new-y (tree-map-org-key-compare-fn res-org)
				       (tree-map-org-val-compare-fn res-org)))))
    (make-wb-map result res-org default)))

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
  (generic-map-union m1 m2 val-fn (let ((dflt1 (map-default m1))
					(dflt2 (map-default m2)))
				    (and (or dflt1 dflt2) (funcall val-fn dflt1 dflt2)))))
(defmethod fset2:map-union ((m1 map) (m2 map) &optional (val-fn (fn (_v1 v2) v2)))
  "Fallback method for mixed implementations."
  (generic-map-union m1 m2 val-fn (map-union-default m1 m2 val-fn)))

(defun generic-map-union (m1 m2 val-fn default)
  (let ((result m1)
	(vcf1 (val-compare-fn m1))
	(m1d (with-default m1 nil))) ; so `lookup' doesn't error
    (do-map (k2 v2 m2)
      (let ((v1 v1? k1 (lookup m1d k2)))
	(if (not v1?)
	    (setf (lookup result k2) v2)
	  (unless (equal?-cmp v1 v2 vcf1)
	    (let ((new-v second-val (funcall val-fn v1 v2)))
	      (if (eq second-val ':no-value)
		  (excludef result k1)
		(setf (lookup result k1) new-v)))))))
    (with-default result default)))

(defun map-union-default (m1 m2 val-fn)
  (let ((dflt1 (map-default m1))
	(dflt2 (map-default m2)))
    (if (eq dflt1 'no-default) dflt2
      (if (eq dflt2 'no-default) dflt1
	(let ((v second-val (funcall val-fn dflt1 dflt2)))
	  (if (eq second-val ':no-value) 'no-default
	    v))))))

(defmethod map-union ((m1 wb-map) (m2 wb-map)
		      &optional (val-fn (fn (_v1 v2) v2)))
  (if-same-wb-map-orgs (m1 m2 tmorg)
      (make-wb-map (WB-Map-Tree-Union (wb-map-contents m1) (wb-map-contents m2)
				      (coerce val-fn 'function) (tree-map-org-key-compare-fn tmorg)
				      (tree-map-org-val-compare-fn tmorg))
		   tmorg (let ((dflt1 (map-default m1))
			       (dflt2 (map-default m2)))
			   (and (or dflt1 dflt2) (funcall val-fn dflt1 dflt2))))
    (call-next-method)))
(defmethod fset2:map-union ((m1 wb-map) (m2 wb-map)
			    &optional (val-fn (fn (_v1 v2) v2)))
  (if-same-wb-map-orgs (m1 m2 tmorg)
      (make-wb-map (WB-Map-Tree-Union (wb-map-contents m1) (wb-map-contents m2)
				      (coerce val-fn 'function) (tree-map-org-key-compare-fn tmorg)
				      (tree-map-org-val-compare-fn tmorg))
		   tmorg (map-union-default m1 m2 val-fn))
    (call-next-method)))

(defmethod map-intersection ((m1 map) (m2 map) &optional (val-fn (fn (_v1 v2) v2)))
  "Fallback method for mixed implementations."
  (generic-map-intersection m1 m2 val-fn (let ((dflt1 (map-default m1))
					       (dflt2 (map-default m2)))
					   (and (or dflt1 dflt2) (funcall val-fn dflt1 dflt2)))))
(defmethod fset2:map-intersection ((m1 map) (m2 map) &optional (val-fn (fn (_v1 v2) v2)))
  "Fallback method for mixed implementations."
  (generic-map-intersection m1 m2 val-fn (map-intersection-default m1 m2 val-fn)))

(defun generic-map-intersection (m1 m2 val-fn default)
  (let ((result (empty-map-like m1))
	(vcf1 (val-compare-fn m1))
	(m2d (with-default m2 nil))) ; prevent `lookup' errors
    (do-map (k v1 m1)
      (let ((v2 v2? (lookup m2d k)))
	(when v2?
	  (if (equal?-cmp v1 v2 vcf1)
	      (setf (lookup result k) v1)
	    (let ((new-v second-val (funcall val-fn v1 v2)))
	      (unless (eq second-val ':no-value)
		(setf (lookup result k) new-v)))))))
    (with-default result default)))

(defun map-intersection-default (m1 m2 val-fn)
  (let ((dflt1 (map-default m1))
	(dflt2 (map-default m2)))
    (if (and (not (eq dflt1 'no-default)) (not (eq dflt2 'no-default)))
	(let ((v second-val (funcall val-fn dflt1 dflt2)))
	  (if (eq second-val ':no-value) 'no-default
	    v))
      'no-default)))

(defmethod map-intersection ((m1 wb-map) (m2 wb-map)
			     &optional (val-fn (fn (_v1 v2) v2)))
  (if-same-wb-map-orgs (m1 m2 tmorg)
      (make-wb-map (WB-Map-Tree-Intersect (wb-map-contents m1) (wb-map-contents m2)
					  (coerce val-fn 'function) (tree-map-org-key-compare-fn tmorg)
					  (tree-map-org-val-compare-fn tmorg))
		   tmorg (let ((dflt1 (map-default m1))
			       (dflt2 (map-default m2)))
			   (and (or dflt1 dflt2) (funcall val-fn dflt1 dflt2))))
    (call-next-method)))
(defmethod fset2:map-intersection ((m1 wb-map) (m2 wb-map)
				   &optional (val-fn (fn (_v1 v2) v2)))
  (if-same-wb-map-orgs (m1 m2 tmorg)
      (make-wb-map (WB-Map-Tree-Intersect (wb-map-contents m1) (wb-map-contents m2)
					  (coerce val-fn 'function) (tree-map-org-key-compare-fn tmorg)
					  (tree-map-org-val-compare-fn tmorg))
		   tmorg (map-intersection-default m1 m2 val-fn))
    (call-next-method)))

(defmethod map-difference-2 ((m1 map) (m2 map))
  "Fallback method for mixed implementations."
  (let ((result1 result2 (generic-map-difference-2 m1 m2)))
    (values (with-default result1 (map-default m1))
	    (with-default result2 (map-default m2)))))
(defmethod fset2:map-difference-2 ((m1 map) (m2 map))
  "Fallback method for mixed implementations."
  (let ((result1 result2 (generic-map-difference-2 m1 m2))
	(dflt1 dflt2 (map-difference-2-defaults m1 m2)))
    (values (with-default result1 dflt1)
	    (with-default result2 dflt2))))

(defun generic-map-difference-2 (m1 m2)
  (let ((result1 (empty-map-like m1))
	(result2 (empty-map-like m2))
	(vcf1 (val-compare-fn m1))
	(vcf2 (val-compare-fn m2))
	(m1d (with-default m1 nil))
	(m2d (with-default m2 nil)))
    (do-map (k v1 m1)
      (let ((v2 v2? (lookup m2d k)))
	(unless (and v2? (equal?-cmp v1 v2 vcf1))
	  (setf (lookup result1 k) v1))))
    (do-map (k v2 m2)
      (let ((v1 v1? (lookup m1d k)))
	(unless (and v1? (equal?-cmp v1 v2 vcf2))
	  (setf (lookup result2 k) v2))))
    (values result1 result2)))

(defun map-difference-2-defaults (m1 m2)
  (let ((dflt1 (map-default m1))
	(dflt2 (map-default m2)))
    (values (if (eq dflt1 'no-default) 'no-default
	      (if (eq dflt2 'no-default) dflt1
		(if (equal?-cmp dflt1 dflt2 (val-compare-fn m1))
		    'no-default
		  dflt1)))
	    (if (eq dflt2 'no-default) 'no-default
	      (if (eq dflt1 'no-default) dflt2
		(if (equal?-cmp dflt1 dflt2 (val-compare-fn m2))
		    'no-default
		  dflt2))))))

(defmethod map-difference-2 ((m1 wb-map) (m2 wb-map))
  (if-same-wb-map-orgs (m1 m2 tmorg)
      (let ((newc1 newc2 (WB-Map-Tree-Diff-2 (wb-map-contents m1) (wb-map-contents m2)
					     (tree-map-org-key-compare-fn tmorg)
					     (tree-map-org-val-compare-fn tmorg))))
	(values (make-wb-map newc1 tmorg (map-default m1))
		(make-wb-map newc2 tmorg (map-default m2))))
    (call-next-method)))
(defmethod fset2:map-difference-2 ((m1 wb-map) (m2 wb-map))
  (if-same-wb-map-orgs (m1 m2 tmorg)
      (let ((newc1 newc2 (WB-Map-Tree-Diff-2 (wb-map-contents m1) (wb-map-contents m2)
					     (tree-map-org-key-compare-fn tmorg)
					     (tree-map-org-val-compare-fn tmorg)))
	    (dflt1 dflt2 (map-difference-2-defaults m1 m2)))
	(values (make-wb-map newc1 tmorg dflt1)
		(make-wb-map newc2 tmorg dflt2)))
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

(define-methods (compose fset2:compose) ((f1 function) (f2 function) &key)
  (compose-functions f1 f2))

(define-methods (compose fset2:compose) ((f1 symbol) (f2 function) &key)
  (compose-functions (symbol-function f1) f2))

(define-methods (compose fset2:compose) ((f1 function) (f2 symbol) &key)
  (compose-functions f1 (symbol-function f2)))

(define-methods (compose fset2:compose) ((f1 symbol) (f2 symbol) &key)
  (compose-functions (symbol-function f1) (symbol-function f2)))

;;; I've removed the fallback method for `compose', as it's not needed -- the maps
;;; don't need to match anyway.

(defmethod compose ((map1 wb-map) (map2 map) &key val-compare-fn-name)
  "The returned map's `val-compare-fn-name' can be specified; it defaults
to `compare'."
  (if (and (empty? map1) (eq (map-default map1) 'no-default))
      map1
    (make-wb-map (WB-Map-Tree-Compose (wb-map-contents map1)
				      ;; This can fail if `map2' is an FSet 2 map with no default.
				      (fn (x) (lookup map2 x)))
		 (wb-map-org (empty-wb-map nil (key-compare-fn-name map1) val-compare-fn-name))
		 (let ((new-default new-default? (lookup map2 (map-default map1))))
		   (if new-default? new-default (map-default map2))))))
(defmethod fset2:compose ((map1 wb-map) (map2 map) &key val-compare-fn-name)
  "The returned map's `val-compare-fn-name' can be specified; it defaults
to `compare'."
  (if (and (empty? map1) (eq (map-default map1) 'no-default))
      map1
    (make-wb-map (WB-Map-Tree-Compose (wb-map-contents map1) (fn (x) (fset2:lookup map2 x)))
		 (wb-map-org (empty-wb-map nil (key-compare-fn-name map1) val-compare-fn-name))
		 (let ((dflt1 (map-default map1)))
		   (if (eq dflt1 'no-default) 'no-default
		     (let ((new-default new-default? (fset2:lookup map2 dflt1)))
		       (if new-default? new-default (map-default map2))))))))

(define-methods (compose fset2:compose) ((m wb-map) (fn function) &key val-compare-fn-name)
  (wb-map-fn-compose m fn val-compare-fn-name))

(define-methods (compose fset2:compose) ((m wb-map) (fn symbol) &key val-compare-fn-name)
  (wb-map-fn-compose m (symbol-function fn) val-compare-fn-name))

(define-methods (compose fset2:compose) ((m wb-map) (s seq) &key val-compare-fn-name)
  (wb-map-fn-compose m (fn (x) (lookup s x)) val-compare-fn-name))

(defun wb-map-fn-compose (m fn val-compare-fn-name)
  (declare (type function fn))
  (make-wb-map (WB-Map-Tree-Compose (wb-map-contents m) fn)
	       (wb-map-org (empty-wb-map nil (key-compare-fn-name m) val-compare-fn-name))
	       (let ((dflt1 (map-default m)))
		 (if (eq dflt1 'no-default) 'no-default
		   (funcall fn dflt1)))))

(define-convert-methods (map fset2:map) ((m map) &key)
  m)

(define-convert-methods (wb-map fset2:wb-map)
			((m wb-map) &key key-compare-fn-name val-compare-fn-name (default (map-default m)))
  (convert-to-wb-map m default
      (let ((m-tmorg (wb-map-org m)))
	(and (or (eq m-tmorg tmorg)
		 (and (eq key-compare-fn (tree-map-org-key-compare-fn m-tmorg))
		      (eq val-compare-fn (tree-map-org-val-compare-fn m-tmorg))))
	     (equal?-cmp default (map-default m) val-compare-fn)))
    (let ((tree nil))
      (Do-WB-Map-Tree-Pairs (k v (wb-map-contents m) tree)
	(setq tree (WB-Map-Tree-With tree k v key-compare-fn val-compare-fn))))))

(define-convert-methods (wb-map fset2:wb-map)
			((m map) &key key-compare-fn-name val-compare-fn-name (default (map-default m)))
  (convert-to-wb-map m default nil
    (let ((tree nil))
      (do-map (k v m tree)
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
(defmethod convert ((to-type (eql 'fset2:set)) (m map) &key (pair-fn #'cons))
  (let ((result nil))
    (do-map (key val m)
      (setq result (ch-set-tree-with result (funcall pair-fn key val) #'hash-value #'compare)))
    (make-ch-set result +fset-default-hash-set-org+)))

;;; &&& Plist support?
(define-convert-methods (wb-map fset2:wb-map)
			((l list) &key (key-fn #'car) (value-fn #'cdr) input-sorted?
				       key-compare-fn-name val-compare-fn-name default)
  (wb-map-from-sequence l key-fn value-fn input-sorted? key-compare-fn-name val-compare-fn-name default))

(defmethod convert ((to-type (eql 'map)) (s seq)
		    &key (key-fn #'car) (value-fn #'cdr) input-sorted? (default (seq-default s)))
  (wb-map-from-sequence s key-fn value-fn input-sorted? nil nil default))

(define-convert-methods (wb-map fset2:wb-map)
			((s seq) &key (key-fn #'car) (value-fn #'cdr) input-sorted?
			 key-compare-fn-name val-compare-fn-name (default (and (eq to-type 'wb-map) (seq-default s))))
  (wb-map-from-sequence s key-fn value-fn input-sorted? key-compare-fn-name val-compare-fn-name default))

(defmethod convert ((to-type (eql 'map)) (s sequence)
		    &key (key-fn #'car) (value-fn #'cdr) input-sorted? default)
  (wb-map-from-sequence s key-fn value-fn input-sorted? nil nil default))

(define-convert-methods (wb-map fset2:wb-map)
			((s sequence) &key (key-fn #'car) (value-fn #'cdr) input-sorted?
					   key-compare-fn-name val-compare-fn-name default)
  (wb-map-from-sequence s key-fn value-fn input-sorted? key-compare-fn-name val-compare-fn-name default))

(defun wb-map-from-sequence (s key-fn value-fn input-sorted?
			     &optional key-compare-fn-name val-compare-fn-name default)
  (convert-to-wb-map s default nil
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

(define-convert-methods (map fset2:map) ((b bag) &key default)
  (convert 'wb-map b :default default))

(define-convert-methods (wb-map fset2:wb-map) ((b bag) &key key-compare-fn-name val-compare-fn-name default)
  ;; &&& If desired, we can easily make a very fast version of this -- all it has to do is
  ;; build new interior nodes, reusing the leaf vectors.  (But only if the compare-fns match.)
  (convert-to-wb-map b default nil
    (let ((tree nil))
      (do-bag-pairs (x n b tree)
	(setq tree (WB-Map-Tree-With tree x n key-compare-fn val-compare-fn))))))

(defmethod convert ((to-type (eql 'map)) (ht hash-table) &key default)
  (convert 'wb-map ht :default default))

(define-convert-methods (wb-map fset2:wb-map) ((ht hash-table) &key key-compare-fn-name val-compare-fn-name default)
  (convert-to-wb-map ht default nil
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
  (let ((test default? (coerce-to-function-or-equal? test)))
    (if key
        (let ((key (coerce-to-function key)))
          (do-map (x y m nil)
            (when (funcall test item (funcall key x))
              (return (values x y)))))
        (if (and default? (eq (key-compare-fn m) #'compare))
            (let ((val val? (lookup m item)))
              (if val? (values item val)
                (values nil nil)))
          (do-map (x y m nil)
	    (when (funcall test item x)
	      (return (values x y))))))))

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
        (if (and default? (eq (key-compare-fn m) #'compare))
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
  (pprint-logical-block
      (stream nil :prefix "#{|"
		  :suffix (let ((tmorg (wb-map-org map))
				((key-cf-name (tree-map-org-key-compare-fn-name tmorg))
				 (val-cf-name (tree-map-org-val-compare-fn-name tmorg))
				 ((key-default? (eq key-cf-name 'compare))
				  (val-default? (eq val-cf-name 'compare))))
				(dflt (map-default map)))
			    (format nil " |}~:[[~:[~S~;~*~];~:[~S~;~*~]]~;~4*~]~:[~;/~:[~S~;[no default]~]~]"
				    (and key-default? val-default?)
				    key-default? key-cf-name val-default? val-cf-name
				    dflt (eq dflt 'no-default) dflt)))
    (do-map (x y map)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline ':fill stream)
      ;; There might be a map entry for 'quote or 'function...
      (let (#+sbcl (sb-pretty:*pprint-quote-with-syntactic-sugar* nil))
	(write (list x y) :stream stream)))))

(defmethod make-load-form ((m wb-map) &optional environment)
  (declare (ignore environment))
  `(with-default (convert 'wb-map ',(convert 'list m)
			  :key-compare-fn-name ',(tree-map-org-key-compare-fn-name (wb-map-org m))
			  :val-compare-fn-name ',(tree-map-org-val-compare-fn-name (wb-map-org m)))
		 ',(map-default m)))


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

(defparameter +fset-default-hash-map-org+
    (make-hash-map-org 'compare #'compare #'hash-value 'compare #'compare #'hash-value))

(declaim (inline make-ch-map))

(defstruct (ch-map
	     (:include map)
	     (:constructor make-ch-map (contents org default))
	     (:predicate ch-map?)
	     (:print-function print-ch-map)
	     (:copier nil))
  (contents nil :read-only t)
  (org nil :type hash-map-org :read-only t))

(defparameter +empty-ch-map+ (make-ch-map nil +fset-default-hash-map-org+ nil))

(defparameter +empty-ch-map/no-default+ (make-ch-map nil +fset-default-hash-map-org+ 'no-default))

(declaim (inline fset2:empty-map))
(defun fset2:empty-map (&key (default nil default?) no-default?)
  "Returns an empty map of the default implementation, with the specified
default or lack of default."
  (empty-ch-map-internal (fset2-default default? default no-default?) nil nil))

(declaim (inline empty-ch-map fset2:empty-ch-map))
(defun empty-ch-map (&optional default key-compare-fn-name val-compare-fn-name)
  (if (and (null key-compare-fn-name) (null val-compare-fn-name))
      (if (null default)
	  +empty-ch-map+
	(make-ch-map nil +fset-default-hash-map-org+ default))
    (empty-ch-custom-map default (or key-compare-fn-name 'compare) (or val-compare-fn-name 'compare))))
(defun fset2:empty-ch-map (&key (default nil default?) no-default? key-compare-fn-name val-compare-fn-name)
  "Returns an empty ch-map with the specified default and comparison functions.
The map's default is `nil' unless a different default is supplied, or
`no-default?' is true."
  (empty-ch-map-internal (fset2-default default? default no-default?) key-compare-fn-name val-compare-fn-name))

(defun empty-ch-map-internal (default key-compare-fn-name val-compare-fn-name)
  (cond ((or key-compare-fn-name val-compare-fn-name)
	 (empty-ch-custom-map default (or key-compare-fn-name 'compare) (or val-compare-fn-name 'compare)))
	((null default)
	 +empty-ch-map+)
	((eq default 'no-default)
	 +empty-ch-map/no-default+)
	(t (make-ch-map nil +fset-default-hash-map-org+ default))))

(deflex +empty-ch-custom-map-cache+ (make-hash-table :test 'equal))

(defun empty-ch-custom-map (default key-compare-fn-name val-compare-fn-name)
  (assert (and key-compare-fn-name (symbolp key-compare-fn-name)
	       (symbol-package key-compare-fn-name))
	  () "key-compare-fn-name must be a nonnull interned symbol")
  (assert (and val-compare-fn-name (symbolp val-compare-fn-name)
	       (symbol-package val-compare-fn-name))
	  () "val-compare-fn-name must be a nonnull interned symbol")
  (if (and (eq key-compare-fn-name 'compare) (eq val-compare-fn-name 'compare))
      (if (null default) +empty-ch-map+
	(make-ch-map nil +fset-default-hash-map-org+ default))
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
		      (eq val-hash-fn (hash-map-org-val-hash-fn prev-org)))))
	  (if (equal?-cmp default (map-default prev-instance) val-compare-fn)
	      prev-instance
	    (setf (gethash cache-key +empty-ch-custom-map-cache+)
		  (make-ch-map nil (ch-map-org prev-instance) default)))
	(setf (gethash cache-key +empty-ch-custom-map-cache+)
	      (make-ch-map nil (make-hash-map-org key-compare-fn-name key-compare-fn key-hash-fn
						  val-compare-fn-name val-compare-fn val-hash-fn)
			   default))))))

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

(defmethod key-hash-fn ((m ch-map))
  (hash-map-org-key-hash-fn (ch-map-org m)))

(defmethod val-hash-fn ((m ch-map))
  (hash-map-org-val-hash-fn (ch-map-org m)))

(defmethod with-default ((m ch-map) new-default)
  (make-ch-map (ch-map-contents m) (ch-map-org m) new-default))

(defmethod fset2:without-default ((m ch-map))
  (make-ch-map (ch-map-contents m) (ch-map-org m) 'no-default))

(defmethod empty? ((m ch-map))
  (null (ch-map-contents m)))

(defmethod arb ((m ch-map))
  (let ((tree (ch-map-contents m)))
    (if tree
	(let ((key val (ch-map-tree-arb-pair tree)))
	  (values key val t))
      (values nil nil nil))))

(defmethod size ((m ch-map))
  (ch-map-tree-size (ch-map-contents m)))

(defmethod contains? ((m ch-map) x &optional (y nil y?))
  (check-three-arguments y? 'contains? 'wb-map)
  (let ((hmorg (ch-map-org m))
	((val? val (ch-map-tree-lookup (ch-map-contents m) x
				       (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)))))
    (and val? (equal?-cmp val y (hash-map-org-val-compare-fn hmorg)))))

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
	((new-contents range-val
	   (ch-map-tree-less contents key (hash-map-org-key-hash-fn hmorg)
			     (hash-map-org-key-compare-fn hmorg) (hash-map-org-val-hash-fn hmorg)))))
    (if (eq new-contents contents)
	m
      (values (make-ch-map new-contents hmorg (map-default m)) range-val))))

(define-methods (lookup fset2:lookup) ((m ch-map) key)
  (let ((hmorg (ch-map-org m))
	((val? val mkey (ch-map-tree-lookup (ch-map-contents m) key
					    (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)))))
    ;; Our internal convention is the reverse of the external one.
    (values (if val? val
	      (let ((dflt (map-default m)))
		(if (eq dflt 'no-default)
		    (error 'fset2:map-domain-error :map m :key key)
		  dflt)))
	    val? mkey)))

(defmethod index ((m ch-map) key)
  (let ((hmorg (ch-map-org m)))
    (ch-map-tree-index (ch-map-contents m) key (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg))))

(defmethod at-index ((m ch-map) index)
  (let ((contents (ch-map-contents m))
	((size (ch-map-tree-size contents))))
    (unless (and (>= index 0) (< index size))
      (error 'simple-type-error :datum index :expected-type `(integer 0 (,size))
				:format-control "Index ~D out of bounds on ~A"
				:format-arguments (list index m)))
    (ch-map-tree-index-pair contents index)))

(defmethod domain ((m ch-map))
  (let ((hmorg (ch-map-org m))
	((set-prototype (empty-ch-set (hash-map-org-key-compare-fn-name hmorg)))))
    (make-ch-set (ch-map-tree-domain (ch-map-contents m) (hash-map-org-key-hash-fn hmorg))
		 (ch-set-org set-prototype))))

(defmethod range ((m ch-map))
  (let ((hmorg (ch-map-org m))
	((set-prototype (empty-ch-set (hash-map-org-val-compare-fn-name hmorg)))
	 (val-hash-fn (hash-map-org-val-hash-fn hmorg))
	 (val-compare-fn (hash-map-org-val-compare-fn hmorg)))
	(result nil))
    (do-map (key val m)
      (declare (ignore key))
      (setq result (ch-set-tree-with result val val-hash-fn val-compare-fn)))
    (make-ch-set result (ch-set-org set-prototype))))

(defmethod domain-contains? ((m ch-map) x)
  (let ((hmorg (ch-map-org m)))
    (ch-map-tree-lookup (ch-map-contents m) x (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg))))

(defmethod compare ((map1 ch-map) (map2 ch-map))
  (if-same-ch-map-orgs (map1 map2 hmorg)
      (let ((val-compare-fn (hash-map-org-val-compare-fn hmorg))
	    ((comp (ch-map-tree-compare (ch-map-contents map1) (ch-map-contents map2)
					(hash-map-org-key-compare-fn hmorg) (hash-map-org-val-hash-fn hmorg)
					val-compare-fn))))
	(if (member comp '(:less :greater))
	    comp
	  (let ((def-comp (funcall val-compare-fn (map-default map1) (map-default map2))))
	    (if (member def-comp '(:less :greater))
		def-comp
	      (if (or (eq comp ':unequal) (eq def-comp ':unequal))
		  ':unequal
		':equal)))))
    ;; See `define-wb-set-method compare' above.
    (let ((m1-kcfn-name (hash-map-org-key-compare-fn-name (ch-map-org map1)))
	  (m1-vcfn-name (hash-map-org-val-compare-fn-name (ch-map-org map1)))
	  (m2-kcfn-name (hash-map-org-key-compare-fn-name (ch-map-org map2)))
	  (m2-vcfn-name (hash-map-org-val-compare-fn-name (ch-map-org map2)))
	  ((name-comp (compare (list m1-kcfn-name m1-vcfn-name) (list m2-kcfn-name m2-vcfn-name)))))
      (ecase name-comp
	((:less :greater)
	  name-comp)
	(:equal
	  (compare (convert 'wb-map map1 :key-compare-fn-name m1-kcfn-name :val-compare-fn-name m1-vcfn-name)
		   (convert 'wb-map map2 :key-compare-fn-name m1-kcfn-name :val-compare-fn-name m1-vcfn-name)))
	(:unequal
	  (error "Can't compare wb-maps with uninterned compare-fn-names with same symbol-name"))))))

(defmethod hash-value ((m ch-map))
  (let ((tree (ch-map-contents m)))
    (hash-mix (ch-map-tree-key-hash tree) (ch-map-tree-value-hash tree (hash-map-org-val-hash-fn (ch-map-org m))))))

(defmethod map-union ((m1 ch-map) (m2 ch-map) &optional (val-fn (fn (_v1 v2) v2)))
  (if-same-ch-map-orgs (m1 m2 hmorg)
      (make-ch-map (ch-map-tree-union (ch-map-contents m1) (ch-map-contents m2)
				      (coerce val-fn 'function)
				      (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)
				      (hash-map-org-val-hash-fn hmorg) (hash-map-org-val-compare-fn hmorg))
		   hmorg (let ((dflt1 (map-default m1))
			       (dflt2 (map-default m2)))
			   (and (or dflt1 dflt2) (funcall val-fn dflt1 dflt2))))
    (call-next-method)))
(defmethod fset2:map-union ((m1 ch-map) (m2 ch-map) &optional (val-fn (fn (_v1 v2) v2)))
  (if-same-ch-map-orgs (m1 m2 hmorg)
      (make-ch-map (ch-map-tree-union (ch-map-contents m1) (ch-map-contents m2)
				      (coerce val-fn 'function)
				      (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)
				      (hash-map-org-val-hash-fn hmorg) (hash-map-org-val-compare-fn hmorg))
		   hmorg (map-union-default m1 m2 val-fn))
    (call-next-method)))

(defmethod map-intersection ((m1 ch-map) (m2 ch-map) &optional (val-fn (fn (_v1 v2) v2)))
  (if-same-ch-map-orgs (m1 m2 hmorg)
      (make-ch-map (ch-map-tree-intersection (ch-map-contents m1) (ch-map-contents m2)
					     (coerce val-fn 'function)
					     (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)
					     (hash-map-org-val-hash-fn hmorg) (hash-map-org-val-compare-fn hmorg))
		   hmorg (let ((def1 (map-default m1))
			       (def2 (map-default m2)))
			   (and (or def1 def2) (funcall val-fn def1 def2))))
    (call-next-method)))
(defmethod fset2:map-intersection ((m1 ch-map) (m2 ch-map) &optional (val-fn (fn (_v1 v2) v2)))
  (if-same-ch-map-orgs (m1 m2 hmorg)
      (make-ch-map (ch-map-tree-intersection (ch-map-contents m1) (ch-map-contents m2)
					     (coerce val-fn 'function)
					     (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)
					     (hash-map-org-val-hash-fn hmorg) (hash-map-org-val-compare-fn hmorg))
		   hmorg (map-intersection-default m1 m2 val-fn))
    (call-next-method)))

(defmethod map-difference-2 ((m1 ch-map) (m2 ch-map))
  (if-same-ch-map-orgs (m1 m2 hmorg)
      (let ((newc1 newc2 (ch-map-tree-diff-2 (ch-map-contents m1) (ch-map-contents m2)
					     (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)
					     (hash-map-org-val-hash-fn hmorg) (hash-map-org-val-compare-fn hmorg))))
	(values (make-ch-map newc1 hmorg (map-default m1))
		(make-ch-map newc2 hmorg (map-default m2))))
    (call-next-method)))
(defmethod fset2:map-difference-2 ((m1 ch-map) (m2 ch-map))
  (if-same-ch-map-orgs (m1 m2 hmorg)
      (let ((newc1 newc2 (ch-map-tree-diff-2 (ch-map-contents m1) (ch-map-contents m2)
					     (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)
					     (hash-map-org-val-hash-fn hmorg) (hash-map-org-val-compare-fn hmorg)))
	    (dflt1 dflt2 (map-difference-2-defaults m1 m2)))
	(values (make-ch-map newc1 hmorg dflt1)
		(make-ch-map newc2 hmorg dflt2)))
    (call-next-method)))

(defmethod restrict ((m ch-map) (s ch-set))
  (let ((hmorg (ch-map-org m))
	(hsorg (ch-set-org s)))
    (if (eq (hash-map-org-key-compare-fn hmorg) (hash-set-org-compare-fn hsorg))
	(make-ch-map (ch-map-tree-restrict (ch-map-contents m) (ch-set-contents s)
					   (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)
					   (hash-map-org-val-hash-fn hmorg))
		     hmorg (map-default m))
      (call-next-method))))

(defmethod restrict-not ((m ch-map) (s ch-set))
  (let ((hmorg (ch-map-org m))
	(hsorg (ch-set-org s)))
    (if (eq (hash-map-org-key-compare-fn hmorg) (hash-set-org-compare-fn hsorg))
	(make-ch-map (ch-map-tree-restrict-not (ch-map-contents m) (ch-set-contents s)
					       (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)
					       (hash-map-org-val-hash-fn hmorg))
		     hmorg (map-default m))
      (call-next-method))))

(defmethod compose ((map1 ch-map) (map2 map) &key val-compare-fn-name)
  (if (and (empty? map1) (eq (map-default map1) 'no-default))
      map1
    (let ((prototype (empty-ch-map nil (key-compare-fn-name map1) val-compare-fn-name)))
      (make-ch-map (ch-map-tree-compose (ch-map-contents map1)
					;; This can fail if `map2' is an FSet 2 map with no default.
					(fn (x) (lookup map2 x)))
		   (ch-map-org prototype)
		   (let ((new-default? new-default
			   (lookup map2 (map-default map1))))
		     (if new-default? new-default (map-default map2)))))))
(defmethod fset2:compose ((map1 ch-map) (map2 map) &key val-compare-fn-name)
  (if (and (empty? map1) (eq (map-default map1) 'no-default))
      map1
    (let ((prototype (empty-ch-map nil (key-compare-fn-name map1) val-compare-fn-name)))
      (make-ch-map (ch-map-tree-compose (ch-map-contents map1) (fn (x) (lookup map2 x)))
		   (ch-map-org prototype)
		   (let ((dflt1 (map-default map1)))
		     (if (eq dflt1 'no-default) 'no-default
		       (let ((new-default new-default? (fset2:lookup map2 dflt1)))
			 (if new-default? new-default (map-default map2)))))))))

(define-methods (compose fset2:compose) ((m ch-map) (fn function) &key val-compare-fn-name)
  (ch-map-fn-compose m fn val-compare-fn-name))

(define-methods (compose fset2:compose) ((m ch-map) (fn symbol) &key val-compare-fn-name)
  (ch-map-fn-compose m (symbol-function fn) val-compare-fn-name))

(define-methods (compose fset2:compose) ((m ch-map) (s seq) &key val-compare-fn-name)
  (ch-map-fn-compose m (fn (x) (lookup s x)) val-compare-fn-name))

(defun ch-map-fn-compose (m fn val-compare-fn-name)
  (declare (type function fn))
  (let ((prototype (empty-ch-map nil (key-compare-fn-name m) val-compare-fn-name)))
    (make-ch-map (ch-map-tree-compose (ch-map-contents m) fn)
		 (ch-map-org prototype)
		 (let ((dflt1 (map-default m)))
		   (if (eq dflt1 'no-default) 'no-default
		     (funcall fn dflt1))))))

(defmethod internal-do-map ((m ch-map) elt-fn value-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  (do-ch-map-tree-pairs (x y (ch-map-contents m) (funcall value-fn))
    (funcall elt-fn x y)))

(define-methods (filter fset2:filter) ((pred function) (m ch-map))
  (ch-map-filter pred m))

(define-methods (filter fset2:filter) ((pred symbol) (m ch-map))
  (ch-map-filter (coerce-to-function pred) m))

(defun ch-map-filter (pred m)
  (declare (optimize (speed 3) (safety 0)) (type function pred))
  (let* ((result nil)
         (hmorg (ch-map-org m))
         (key-hash-fn (hash-map-org-key-hash-fn hmorg))
         (key-compare-fn (hash-map-org-key-compare-fn hmorg))
         (val-hash-fn (hash-map-org-val-hash-fn hmorg))
         (val-compare-fn (hash-map-org-val-compare-fn hmorg)))
    (do-ch-map-tree-pairs (k v (ch-map-contents m))
      (when (funcall pred k v)
	(setq result
              (ch-map-tree-with result k v key-hash-fn key-compare-fn val-hash-fn val-compare-fn))))
    (make-ch-map result hmorg (ch-map-default m))))

(defmethod image ((fn function) (m ch-map) &key key-compare-fn-name val-compare-fn-name)
  (ch-map-image fn m key-compare-fn-name val-compare-fn-name (map-default m)))
(defmethod map-image ((fn function) (m ch-map)
		      &key key-compare-fn-name val-compare-fn-name (default nil default?) no-default?)
  (ch-map-image fn m key-compare-fn-name val-compare-fn-name (fset2-default default? default no-default?)))

(defmethod image ((fn symbol) (m ch-map) &key key-compare-fn-name val-compare-fn-name)
  (ch-map-image (coerce-to-function fn) m key-compare-fn-name val-compare-fn-name (map-default m)))
(defmethod map-image ((fn symbol) (m ch-map)
		      &key key-compare-fn-name val-compare-fn-name (default nil default?) no-default?)
  (ch-map-image (coerce-to-function fn) m key-compare-fn-name val-compare-fn-name
		(fset2-default default? default no-default?)))

(defun ch-map-image (fn m key-compare-fn-name val-compare-fn-name default)
  (declare (type function fn))
  (let ((m-org (ch-map-org m))
	((res-org (ch-map-org (empty-ch-map nil (or key-compare-fn-name (hash-map-org-key-compare-fn-name m-org))
					    (or val-compare-fn-name (hash-map-org-val-compare-fn-name m-org)))))
	 ((key-hash-fn (hash-map-org-key-hash-fn res-org))
          (key-compare-fn (hash-map-org-key-compare-fn res-org))
          (val-hash-fn (hash-map-org-val-hash-fn res-org))
          (val-compare-fn (hash-map-org-val-compare-fn res-org))))
	(result nil))
    (do-ch-map-tree-pairs (x y (ch-map-contents m))
      (let ((new-x new-y (funcall fn x y)))
	(setq result (ch-map-tree-with result new-x new-y key-hash-fn key-compare-fn val-hash-fn val-compare-fn))))
    (make-ch-map result res-org default)))

(defmethod iterator ((m ch-map) &key)
  (make-ch-map-tree-iterator (ch-map-contents m)))

(defmethod fun-iterator ((s ch-map) &key from-end?)
  (if from-end?
      (ch-map-tree-rev-fun-iter (ch-map-contents s))
    (ch-map-tree-fun-iter (ch-map-contents s))))

(define-convert-methods (ch-map fset2:ch-map)
			((m map) &key key-compare-fn-name val-compare-fn-name default)
  (convert-to-ch-map m default nil
    (let ((tree nil))
      (do-map (k v m)
	(setq tree (ch-map-tree-with tree k v key-hash-fn key-compare-fn val-hash-fn val-compare-fn)))
      tree)))

(define-convert-methods (ch-map fset2:ch-map)
			((m ch-map) &key key-compare-fn-name val-compare-fn-name default)
  "The result uses `default' if supplied, otherwise has the same default as `m'."
  (convert-to-ch-map m default
      (let ((m-hmorg (ch-map-org m)))
	(and (or (eq m-hmorg hmorg)
		 (and (eq key-hash-fn (hash-map-org-key-hash-fn m-hmorg))
		      (eq key-compare-fn (hash-map-org-key-compare-fn m-hmorg))
		      (eq val-hash-fn (hash-map-org-val-hash-fn m-hmorg))
		      (eq val-compare-fn (hash-map-org-val-compare-fn m-hmorg))))
	     (equal?-cmp default (map-default m) val-compare-fn)))
    (let ((tree nil))
      (do-ch-map-tree-pairs (k v (ch-map-contents m))
	(setq tree (ch-map-tree-with tree k v key-hash-fn key-compare-fn val-hash-fn val-compare-fn)))
      tree)))

(defmethod convert ((to-type (eql 'fset2:map)) (s seq)
		    &key (key-fn #'car) (value-fn #'cdr) default)
  (ch-map-from-sequence s key-fn value-fn nil nil default))

(define-convert-methods (ch-map)
			((s seq) &key (key-fn #'car) (value-fn #'cdr) key-compare-fn-name val-compare-fn-name
				      (default (seq-default s)))
  (ch-map-from-sequence s key-fn value-fn key-compare-fn-name val-compare-fn-name default))
(define-convert-methods (fset2:ch-map)
			((s seq) &key (key-fn #'car) (value-fn #'cdr) key-compare-fn-name val-compare-fn-name
				      default) ; default default is now `nil'
  (ch-map-from-sequence s key-fn value-fn key-compare-fn-name val-compare-fn-name default))

(defmethod convert ((to-type (eql 'fset2:map)) (s sequence)
		    &key (key-fn #'car) (value-fn #'cdr) default)
  (ch-map-from-sequence s key-fn value-fn nil nil default))

(define-convert-methods (ch-map fset2:ch-map)
			((s sequence) &key (key-fn #'car) (value-fn #'cdr) key-compare-fn-name val-compare-fn-name
					   default)
  (ch-map-from-sequence s key-fn value-fn key-compare-fn-name val-compare-fn-name default))

(defun ch-map-from-sequence (s key-fn value-fn &optional key-compare-fn-name val-compare-fn-name default)
  (convert-to-ch-map s default nil
    (let ((key-fn (coerce key-fn 'function))
	  (value-fn (coerce value-fn 'function))
	  (tree nil))
      (if (listp s)
	  (dolist (x s)
	    (setq tree (ch-map-tree-with tree (funcall key-fn x) (funcall value-fn x)
					 key-hash-fn key-compare-fn val-hash-fn val-compare-fn)))
	(let ((it (iterator s)))
	  (while (funcall it ':more?)
	    (let ((x (funcall it ':get)))
	      (setq tree (ch-map-tree-with tree (funcall key-fn x) (funcall value-fn x)
					   key-hash-fn key-compare-fn val-hash-fn val-compare-fn))))))
      tree)))

(define-convert-methods (ch-map fset2:ch-map) ((b bag) &key key-compare-fn-name val-compare-fn-name default)
  (convert-to-ch-map b default nil
    (let ((tree nil))
      (do-bag-pairs (x n b tree)
	(setq tree (ch-map-tree-with tree x n key-hash-fn key-compare-fn val-hash-fn val-compare-fn))))))

(defmethod convert ((to-type (eql 'fset2:map)) (ht hash-table) &key (default nil default?) no-default?)
  (convert 'ch-map ht :default (fset2-default default? default no-default?)))

(define-convert-methods (ch-map fset2:ch-map) ((ht hash-table) &key key-compare-fn-name val-compare-fn-name default)
  (convert-to-ch-map ht default nil
    (let ((tree nil))
      (maphash (lambda (k v)
		 (setq tree (ch-map-tree-with tree k v key-hash-fn key-compare-fn val-hash-fn val-compare-fn)))
	       ht)
      tree)))

(defun print-ch-map (map stream level)
  (declare (ignore level))
  (pprint-logical-block
      (stream nil :prefix "##{|"
		  :suffix (let ((hmorg (ch-map-org map))
				((key-cf-name (hash-map-org-key-compare-fn-name hmorg))
				 (val-cf-name (hash-map-org-val-compare-fn-name hmorg))
				 ((key-default? (eq key-cf-name 'compare))
				  (val-default? (eq val-cf-name 'compare))))
				(dflt (map-default map)))
			    (format nil " |}~:[[~:[~S~;~*~];~:[~S~;~*~]]~;~4*~]~:[~;/~:[~S~;[no default]~]~]"
				    (and key-default? val-default?)
				    key-default? key-cf-name val-default? val-cf-name
				    dflt (eq dflt 'no-default) dflt)))
    (do-map (x y map)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline ':fill stream)
      ;; There might be a map entry for 'quote or 'function...
      (let (#+sbcl (sb-pretty:*pprint-quote-with-syntactic-sugar* nil))
	(write (list x y) :stream stream)))))

(defmethod make-load-form ((m ch-map) &optional environment)
  (declare (ignore environment))
  `(convert 'ch-map ',(convert 'list m) :key-compare-fn-name ',(hash-map-org-key-compare-fn-name (ch-map-org m))
					:val-compare-fn-name ',(hash-map-org-val-compare-fn-name (ch-map-org m))))


;;; ================================================================================
;;; Seqs

(declaim (inline raw-make-wb-seq))

(defstruct (wb-seq
	     (:include seq)
	     (:constructor raw-make-wb-seq (contents default))
	     (:predicate wb-seq?)
	     (:print-function print-wb-seq)
	     (:copier nil))
  "A class of functional seqs (sequences, but we use the short name to avoid
confusion with `cl:sequence') represented as weight-balanced binary trees.
This is the default implementation of seqs in FSet."
  (contents nil :read-only t))

;;; A slotless subclass used for method dispatching.  In this case, `contents'
;;; is always a `WB-HT-Seq-Tree'.
(defstruct (wb-ht-seq
	     (:include wb-seq)
	     (:constructor make-wb-ht-seq (contents default))
	     (:predicate nil)
	     (:print-function print-wb-seq)
	     (:copier nil)))


(defparameter +empty-wb-seq+ (raw-make-wb-seq nil nil))

(defparameter +empty-wb-seq/no-default+ (raw-make-wb-seq nil 'no-default))

(declaim (inline empty-seq fset2:empty-seq))
(defun empty-seq (&optional default)
  "Returns an empty seq of the default implementation."
  (if default (raw-make-wb-seq nil default)
    +empty-wb-seq+))
(defun fset2:empty-seq (&key (default nil default?) no-default?)
  "Returns an empty seq of the default implementation.  The seq's default is
`nil' unless a different default is supplied, or `no-default?' is true."
  (empty-wb-seq-internal (fset2-default default? default no-default?)))

(declaim (inline empty-wb-seq fset2:empty-wb-seq))
(defun empty-wb-seq (&optional default)
  "Returns an empty wb-seq."
  (if default (raw-make-wb-seq nil default)
    +empty-wb-seq+))
(defun fset2:empty-wb-seq (&key (default nil default?) no-default?)
  "Returns an empty wb-seq."
  (empty-wb-seq-internal (fset2-default default? default no-default?)))

(defun empty-wb-seq-internal (default)
  (cond ((null default)
	 +empty-wb-seq+)
	((eq default 'no-default)
	 +empty-wb-seq/no-default+)
	(t (raw-make-wb-seq nil default))))

(declaim (inline make-wb-seq))
(defun make-wb-seq (contents default)
  (if (WB-HT-Seq-Tree? contents)
      (make-wb-ht-seq contents default)
    (raw-make-wb-seq contents default)))

(defmethod empty? ((s wb-seq))
  (null (wb-seq-contents s)))

(defmethod default ((s seq))
  (let ((dflt (seq-default s)))
    (if (eq dflt 'no-default) (values nil nil)
      (values dflt t))))

(defmethod with-default ((s wb-seq) new-default)
  (make-wb-seq (wb-seq-contents s) new-default))

(defmethod fset2:without-default ((s wb-seq))
  (make-wb-seq (wb-seq-contents s) 'no-default))

(define-wb-seq-method size ((s wb-seq))
  (n-values 1 (call-selected WB-Seq-Tree-Size WB-HT-Seq-Tree-Size
			     (wb-seq-contents s))))

(define-wb-seq-method char-seq? ((s wb-seq))
  (let ((size chars? (call-selected WB-Seq-Tree-Size WB-HT-Seq-Tree-Size
				    (wb-seq-contents s))))
    (declare (ignore size))
    chars?))

(define-condition fset2:seq-bounds-error (fset2:lookup-error)
    ((fset2:seq :initarg :seq :reader fset2:seq-bounds-error-seq)
     (fset2:index :initarg :index :reader fset2:seq-bounds-error-index))
  (:report (lambda (sbe stream)
	     (let ((*print-length* 8)
		   (*print-level* 3))
	       (format stream "Index ~D out of bounds for seq ~A, which has no default"
		       (fset2:seq-bounds-error-index sbe) (fset2:seq-bounds-error-seq sbe))))))

(define-wb-seq-methods (lookup fset2:lookup) ((s wb-seq) index)
  (let ((val? val (if (typep index 'fixnum)
		      (call-selected WB-Seq-Tree-Subscript WB-HT-Seq-Tree-Subscript
				     (wb-seq-contents s) index)
		    (values nil nil))))
    (values (if val? val
	      (let ((dflt (seq-default s)))
		(if (eq dflt 'no-default)
		    (error 'fset2:seq-bounds-error :seq s :index index)
		  dflt)))
	    val?)))

(define-condition fset2:empty-seq-error (fset2:lookup-error)
    ((fset2:seq :initarg :seq :reader fset2:empty-seq-error-seq))
  (:report (lambda (sbe stream)
	     (let ((*print-length* 8)
		   (*print-level* 3))
	       (format stream "Seq ~A, which has no default, is empty"
		       (fset2:empty-seq-error-seq sbe))))))

(define-wb-seq-method first ((s wb-seq))
  (let ((val? val (call-selected WB-Seq-Tree-Subscript WB-HT-Seq-Tree-Subscript
				 (wb-seq-contents s) 0)))
    (values (if val? val
	      (let ((dflt (seq-default s)))
		(if (eq dflt 'no-default)
		    (error 'fset2:empty-seq-error :seq s)
		  dflt)))
	    val?)))

(define-wb-seq-method last ((s wb-seq))
  (let ((tree (wb-seq-contents s))
	((val? val (call-selected WB-Seq-Tree-Subscript WB-HT-Seq-Tree-Subscript
				  tree (1- (call-selected WB-Seq-Tree-Size WB-HT-Seq-Tree-Size tree))))))
    (values (if val? val
	      (let ((dflt (seq-default s)))
		(if (eq dflt 'no-default)
		    (error 'fset2:empty-seq-error :esq s)
		  dflt)))
	    val?)))

(define-wb-seq-method with-first ((s wb-seq) val)
  (make-wb-seq (call-selected WB-Seq-Tree-Insert WB-HT-Seq-Tree-Insert (wb-seq-contents s) 0 val)
	       (seq-default s)))

(define-wb-seq-method with-last ((s wb-seq) val)
  (let ((tree (wb-seq-contents s)))
    (make-wb-seq (call-selected WB-Seq-Tree-Append WB-HT-Seq-Tree-Append tree val)
		 (seq-default s))))

(define-wb-seq-method less-first ((s wb-seq))
  (let ((tree (wb-seq-contents s)))
    (make-wb-seq (call-selected WB-Seq-Tree-Subseq WB-HT-Seq-Tree-Subseq
				tree 1 (call-selected WB-Seq-Tree-Size WB-HT-Seq-Tree-Size tree))
		 (seq-default s))))

(define-wb-seq-method less-last ((s wb-seq))
  (let ((tree (wb-seq-contents s)))
    (make-wb-seq (call-selected WB-Seq-Tree-Subseq WB-HT-Seq-Tree-Subseq
				tree 0 (1- (call-selected WB-Seq-Tree-Size WB-HT-Seq-Tree-Size tree)))
		 (seq-default s))))

(define-wb-seq-method with ((s wb-seq) idx &optional (val nil val?))
  (check-three-arguments val? 'with 'wb-seq)
  (let ((tree (wb-seq-contents s))
	((size (call-selected WB-Seq-Tree-Size WB-HT-Seq-Tree-Size tree))))
    (when (< idx -1)
      (when (eq (seq-default s) 'no-default)
	(error 'fset2:seq-bounds-error :seq s :index idx))
      (setq tree (call-selected WB-Seq-Tree-Concat WB-HT-Seq-Tree-Concat
				(WB-Seq-Tree-Fill (- -1 idx) (seq-default s))
				tree))
      (setq idx -1))
    (when (> idx size)
      (when (eq (seq-default s) 'no-default)
	(error 'fset2:seq-bounds-error :seq s :index idx))
      (setq tree (call-selected WB-Seq-Tree-Concat WB-HT-Seq-Tree-Concat tree
				(WB-Seq-Tree-Fill (- idx size) (seq-default s))))
      (setq size idx))
    (make-wb-seq (if (= idx -1)
		     (call-selected WB-Seq-Tree-Insert WB-HT-Seq-Tree-Insert tree 0 val)
		   (if (= idx size)
		       (call-selected WB-Seq-Tree-Insert WB-HT-Seq-Tree-Insert tree idx val)
		     (call-selected WB-Seq-Tree-With WB-HT-Seq-Tree-With tree idx val)))
		 (seq-default s))))

(define-wb-seq-method insert ((s wb-seq) idx val)
  (let ((tree (wb-seq-contents s))
	((size (call-selected WB-Seq-Tree-Size WB-HT-Seq-Tree-Size tree))))
    (when (< idx 0)
      (when (eq (seq-default s) 'no-default)
	(error 'fset2:seq-bounds-error :seq s :index idx))
      (setq tree (call-selected WB-Seq-Tree-Concat WB-HT-Seq-Tree-Concat
				(WB-Seq-Tree-Fill (- idx) (seq-default s))
				tree))
      (setq idx 0))
    (when (> idx size)
      (when (eq (seq-default s) 'no-default)
	(error 'fset2:seq-bounds-error :seq s :index idx))
      (setq tree (call-selected WB-Seq-Tree-Concat WB-HT-Seq-Tree-Concat tree
				(WB-Seq-Tree-Fill (- idx size) (seq-default s))))
      (setq size idx))
    (make-wb-seq (call-selected WB-Seq-Tree-Insert WB-HT-Seq-Tree-Insert tree idx val)
		 (seq-default s))))

(define-wb-seq-method splice ((s wb-seq) idx subseq)
  (let ((tree (wb-seq-contents s))
	((size (call-selected WB-Seq-Tree-Size WB-HT-Seq-Tree-Size tree)))
	(subseq-tree (wb-seq-contents (convert 'wb-seq subseq))))
    (when (< idx 0)
      (when (eq (seq-default s) 'no-default)
	(error 'fset2:seq-bounds-error :seq s :index idx))
      (setq tree (WB-Seq-Tree-Concat (WB-Seq-Tree-Fill (- idx) (seq-default s))
				     tree))
      (setq idx 0))
    (when (> idx size)
      (when (eq (seq-default s) 'no-default)
	(error 'fset2:seq-bounds-error :seq s :index idx))
      (setq tree (WB-Seq-Tree-Concat tree (WB-Seq-Tree-Fill (- idx size) (seq-default s)))))
    ;; `subseq-tree' could be an HT tree.
    (make-wb-seq (WB-HT-Seq-Tree-Concat (WB-HT-Seq-Tree-Concat
					  (call-selected WB-Seq-Tree-Subseq WB-HT-Seq-Tree-Subseq tree 0 idx)
					  subseq-tree)
					(call-selected WB-Seq-Tree-Subseq WB-HT-Seq-Tree-Subseq tree idx
						       (call-selected WB-Seq-Tree-Size WB-HT-Seq-Tree-Size tree)))
		 (seq-default s))))

(define-wb-seq-method less ((s wb-seq) idx &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'wb-seq)
  (let ((tree (wb-seq-contents s))
	((size (call-selected WB-Seq-Tree-Size WB-HT-Seq-Tree-Size
			      tree))))
    (if (and (>= idx 0) (< idx size))
	(make-wb-seq (call-selected WB-Seq-Tree-Remove WB-HT-Seq-Tree-Remove
				    tree idx)
		     (seq-default s))
      s)))

(defmethod concat ((s1 seq) &rest seqs)
  (let ((tree (wb-seq-contents s1)))
    (dolist (seq seqs)
      (setq tree (WB-HT-Seq-Tree-Concat tree (wb-seq-contents (convert 'seq seq)))))
    (make-wb-seq tree (seq-default s1))))

(define-wb-seq-method subseq ((s wb-seq) start &optional end)
  (let ((tree (wb-seq-contents s))
 	((size (call-selected WB-Seq-Tree-Size WB-HT-Seq-Tree-Size tree))
	 ((start (max 0 start))
	  (end (if end (min end size) size)))))
    (if (and (= start 0) (= end size))
	s
      (make-wb-seq (call-selected WB-Seq-Tree-Subseq WB-HT-Seq-Tree-Subseq tree start end)
		   (seq-default s)))))

(define-wb-seq-method reverse ((s wb-seq))
  (make-wb-seq (call-selected WB-Seq-Tree-Reverse WB-HT-Seq-Tree-Reverse (wb-seq-contents s))
	       (seq-default s)))

(defmethod sort ((s wb-seq) pred &key key)
  (with-default (convert 'seq (cl:sort (convert 'vector s) pred :key key))
		(seq-default s)))

(defmethod stable-sort ((s wb-seq) pred &key key)
  (with-default (convert 'seq (cl:stable-sort (convert 'vector s) pred :key key))
		(seq-default s)))

(define-wb-seq-method domain ((s wb-seq))
  (let ((result nil))
    (dotimes (i (size s))
      (setq result (WB-Set-Tree-With result i #'compare)))
    (make-wb-set result)))

(define-wb-seq-method range ((s wb-seq))
  (convert 'set s))

(defmethod convert ((to-type (eql 'list)) (s wb-seq) &key)
  (WB-Seq-Tree-To-List (wb-seq-contents s)))

(defmethod convert ((to-type (eql 'vector)) (s wb-seq) &key)
  (WB-Seq-Tree-To-Vector (wb-seq-contents s)))

(define-condition non-char-seq-error (error)
    ((seq :initarg :seq :reader non-char-seq-error-seq))
  (:report (lambda (ncse stream)
	     (let ((*print-length* 8)
		   (*print-level* 3))
	       (format stream "Cannot convert seq to string; it contains non-characters:~%~S"
		       (non-char-seq-error-seq ncse))))))

(defmethod convert ((to-type (eql 'string)) (s wb-seq) &key)
  (unless (char-seq? s)
    (error 'non-char-seq-error :seq s))
  (WB-Seq-Tree-To-String (wb-seq-contents s)))

(defmethod convert ((to-type (eql 'base-string)) (s wb-seq) &key)
  (unless (char-seq? s)
    (error 'non-char-seq-error :seq s))
  (WB-Seq-Tree-To-String (wb-seq-contents s) 'base-char))

(define-convert-methods (seq fset2:seq wb-seq fset2:wb-seq) ((s wb-seq) &key)
  s)

(define-convert-methods (seq wb-seq fset2:seq fset2:wb-seq) ((vec vector) &key default)
  (make-wb-seq (WB-Seq-Tree-From-Vector vec) default))

(define-convert-methods (seq wb-seq fset2:seq fset2:wb-seq) ((l list) &key reverse? default)
  (make-wb-seq (if reverse? (WB-Seq-Tree-From-List-Reverse l)
		 (WB-Seq-Tree-From-List l))
	       default))

(define-convert-methods (seq wb-seq fset2:seq fset2:wb-seq) ((s set) &key default)
  (make-wb-seq (wb-seq-tree-from-iterable (iterator s) (size s)) default))

(define-convert-methods (seq wb-seq fset2:seq fset2:wb-seq)
			((b bag) &key pairs? (pair-fn #'cons) default)
  (bag-to-seq b pairs? pair-fn default))

(defun bag-to-seq (b pairs? pair-fn default)
  (make-wb-seq (wb-seq-tree-from-iterable (if pairs?
					      (let ((it (iterator b :pairs? t)))
						(lambda (op)
						  (if (eq op ':get)
						      (let ((v n (funcall it ':get)))
							(funcall pair-fn v n))
						    (funcall it op))))
					    (iterator b))
					  (if pairs? (set-size b) (size b)))
	       default))

(define-convert-methods (seq wb-seq fset2:seq fset2:wb-seq)
			((m map) &key (pair-fn #'cons) (default (and (member to-type '(seq wb-seq)) (map-default m))))
  (make-wb-seq (wb-seq-tree-from-iterable (let ((m-it (iterator m)))
					    (lambda (op)
					      (ecase op
						(:get (let ((k v (funcall m-it ':get)))
							(funcall pair-fn k v))))))
					  (size m))
	       default))

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

(defun compare-seqs-lexicographically (a b &optional (val-compare-fn #'compare))
  (WB-Seq-Tree-Compare-Lexicographically (wb-seq-contents a) (wb-seq-contents b) val-compare-fn))

(defmethod hash-value ((s wb-seq))
  ;; Currently doing bounded hashing, but &&& am considering changing it to do caching
  ;; with incremental update.
  (let ((result 0)
	(i 0)
	(mult 1))
    (do-wb-seq-tree-members (x (wb-seq-contents s))
      (hash-mixf result (hash-multiply mult (hash-value-fixnum x)))
      (setq mult (hash-multiply mult 13))
      (when (= (incf i) 32)
	(return)))
    result))

(defmethod compare-lexicographically ((s1 wb-seq) (s2 wb-seq) &key (val-compare-fn #'compare))
  (WB-Seq-Tree-Compare (wb-seq-contents s1) (wb-seq-contents s2) val-compare-fn
		       :lexicographic? t))

(defgeneric internal-do-seq (seq elt-fn value-fn index?
				 &key start end from-end?)
  (:documentation
    "Calls `elt-fn' on successive elements of `seq', possibly restricted by
`start' and `end', and in reverse order if `from-end?' is true.  When done,
calls `value-fn' on no arguments and returns the result(s).  This is called
by `do-seq' to provide for the possibility of different seq implementations;
it is not for public use.  `elt-fn' and `value-fn' must be function objects,
not symbols."))


(define-wb-seq-method internal-do-seq ((s wb-seq) elt-fn value-fn index?
				       &key (start 0) end from-end?)
  (declare (optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  (unless end
    (setq end (call-selected WB-Seq-Tree-Size WB-HT-Seq-Tree-Size
			     (wb-seq-contents s))))
  (assert (and (typep start 'fixnum) (typep end 'fixnum)))
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

(defmethod iterator ((s wb-seq) &key start end from-end?)
  (if from-end?
      (Make-WB-Seq-Tree-Rev-Iterator (wb-seq-contents s) start end)
    (Make-WB-Seq-Tree-Iterator (wb-seq-contents s) start end)))

(defmethod fun-iterator ((s wb-seq) &key from-end?)
  (if from-end?
      (WB-Seq-Tree-Rev-Fun-Iter (wb-seq-contents s))
    (WB-Seq-Tree-Fun-Iter (wb-seq-contents s))))

(defgeneric internal-do-seq-chunks (seq vec-fn value-fn)
  (:documentation
    "Internally, a seq is represented as a tree whose leaves are vectors, each
either a `simple-vector' or a `simple-string'.  Calls `vec-fn' on successive
vectors of `seq'.  When done, calls `value-fn' on no arguments and returns the
result(s).  This is called by `do-seq-chunks' to provide for the possibility of
different seq implementations; it is not for public use.  `vec-fn' and
`value-fn' must be function objects, not symbols."))

(defmethod internal-do-seq-chunks (seq vec-fn value-fn)
  (Do-WB-Seq-Tree-Leaves (v (wb-seq-contents seq) (funcall value-fn))
    (funcall vec-fn v)))

(defmethod domain-contains? ((s seq) x)
  (and (integerp x) (>= x 0) (< x (size s))))

(defmethod range-contains? ((s seq) x)
  (declare (optimize (speed 3) (safety 0)))
  (do-seq (y s)
    (when (equal? y x)
      (return t))))

(define-methods (filter fset2:filter) ((fn function) (s seq))
  (seq-filter fn s))

(define-methods (filter fset2:filter) ((fn symbol) (s seq))
  (seq-filter (coerce-to-function fn) s))

(defmethod filter ((fn map) (s seq))
  (seq-filter #'(lambda (x) (lookup fn x)) s))
(defmethod fset2:filter ((fn map) (s seq))
  (seq-filter #'(lambda (x) (fset2:lookup fn x)) s))

(define-methods (filter fset2:filter) ((fn set) (s seq))
  (seq-filter #'(lambda (x) (contains? fn x)) s))

(define-methods (filter fset2:filter) ((fn bag) (s seq))
  (seq-filter #'(lambda (x) (contains? fn x)) s))

(defun seq-filter (fn s)
  (declare (optimize (speed 3) (safety 0))
	   (type function fn))
  (let ((result nil))
    (do-seq (x s)
      (when (funcall fn x)
	(push x result)))
    (make-wb-seq (WB-Seq-Tree-From-List (nreverse result))
		 (seq-default s))))

(define-methods (partition fset2:partition) ((fn function) (s seq))
  (seq-partition fn s))

(define-methods (partition fset2:partition) ((fn symbol) (s seq))
  (seq-partition (coerce-to-function fn) s))

(defmethod partition ((fn map) (s seq))
  (seq-partition #'(lambda (x) (lookup fn x)) s))
(defmethod fset2:partition ((fn map) (s seq))
  (seq-partition #'(lambda (x) (fset2:lookup fn x)) s))

(define-methods (partition fset2:partition) ((fn set) (s seq))
  (seq-partition #'(lambda (x) (contains? fn x)) s))

(define-methods (partition fset2:partition) ((fn bag) (s seq))
  (seq-partition #'(lambda (x) (contains? fn x)) s))

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

(defmethod image ((fn function) (s seq) &key)
  (seq-image fn s (seq-default s)))
(defmethod fset2:image ((fn function) (s seq) &key)
  (seq-image fn s (let ((dflt (seq-default s)))
		    (if (eq dflt 'no-default) dflt
		      (funcall fn dflt)))))

(defmethod image ((fn symbol) (s seq) &key)
  (seq-image (coerce-to-function fn) s (seq-default s)))
(defmethod fset2:image ((fn symbol) (s seq) &key)
  (let ((fn (coerce-to-function fn)))
    (seq-image fn s (let ((dflt (seq-default s)))
		      (if (eq dflt 'no-default) dflt
			(funcall fn dflt))))))

(defmethod image ((fn map) (s seq) &key)
  (seq-image #'(lambda (x) (lookup fn x)) s (seq-default s)))
(defmethod fset2:image ((fn map) (s seq) &key)
  (seq-image #'(lambda (x) (fset2:lookup fn x)) s (let ((dflt (seq-default s)))
						    (if (eq dflt 'no-default) dflt
						      (fset2:lookup fn (seq-default s))))))

(defmethod image ((fn set) (s seq) &key)
  (seq-image #'(lambda (x) (lookup fn x)) s (seq-default s)))
(defmethod fset2:image ((fn set) (s seq) &key)
  (seq-image #'(lambda (x) (fset2:lookup fn x)) s (let ((dflt (seq-default s)))
						    (if (eq dflt 'no-default) dflt
						      (fset2:lookup fn (seq-default s))))))

(defmethod image ((fn bag) (s seq) &key)
  (seq-image #'(lambda (x) (lookup fn x)) s (seq-default s)))
(defmethod fset2:image ((fn bag) (s seq) &key)
  (seq-image #'(lambda (x) (fset2:lookup fn x)) s (let ((dflt (seq-default s)))
						    (if (eq dflt 'no-default) dflt
						      (fset2:lookup fn (seq-default s))))))

(defun seq-image (fn s default)
  (make-wb-seq (WB-HT-Seq-Tree-Image (wb-seq-contents s) fn) default))

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
      (do-seq (x s :start start :end end :from-end? from-end?)
	(if call-fn?
	    (setq result (funcall fn result (if key (funcall key x) x)))
	  (setq result (if key (funcall key x) x)
		call-fn? t))))
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
  (let ((test (coerce-to-function-or-equal? test))
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
		(when (funcall test item (funcall key x))
		  (done))
		(incf pos)))
          (do-seq (x s :start start :end end :from-end? from-end)
	    (when (funcall test item x)
	      (done))
	    (incf pos)))))))

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
    (concat head (concat (convert 'seq mid :reverse? (not from-end))
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
    (concat head (concat (convert 'seq mid :reverse? (not from-end))
			 tail))))

(defmethod substitute-if-not (newitem pred (s seq) &key key start end from-end count)
  (declare (optimize (speed 3) (safety 0)))
  (let ((pred (coerce-to-function pred)))
    (substitute-if newitem #'(lambda (x) (not (funcall pred x))) s
		   :key key :start start :end end :from-end from-end :count count)))

;;; TIL!  "Quaesitum" is Latin for "thing to be searched for".
(defmethod search (quaesitum (in-seq seq) &key from-end test key (start1 0) (start2 0) end1 end2)
  (declare (optimize (speed 3) (safety 1))
	   (fixnum start1 start2))
  (let ((quaesitum (convert 'seq quaesitum))
	(test (if test (coerce-to-function test) #'equal?))
	(key (and key (coerce-to-function key)))
	(q-size (size quaesitum))
	(s-size (size in-seq))
	((end1 (or end1 q-size))
	 (end2 (or end2 s-size))))
    (declare (fixnum q-size s-size end1 end2))
    (unless (and (<= 0 start1 q-size) (<= 0 start2 s-size)
		 (<= 0 end1 q-size) (<= 0 end2 s-size))
      (error "One of `start1', `end1', `start2', or `end2' is out of bounds: ~D, ~D, ~D, ~D"
	     start1 end1 start2 end2))
    (flet ((apply-key (x)
	     (muffle-notes  ; `split-cases' will cause unreachable code, as intended.
	       (if key (funcall key x) x))))
      (declare (inline apply-key))
      (if (not from-end)
	  (if (<= end1 start1)
	      start2
	    (split-cases (key)
	      (let ((q-it (the function (iterator quaesitum :start start1 :end end1))))
		(loop
		  (let ((q-elt (apply-key (funcall q-it ':get)))
			(match-started? nil))
		    (Do-WB-Seq-Tree-Members-Gen (seq-elt (wb-seq-contents in-seq) start2 end2 nil)
		      (when (> (the fixnum (+ start2 q-size)) end2)
			(return-from search nil))
		      (if (funcall test q-elt (apply-key seq-elt))
			  (if (funcall q-it ':done?)
			      (return-from search (the fixnum start2))
			    (setq q-elt (apply-key (funcall q-it ':get))
				  match-started? t))
			(progn
			  (incf start2)
			  (when match-started?
			    (funcall q-it ':reset)
			    (return))))))))))  ; exits inner loop
	(if (<= end1 start1)
	    end2
	  (split-cases (key)
	    (let ((q-it (the function (iterator quaesitum :start start1 :end end1 :from-end? t))))
	      (loop
		(let ((q-elt (apply-key (funcall q-it ':get)))
		      (match-started? nil))
		  (Do-WB-Seq-Tree-Members-Gen (seq-elt (wb-seq-contents in-seq) start2 end2 t)
		    (when (< (the fixnum (- end2 q-size)) start2)
		      (return-from search nil))
		    (if (funcall test q-elt (apply-key seq-elt))
			(if (funcall q-it ':done?)
			    (return-from search (the fixnum (- end2 q-size)))
			  (setq q-elt (apply-key (funcall q-it ':get))
				match-started? t))
		      (progn
			(decf end2)
			(when match-started?
			  (funcall q-it ':reset)
			  (return))))))))))))))

(defmethod mismatch ((sequence-1 seq) sequence-2 &key from-end test key (start1 0) (start2 0) end1 end2)
  (declare (optimize (speed 3) (safety 1))
	   (fixnum start1 start2))
  (let ((test (if test (coerce-to-function test) #'equal?))
	(key (and key (coerce-to-function key)))
	(sequence-2 (convert 'seq sequence-2))
	((size-1 (size sequence-1))
	 (size-2 (size sequence-2))
	 ((end1 (or end1 size-1))
	  (end2 (or end2 size-2)))))
    (declare (fixnum size-1 size-2 end1 end2))
    (unless (and (<= 0 start1 size-1) (<= 0 start2 size-2)
		 (<= 0 end1 size-1) (<= 0 end2 size-2))
      (error "One of `start1', `end1', `start2', or `end2' is out of bounds: ~D, ~D, ~D, ~D"
	     start1 end1 start2 end2))
    (flet ((apply-key (x)
	     (muffle-notes  ; `split-cases' will cause unreachable code, as intended.
	       (if key (funcall key x) x))))
      (if (or (<= end1 start1) (<= end2 start2))
	      0
	(if (not from-end)
	    (split-cases (key)
	      (let ((it-1 (the function (iterator sequence-1 :start start1 :end end1)))
		    (it-2 (the function (iterator sequence-2 :start start2 :end end2)))
		    (idx start1))
		(declare (fixnum idx))
		(loop
		  (when (or (funcall it-1 ':done?) (funcall it-2 ':done?))
		    (return nil))
		  (let ((elt-1 (apply-key (funcall it-1 ':get)))
			(elt-2 (apply-key (funcall it-2 ':get))))
		    (unless (funcall test elt-1 elt-2)
		      (return idx)))
		  (incf idx))))
	  (split-cases (key)
	    (let ((it-1 (the function (iterator sequence-1 :start start1 :end end1 :from-end? t)))
		  (it-2 (the function (iterator sequence-2 :start start2 :end end2 :from-end? t)))
		  (idx end1))
	      (declare (fixnum idx))
	      (loop
		(when (or (funcall it-1 ':done?) (funcall it-2 ':done?))
		  (return nil))
		(let ((elt-1 (apply-key (funcall it-1 ':get)))
		      (elt-2 (apply-key (funcall it-2 ':get))))
		  (unless (funcall test elt-1 elt-2)
		    (return idx)))
		(decf idx)))))))))

(defmethod mismatch (sequence-1 (sequence-2 seq) &rest keyword-args)
  (apply #'mismatch (convert 'seq sequence-1) sequence-2 keyword-args))


(defun print-wb-seq (seq stream level)
  (declare (ignore level))
  (flet ((print-as-string (prefix seq stream)
	   (write-string prefix stream)
	   (write-char #\" stream)
	   (do-seq (c seq)
	     (when (or (eql c #\\) (eql c #\"))
	       (write-char #\\ stream))
	     (write-char c stream))
	   (write-char #\" stream)))
    (if (char-seq? seq)
	(if (or *print-readably* *print-escape*)
	    (print-as-string "#" seq stream)
	  ;; Probably faster than writing individual characters.
	  (do-seq-chunks (s seq)
	    (write-string s stream)))
      (pprint-logical-block (stream nil :prefix "#["
					:suffix (let ((dflt (seq-default seq)))
						  (format nil " ]~:[~;/~:[~S~;[no default]~]~]"
							  dflt (eq dflt 'no-default) dflt)))
	(let ((chars (empty-seq)))
	  (labels ((print-thing (x)
		     (pprint-pop)
		     (write-char #\Space stream)
		     (pprint-newline ':fill stream)
		     (write x :stream stream))
		   (print-chars ()
		     (if (>= (size chars) 2)
			 (progn
			   (pprint-pop)
			   (write-char #\Space stream)
			   (pprint-newline ':fill stream)
			   (print-as-string "#$" chars stream))
		       (do-seq (x chars)
			 (print-thing x)))
		     (setq chars (empty-seq))))
	    (do-seq (x seq)
	      (if (characterp x)
		  (push-last chars x)
		(progn
		  (print-chars)
		  (print-thing x))))
	    (print-chars)))))))

(defmethod make-load-form ((s wb-seq) &optional environment)
  (declare (ignore environment))
  `(with-default (convert 'wb-seq ',(convert 'list s)) ',(seq-default s)))


;;; ================================================================================
;;; CL Sequences and Functions

;;; Convenience methods for some of the FSet generic functions.

(defmethod empty? ((l list))
  (null l))

(defmethod empty? ((s sequence))
  (zerop (length s)))

(defmethod size ((s sequence))
  (length s))

(define-methods (lookup fset2:lookup) ((s sequence) (idx integer))
  (values (elt s idx) t))

(define-methods (lookup fset2:lookup) ((fn function) (v t))
  (funcall fn v))

(define-methods (lookup fset2:lookup) ((fn symbol) (v t))
  (funcall fn v))

(defmethod convert ((to-type (eql 'list)) (v vector) &key)
  (coerce v 'list))

(defmethod convert ((to-type (eql 'vector)) (l list) &key)
  (coerce l 'vector))

(defmethod convert ((to-type (eql 'string)) (s sequence) &key)
  (coerce s 'string))

(defmethod convert ((to-type (eql 'base-string)) (s sequence) &key)
  (coerce s 'base-string))

(defmethod convert ((to-type (eql 'list)) (s sequence) &key)
  (coerce s 'list))

(defmethod convert ((to-type (eql 'vector)) (s sequence) &key)
  (coerce s 'vector))

(declaim (inline compose-with-key-fn))
(defun compose-with-key-fn (fn collection)
  (declare (type function fn))
  (lambda (key) (lookup collection (funcall fn key))))

(define-methods (compose fset2:compose) ((fn function) (m map) &key)
  (compose-with-key-fn fn m))

(define-methods (compose fset2:compose) ((fn symbol) (m map) &key)
  (compose-with-key-fn (coerce fn 'function) m))

(define-methods (compose fset2:compose) ((fn function) (s seq) &key)
  (compose-with-key-fn fn s))

(define-methods (compose fset2:compose) ((fn symbol) (s seq) &key)
  (compose-with-key-fn (coerce fn 'function) s))


;;; ================================================================================
;;; Miscellany

(define-condition simple-program-error (simple-condition program-error)
  ())

