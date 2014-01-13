/*
 * PureCachedHashSet.java
 *
 * Copyright (c) 2003 Sympoiesis, Inc.  All rights reserved.
 *
 * $Id: PureCachedHashSet.java,v 1.18 2006/09/27 21:58:38 gyro Exp $
 */


package com.sympoiesis.FSet;
import java.util.*;
import java.lang.ref.*;

/**
 * A pure set that uses hash codes to order objects.  The name notwithstanding, it
 * is implemented as a tree, not a hash table, but its ordering function is
 * implemented by calling <code>hashCode</code>.  Thus the element objects need not
 * implement {@link Comparable}; they need not even have any superclass in common
 * other than <code>Object</code>.  In the case where distinct elements have the
 * same hash code, the implementation is correct, but less efficient; so it is
 * important to make the hash codes distinct whenever possible.  No guarantee is
 * made as to the order in which elements are returned by the iterator.
 *
 * <p>This class is very similar to <code>PureHashSet</code>; the difference is that
 * this one caches the hash code of each element.  This improves performance,
 * particularly in the case where the elements have an expensive hash code
 * computation and do not cache the result themselves; but this class is slightly
 * faster even when that is not the case.
 *
 * <p>Time costs: <code>isEmpty</code>, <code>size</code>, and <code>arb</code> take
 * O(1) (constant) time.  <code>contains</code>, <code>with</code>, and
 * <code>less</code> take O(log <i>n</i>) time.  <code>union</code>,
 * <code>intersection</code>, <code>difference</code>, <code>isSubset</code>,
 * <code>isSuperset</code>, and <code>equals</code> take O(n) (linear) time if the
 * other set involved is also a <code>PureCachedHashSet</code>; otherwise, they take
 * O(<i>n</i> log <i>n</i>) time.  <code>compareTo</code> (called, for instance, if
 * this set is an element in a containing <code>PureTreeSet</code>) takes O(n)
 * (linear) time.
 *
 * <p>Space costs: <code>PureCachedHashSet</code> uses a heterogeneous binary tree
 * structure with bounded-length arrays at the leaves.  It uses much less space than
 * traditional homogeneous binary trees; typical space consumption is roughly four
 * times that of a plain array.  (<code>PureCachedHashSet</code> uses more space
 * than <code>PureHashSet</code> because it caches the hash code of each element.)
 *
 * <p><code>PureCachedHashSet</code> accepts the null element.
 *
 * <p><code>PureCachedHashSet</code> implements {@link java.io.Serializable}; an instance
 * of it is serializable provided that all elements it contains are serializable.
 *
 * @author Scott L. Burson, Sympoiesis, Inc.
 * @see PureSet
 * @see PureTreeSet
 * @see PureHashSet
 */

public class PureCachedHashSet
    extends AbstractPureSet
    implements Comparable, java.io.Serializable
{

    /**
     * Constructs an empty <code>PureCachedHashSet</code>.
     */
    public PureCachedHashSet () {
	tree = null;
    }

    /**
     * Constructs a <code>PureCachedHashSet</code> containing the same elements as
     * <code>coll</code>.
     *
     * @param coll the collection to use the elements of
     */
    public PureCachedHashSet (Collection coll) {
	if (coll instanceof PureCachedHashSet) tree = ((PureCachedHashSet)coll).tree;
	else if (coll instanceof RandomAccess)		// entails `List'
	    tree = fromRandomAccessCollection((List)coll, 0, coll.size());
	else tree = new PureTreeList(coll).toPureCachedHashSet().tree;
    }

    private Subtree fromRandomAccessCollection (List list, int lo, int hi) {
	if (lo == hi) return null;
	else if (lo + 1 == hi)
	    return with(null, list.get(lo));
	else {
	    int mid = (lo + hi) >> 1;
	    return union(fromRandomAccessCollection(list, lo, mid),
			 fromRandomAccessCollection(list, mid, hi));
	}
    }

    /**
     * Constructs a <code>PureCachedHashSet</code> whose elements are the components of
     * <code>ary</code>.  That is, the elements are <code>ary[0]</code>,
     * <code>ary[1]</code>, etc.  (So, if <code>ary</code> is multidimensional, the
     * elements of the set will be the inner arrays, not the objects which are
     * ultimately found after multiple levels of indexing.)
     *
     * @param ary the array to use the components of
     */
    public PureCachedHashSet (Object[] ary) {
	tree = fromArray(ary, 0, ary.length);
    }

    private Subtree fromArray (Object[] ary, int lo, int hi) {
	if (lo == hi) return null;
	else if (lo + 1 == hi)
	    return with(null, ary[lo]);
	else {
	    int mid = (lo + hi) >> 1;
	    return union(fromArray(ary, lo, mid),
			 fromArray(ary, mid, hi));
	}
    }

    public boolean isEmpty () {
	return tree == null;
    }

    public int size () {
	return Subtree.size(tree);
    }

    public Object arb () {
	if (tree == null) return null;
	else return tree.arb();
    }

    public boolean contains (Object elt) {
	return Subtree.contains(tree, elt, hashCode(elt));
    }

    public Iterator iterator () {
	return new PCHSIterator(tree);
    }

    public PureSet with (Object elt) {
	Subtree t = Subtree.with(tree, elt, hashCode(elt));
	if (t == tree) return this;
	else return new PureCachedHashSet(t);
    }

    public PureSet less (Object elt) {
	Subtree t = Subtree.less(tree, elt, hashCode(elt));
	if (t == tree) return this;
	else return new PureCachedHashSet(t);
    }

    /**
     * Returns the union of this set with <code>coll</code>.  That is, returns a set
     * containing all elements that are in either this set, or <code>coll</code>, or
     * both.  The returned set is of the same class as this set and uses the same
     * ordering.
     *
     * <p>This operation runs in O(n) (linear) time if <code>coll</code> is also a
     * <code>PureCachedHashSet</code>; otherwise it runs in O(n log n) time.
     *
     * @param coll the set to take the union with
     * @return the union of the two sets
     */
    public PureSet union (Collection coll) {
	if (coll == this) return this;
	else if (coll instanceof PureCachedHashSet) {
	    PureCachedHashSet pchs = (PureCachedHashSet)coll;
	    Subtree t = union(tree, pchs.tree);
	    return new PureCachedHashSet(t);
	} else {
	    // Not a `PureCachedHashSet'.
	    PureCachedHashSet pchs = new PureCachedHashSet(coll);
	    Subtree t = union(tree, pchs.tree);
	    return new PureCachedHashSet(t);
	}
    }

    /**
     * Returns the intersection of this set with <code>coll</code>.  That is, returns a
     * set containing all elements that are in both this set and <code>coll</code>.
     * The returned set is of the same class as this set and uses the same ordering.
     *
     * <p>This operation runs in O(n) (linear) time if <code>coll</code> is also a
     * <code>PureCachedHashSet</code>; otherwise it runs in O(n log n) time.
     *
     * @param coll the set to take the intersection with
     * @return the intersection of the two sets
     */
    public PureSet intersection (Collection coll) {
	if (coll == this) return this;
	else if (coll instanceof PureCachedHashSet) {
	    PureCachedHashSet pchs = (PureCachedHashSet)coll;
	    Subtree t = intersection(tree, pchs.tree);
	    return new PureCachedHashSet(t);
	} else {
	    // Not a `PureCachedHashSet'.
	    PureCachedHashSet pchs = new PureCachedHashSet(coll);
	    Subtree t = intersection(tree, pchs.tree);
	    return new PureCachedHashSet(t);
	}
    }

    /**
     * Returns the difference of this set less <code>coll</code>.  That is, returns a
     * set containing all elements that are in this set and not in <code>coll</code>.
     * The returned set is of the same class as this set and uses the same ordering.
     *
     * <p>This operation runs in O(n) (linear) time if <code>coll</code> is also a
     * <code>PureCachedHashSet</code>; otherwise it runs in O(n log n) time.
     *
     * @param coll the set to take the difference with
     * @return the difference of the two sets (this set less <code>coll</code>)
     */
    public PureSet difference (Collection coll) {
	if (coll == this) return new PureCachedHashSet();
	else if (coll instanceof PureCachedHashSet) {
	    PureCachedHashSet pchs = (PureCachedHashSet)coll;
	    Subtree t = difference(tree, pchs.tree);
	    return new PureCachedHashSet(t);
	} else {
	    // Not a `PureCachedHashSet'.
	    PureCachedHashSet pchs = new PureCachedHashSet(coll);
	    Subtree t = difference(tree, pchs.tree);
	    return new PureCachedHashSet(t);
	}
    }

    public int compareTo (Object obj) {
	if (obj == this) return 0;
	else if (obj == null || !(obj instanceof PureCachedHashSet))
	    throw new ClassCastException();
	else {
	    Subtree tree2 = ((PureCachedHashSet)obj).tree;
	    if (tree == tree2) return 0;
	    int size = Subtree.size(tree), size2 = Subtree.size(tree2);
	    // Start by comparing the sizes; smaller sets are considered less than
	    // larger ones.  Only if the sizes are equal do we have to do the lexicographic
	    // comparison.
	    if (size < size2) return -1;
	    else if (size > size2) return 1;
	    else return Subtree.compareTo(tree, 0, tree2, 0, 0, size);
	}
    }

    /* We don't bother caching equality results on sets smaller than this. */
    private static final int EQUALS_CACHE_THRESHOLD = 8;

    public boolean equals (Object obj) {
	if (obj == this || cachedEquals(obj)) return true;
	else if (obj instanceof PureCachedHashSet) {
	    PureCachedHashSet pchs = (PureCachedHashSet)obj;
	    boolean is_equal = equals(tree, pchs.tree);
	    if (is_equal && size() >= EQUALS_CACHE_THRESHOLD) {
		cacheEqual(pchs);
		pchs.cacheEqual(this);
	    }
	    return is_equal;
	} else if (!(obj instanceof Collection)) return false;
	else {
	    Collection coll = (Collection)obj;
	    if (size() != coll.size()) return false;
	    for (Iterator it = coll.iterator(); it.hasNext(); )
		if (!contains(it.next())) return false;
	    if (size() >= EQUALS_CACHE_THRESHOLD) {
		if (obj instanceof PureSet) {
		    cacheEqual((PureSet)obj);
		    if (obj instanceof AbstractPureSet)
			((AbstractPureSet)obj).cacheEqual(this);
		}
	    }
	    return true;
	}
    }

    /**
     * Returns true if this set is a subset of <code>coll</code>.  That is, returns
     * true if <code>coll</code> contains all elements of this set.  The inclusion need
     * not be proper; that is, this method returns true if the two sets are equal.
     *
     * <p>This operation runs in O(n) (linear) time if <code>coll</code> is also a
     * <code>PureCachedHashSet</code>; otherwise it runs in O(n log n) time.
     *
     * @param coll the collection to compare against
     * @return whether this set is a subset of <code>coll</code>
     */
    public boolean isSubset (Collection coll) {
	if (coll == this) return true;
	else if (size() > coll.size()) return false;
	else if (coll instanceof PureCachedHashSet) {
	    PureCachedHashSet pts = (PureCachedHashSet)coll;
	    return isSubset(tree, pts.tree);
	} else {
	    for (Iterator it = iterator(); it.hasNext(); )
		if (!coll.contains(it.next())) return false;
	    return true;
	}
    }

    /**
     * Returns true if this set is a superset of <code>coll</code>.  That is, returns
     * true if this set contains all elements of <code>coll</code>.  The inclusion need
     * not be proper; that is, this method returns true if the two sets are equal.
     * (Synonym for <code>containsAll</code>.)
     *
     * <p>This operation runs in O(n) (linear) time if <code>coll</code> is also a
     * <code>PureCachedHashSet</code>; otherwise it runs in O(n log n) time.
     *
     * @param coll the collection to compare against
     * @return whether this set is a superset of <code>coll</code>
     */
    public boolean isSuperset (Collection coll) {
	if (coll == this) return true;
	else if (size() < coll.size()) return false;
	else if (coll instanceof PureCachedHashSet) {
	    PureCachedHashSet pts = (PureCachedHashSet)coll;
	    return isSubset(pts.tree, tree);
	} else {
	    for (Iterator it = coll.iterator(); it.hasNext(); )
		if (!contains(it.next())) return false;
	    return true;
	}
    }

    // Overriding this just to provide a slightly more efficient implementation.
    // The default one (in `AbstractSet') uses the iterator.  But we have to compute
    // the same value here, viz., the sum of the hash codes of the elements.
    public int hashCode () {
	if (hash_code == Integer.MIN_VALUE) hash_code = hashCode(tree);
	return hash_code;
    }

    // For debugging.
    String dump () {
	return dump(tree);
    }

    boolean verify () {
	return verify(tree, NEGATIVE_INFINITY, POSITIVE_INFINITY);
    }

    /******************************************************************************/
    /* Internals */

    // Inspired by Stephen Adams' paper on weight-balanced binary trees.  As an additional
    // development, these trees are heterogeneous: instead of consisting entirely of nodes,
    // the lowest two to three levels of the tree are stored in bounded-length vectors.
    // This cuts space requirements roughly in half without costing much (if any) time.

    /* Instance variables */
    // This has package access for benefit of `PureCachedHashMap.restrict[Not]'.
    transient Subtree tree;
    private transient int hash_code = Integer.MIN_VALUE;		// cache
    // See `cacheEqual'.
    private transient WeakReference[] equality_cache = null;

    /* The threshold length above which tree nodes will be built. */
    private static final int MAX_LEAF_ARRAY_LENGTH = 8;

    /* The factor by which one subtree may outweigh another.  See Adams.  Don't
     * change this unless you understand his analysis. */
    private static final int BALANCE_FACTOR = 4;

    /* To represent negative and positive infinity, we use the smallest and largest
     * available integers.  We arrange to make sure `hashCode(Object)' never returns
     * them.
     */
    private static final int NEGATIVE_INFINITY = Integer.MIN_VALUE;
    private static final int POSITIVE_INFINITY = Integer.MAX_VALUE;

    private static int hashCode (Object x) {
	if (x == null) return 0;
	int h = x.hashCode();
	if (h == NEGATIVE_INFINITY) return NEGATIVE_INFINITY + 1;
	else if (h == POSITIVE_INFINITY) return POSITIVE_INFINITY - 1;
	else return h;
    }

    /* Again we play a little game to work around Java's inability to efficiently
     * return multiple values.  In this case `findEquiv' needs to return something
     * indicating "no element found", but it can't be `null' because we allow `null'
     * as an element.  So we make a special object. */
    // This has package access for benefit of `PureCachedHashMap.restrict[Not]'.
    static final Object NO_ELEMENT = new Object();

    /****************/

    // This has default (package-wide) access so `PureCachedHashMap.domain' can use it.
    // Also, `PureTreeList.toPureCachedHashSet'.
    PureCachedHashSet (Subtree _tree) {
	tree = _tree;
    }

    private static int elementSize (Object elt) {
	if (elt instanceof EquivalentSet)
	    return ((EquivalentSet)elt).contents.size();
	else return 1;
    }

    private static int hashCode (Subtree subtree) {
	if (subtree == null) return 0;
	else return subtree.hashCode();
    }

    // Used by `PureTreeList.toPureCachedHashSet'.
    static Subtree union (Subtree tree1, Subtree tree2) {
	return Subtree.union(tree1, tree2, NEGATIVE_INFINITY, POSITIVE_INFINITY);
    }

    private static Subtree intersection (Subtree tree1, Subtree tree2) {
	return Subtree.intersection(tree1, tree2, NEGATIVE_INFINITY, POSITIVE_INFINITY);
    }

    private static Subtree difference (Subtree tree1, Subtree tree2) {
	return Subtree.difference(tree1, tree2, NEGATIVE_INFINITY, POSITIVE_INFINITY);
    }

    private static boolean equals (Subtree tree1, Subtree tree2) {
	if (tree1 == tree2) return true;
	int size1 = Subtree.size(tree1), size2 = Subtree.size(tree2);
	if (size1 != size2) return false;
	else return Subtree.equals(tree1, 0, tree2, 0, 0, size1);
    }

    private static boolean isSubset (Subtree tree1, Subtree tree2) {
	return Subtree.isSubset(tree1, tree2, NEGATIVE_INFINITY, POSITIVE_INFINITY);
    }

    // Used by `PureTreeList.toPureCachedHashSet'.
    static Subtree with (Subtree tree, Object elt) {
	return Subtree.with(tree, elt, hashCode(elt));
    }

    /****************/
    /* A subtree can be either null, a `Node', or a `Leaf'.  In `PureTreeSet' we
     * used a bare `Object[]' for a leaf, but that doesn't work here because we need
     * to cache the hash codes as well; since those are ints, they can't go in the
     * `Object[]'.  (Making `Integer's for them would more than chew up the space we
     * gain by using arrays at the leaves in the first place.)  So this time we go
     * with proper OO style rather than all those `instanceof' tests -- probably
     * gaining some speed as well (though we still do `instanceof' to check for
     * `EquivalentSet's). */
    static abstract class Subtree {

	static Node makeNode (Object elt, int ehash, Subtree left, Subtree right) {
	    return new Node(size(left) + size(right) + elementSize(elt),
			    elt, ehash, left, right);
	}

	static Leaf makeLeaf (Object[] elements, int[] hashes) {
	    if (elements == null) return null;
	    else return new Leaf(elements, hashes);
	}

	static Leaf buildLeaf (Object elt, int ehash, Leaf left, Leaf right) {
	    int llen = (left == null ? 0 : left.elements.length);
	    int rlen = (right == null ? 0 : right.elements.length);
	    int len = llen + 1 + rlen;
	    Object[] elements = new Object[len];
	    int[] hashes = new int[len];
	    for (int i = 0; i < llen; ++i) {
		elements[i] = left.elements[i];
		hashes[i] = left.hashes[i];
	    }
	    elements[llen] = elt;
	    hashes[llen] = ehash;
	    for (int i = 0; i < rlen; ++i) {
		elements[i + llen + 1] = right.elements[i];
		hashes[i + llen + 1] = right.hashes[i];
	    }
	    return new Leaf(elements, hashes);
	}

	abstract int size();

	static int size (Subtree subtree) {
	    if (subtree == null) return 0;
	    else return subtree.size();
	}

	abstract Object arb();

	abstract boolean contains(Object elt, int ehash);

	static boolean contains (Subtree subtree, Object elt, int ehash) {
	    if (subtree == null) return false;
	    else return subtree.contains(elt, ehash);
	}

	abstract Subtree with(Object elt, int ehash);	// `elt' may be an `EquivalentSet'

	static Subtree with (Subtree subtree, Object elt, int ehash) {
	    if (subtree == null) {
		if (!(elt instanceof EquivalentSet)) {
		    Object[] elements = new Object[1];
		    elements[0] = elt;
		    int[] hashes = new int[1];
		    hashes[0] = ehash;
		    return new Leaf(elements, hashes);
		} else return makeNode(elt, ehash, null, null);
	    } else return subtree.with(elt, ehash);
	}

	abstract Subtree less(Object elt, int ehash);

	static Subtree less (Subtree subtree, Object elt, int ehash) {
	    if (subtree == null) return null;
	    else return subtree.less(elt, ehash);
	}

	abstract Object minElement();
	abstract int minHash();
	abstract Subtree lessMin();

	abstract Subtree union(Subtree subtree2, int lo, int hi);

	static Subtree union (Subtree subtree1, Subtree subtree2, int lo, int hi) {
	    if (subtree2 == null) return split(subtree1, lo, hi);
	    else if (subtree1 == null) return split(subtree2, lo, hi);
	    else return subtree1.union(subtree2, lo, hi);
	}

	abstract Subtree intersection(Subtree subtree2, int lo, int hi);

	static Subtree intersection (Subtree subtree1, Subtree subtree2,
					     int lo, int hi) {
	    if (subtree1 == null || subtree2 == null) return null;
	    else return subtree1.intersection(subtree2, lo, hi);
	}

	abstract Subtree difference(Subtree subtree2, int lo, int hi);

	static Subtree difference (Subtree subtree1, Subtree subtree2, int lo, int hi) {
	    if (subtree1 == null) return null;
	    else if (subtree2 == null) return split(subtree1, lo, hi);
	    else return subtree1.difference(subtree2, lo, hi);
	}

	abstract int compareTo(int base, Subtree subtree2, int base2, int lo, int hi);

	// `lo' and `hi' are ranks, not hash codes.
	static int compareTo (Subtree subtree1, int base1, Subtree subtree2, int base2,
			      int lo, int hi) {
	    if (lo == hi) return 0;
	    else return subtree1.compareTo(base1, subtree2, base2, lo, hi);
	}

	abstract boolean equals(int base, Subtree subtree2, int base2, int lo, int hi);

	// `lo' and `hi' are ranks, not hash codes.
	static boolean equals (Subtree subtree1, int base1, Subtree subtree2, int base2,
			       int lo, int hi) {
	    if (lo == hi) return true;
	    else return subtree1.equals(base1, subtree2, base2, lo, hi);
	}

	abstract boolean isSubset(Subtree subtree2, int lo, int hi);

	static boolean isSubset (Subtree subtree1, Subtree subtree2, int lo, int hi) {
	    if (subtree1 == null) return true;
	    else return subtree1.isSubset(subtree2, lo, hi);
	}

	abstract Subtree split(int lo, int hi);

	// Returns a new tree all of whose elements are greater than `lo' and less
	// than `hi'.  (Contrast `trim'.)
	static Subtree split (Subtree subtree, int lo, int hi) {
	    if (subtree == null) return null;
	    else return subtree.split(lo, hi);
	}

	abstract Subtree trim(int lo, int hi);

	// Returns the largest subtree of `subtree' whose root node is greater than `lo'
	// and less than `hi'.  (Contrast `split'.)
	static Subtree trim (Subtree subtree, int lo, int hi) {
	    if (subtree == null) return null;
	    else return subtree.trim(lo, hi);
	}

	abstract Object rankElement(int rank);
	abstract int rankHash(int rank);

	static final class RankTrimResult {
	    RankTrimResult (Subtree _subtree, int _base) {
		subtree = _subtree;
		base = _base;
	    }
	    Subtree subtree;
	    int base;
	}

	RankTrimResult rankTrim (Subtree subtree, int base, int lo, int hi) {
	    while (subtree instanceof Node) {
		Node node = (Node)subtree;
		int nrank = base + size(node.left);
		if (nrank >= lo) {
		    if (nrank < hi) break;
		    else subtree = node.left;
		} else {
		    int rbase = nrank + elementSize(node.element);
		    if (rbase > lo) break;
		    else {
			subtree = node.right;
			base = rbase;
		    }
		}
	    }
	    return new RankTrimResult(subtree, base);
	}

	abstract Object findEquiv(int hash);

	static Object findEquiv (Subtree subtree, int hash) {
	    if (subtree == null) return NO_ELEMENT;
	    else return subtree.findEquiv(hash);
	}

	// Assumes that all elements of `left' are less than `elt', and all elements
	// of `right' are greater than `elt'; returns a new tree containing all these
	// elements.  This does more rebalancing than `buildNode', which otherwise
	// has the same contract.  `elt' may be an `EquivalentSet'.
	static Subtree concat (Object elt, int ehash, Subtree left, Subtree right) {
	    if (left == null) return with(right, elt, ehash);
	    else if (right == null) return with(left, elt, ehash);
	    else {
		int sizl = size(left);
		int sizr = size(right);
		if (left instanceof Node && sizl > sizr * BALANCE_FACTOR) {
		    Node l = (Node)left;
		    return buildNode(l.element, l.hash, l.left,
				     concat(elt, ehash, l.right, right));
		} else if (right instanceof Node && sizr > sizl * BALANCE_FACTOR) {
		    Node r = (Node)right;
		    return buildNode(r.element, r.hash,
				     concat(elt, ehash, left, r.left), r.right);
		} else return buildNode(elt, ehash, left, right);
	    }
	}

	// Returns the union of `left' and `right' under the assumption that all values
	// in `left' are less than any value in `right'.
	static Subtree join (Subtree left, Subtree right) {
	    if (left == null) return right;
	    else if (right == null) return left;
	    else return concat(right.minElement(), right.minHash(), left, right.lessMin());
	}

	static Subtree buildNode (Object elt, int ehash, Subtree left, Subtree right) {
	    if ((left == null || left instanceof Leaf) &&
		(right == null || right instanceof Leaf)) {
		Leaf lleaf = (Leaf)left, rleaf = (Leaf)right;
		if (!(elt instanceof EquivalentSet) &&
		    (left == null ? 0 : lleaf.elements.length) + 1 +
		    (right == null ? 0 : rleaf.elements.length) < MAX_LEAF_ARRAY_LENGTH)
		    return buildLeaf(elt, ehash, lleaf, rleaf);
		else return makeNode(elt, ehash, left, right);
	    } else {
		int sizl = size(left);
		int sizr = size(right);
		// This code is subtly different from Adams' in order to create more
		// opportunities to coalesce adjacent leaf arrays.
		if (right instanceof Node && sizr > sizl * BALANCE_FACTOR) {
		    Node r = (Node)right;
		    Subtree rl = r.left;
		    Subtree rr = r.right;
		    if (rl instanceof Leaf || size(rl) <= size(rr))
			return makeNode(r.element, r.hash,
					buildNode(elt, ehash, left, rl),
					rr);
		    else {
			Node rln = (Node)rl;
			return makeNode(rln.element, rln.hash,
					buildNode(elt, ehash, left, rln.left),
					buildNode(r.element, r.hash, rln.right, rr));
		    }
		} else if (left instanceof Node && sizl > sizr * BALANCE_FACTOR) {
		    Node l = (Node)left;
		    Subtree ll = l.left;
		    Subtree lr = l.right;
		    if (lr instanceof Leaf || size(lr) <= size(ll))
			return makeNode(l.element, l.hash, ll,
					buildNode(elt, ehash, lr, right));
		    else {
			Node lrn = (Node)lr;
			return makeNode(lrn.element, lrn.hash,
					buildNode(l.element, l.hash, ll, lrn.left),
					buildNode(elt, ehash, lrn.right, right));
		    }
		} else return makeNode(elt, ehash, left, right);
	    }
	}

	static int hashCode (Subtree subtree) {
	    if (subtree == null) return 0;
	    else return subtree.hashCode();
	}
    }

    /****************/
    /* Node */
    static final class Node extends Subtree {
	Node (int _size, Object _element, int _hash, Subtree _left, Subtree _right) {
	    size = _size;
	    element = _element;
	    hash = _hash;
	    left = _left;
	    right = _right;
	}
	int size;	// the number of elements in the subtree
	Object element;
	int hash;	// `hashCode(element)', cached
	Subtree left;	// a subtree
	Subtree right;	// a subtree

	int size () { return size; }

	Object arb () {
	    if (element instanceof EquivalentSet)
		return ((EquivalentSet)element).contents.get(0);
	    else return element;
	}

	boolean contains (Object elt, int ehash) {
	    if (ehash == hash) {
		if (element instanceof EquivalentSet)
		    return ((EquivalentSet)element).contents.contains(elt);
		else return elt == null ? element == null : elt.equals(element);
	    } else if (ehash < hash) return contains(left, elt, ehash);
	    else return contains(right, elt, ehash);
	}
	
	Subtree with (Object elt, int ehash) {
	    if (ehash == hash) {
		if (!(elt instanceof EquivalentSet) &&
		    !(element instanceof EquivalentSet) &&
		    (elt == null ? element == null : elt.equals(element)))
		    return this;
		else return makeNode(equivUnion(elt, element), hash,
				     left, right);
	    } else if (ehash < hash) {
		Subtree new_left = with(left, elt, ehash);
		if (new_left == left) return this;
		else return buildNode(element, hash, new_left, right);
	    } else {
		Subtree new_right = with(right, elt, ehash);
		if (new_right == right) return this;
		else return buildNode(element, hash, left, new_right);
	    }
	}

	Subtree less (Object elt, int ehash) {
	    if (ehash == hash) {
		if (!(element instanceof EquivalentSet)) {
		    if (elt == null ? element != null : !elt.equals(element))
			return this;
		    else if (right == null) return left;
		    else if (left == null) return right;
		    else return buildNode(right.minElement(), right.minHash(), left,
					  right.lessMin());
		} else {
		    Object diff = equivDiff(element, elt);
		    return buildNode(diff, hash, left, right);
		}
	    } else if (ehash < hash) {
		Subtree new_left = less(left, elt, ehash);
		if (new_left == left) return this;
		else return buildNode(element, hash, new_left, right);
	    } else {
		Subtree new_right = less(right, elt, ehash);
		if (new_right == right) return this;
		else return buildNode(element, hash, left, new_right);
	    }
	}

	Object minElement () {
	    if (left == null) return element;
	    else return left.minElement();
	}

	int minHash () {
	    if (left == null) return hash;
	    else return left.minHash();
	}

	Subtree lessMin () {
	    if (left == null) return right;
	    else return concat(element, hash, left.lessMin(), right);
	}

	Subtree union (Subtree subtree2, int lo, int hi) {
	    Object elt2 = findEquiv(subtree2, hash);
	    return concat(equivUnion(element, elt2), hash,
			  union(trim(left, lo, hash),
				trim(subtree2, lo, hash),
				lo, hash),
			  union(trim(right, hash, hi),
				trim(subtree2, hash, hi),
				hash, hi));
	}

	Subtree intersection (Subtree subtree2, int lo, int hi) {
	    Object elt2 = findEquiv(subtree2, hash);
	    Subtree new_left = intersection(left, trim(subtree2, lo, hash), lo, hash);
	    Subtree new_right = intersection(right, trim(subtree2, hash, hi), hash, hi);
	    Object isect = equivIntersect(element, elt2);
	    if (isect == NO_ELEMENT) return join(new_left, new_right);
	    else return concat(isect, hash, new_left, new_right);
	}

	Subtree difference (Subtree subtree2, int lo, int hi) {
	    Object elt2 = findEquiv(subtree2, hash);
	    Subtree new_left = difference(left, trim(subtree2, lo, hash), lo, hash);
	    Subtree new_right = difference(right, trim(subtree2, hash, hi), hash, hi);
	    Object diff = equivDiff(element, elt2);
	    if (diff == NO_ELEMENT) return join(new_left, new_right);
	    else return concat(diff, hash, new_left, new_right);
	}

	int compareTo (int base, Subtree subtree2, int base2, int lo, int hi) {
	    int lsize = size(left);
	    int new_hi = base + lsize;
	    RankTrimResult rtr1 = rankTrim(left, base, lo, new_hi);
	    RankTrimResult rtr2 = rankTrim(subtree2, base2, lo, new_hi);
	    int left_comp_res = compareTo(rtr1.subtree, rtr1.base,
					  rtr2.subtree, rtr2.base, lo, new_hi);
	    if (left_comp_res != 0) return left_comp_res;
	    else {
		int hash2 = subtree2.rankHash(new_hi - base2);
		if (hash < hash2) return -1;
		else if (hash > hash2) return 1;
		else {
		    int elt_size = elementSize(element);
		    Object elt2 = subtree2.rankElement(new_hi - base2);
		    int elt2_size = elementSize(elt2);
		    if (elt_size < elt2_size) return 1;
		    else if (elt_size > elt2_size) return -1;
		    else {
			int new_lo = base + lsize + elt_size;
			RankTrimResult rtr3 = rankTrim(right, new_lo, new_lo, hi);
			RankTrimResult rtr4 = rankTrim(subtree2, base2, new_lo, hi);
			return compareTo(rtr3.subtree, rtr3.base,
					 rtr4.subtree, rtr4.base, new_lo, hi);
		    }
		}
	    }
	}

	boolean equals (int base, Subtree subtree2, int base2, int lo, int hi) {
	    int lsize = size(left);
	    int new_hi = base + lsize;
	    RankTrimResult rtr1 = rankTrim(left, base, lo, new_hi);
	    RankTrimResult rtr2 = rankTrim(subtree2, base2, lo, new_hi);
	    if (!equals(rtr1.subtree, rtr1.base, rtr2.subtree, rtr2.base, lo, new_hi))
		return false;
	    else {
		int hash2 = subtree2.rankHash(new_hi - base2);
		if (hash != hash2) return false;
		else {
		    Object elt2 = subtree2.rankElement(new_hi - base2);
		    if (element == null ? elt2 != null : !equivEquals(element, elt2))
			return false;
		    else {
			int elt_size = elementSize(element);
			int new_lo = base + lsize + elt_size;
			RankTrimResult rtr3 = rankTrim(right, new_lo, new_lo, hi);
			RankTrimResult rtr4 = rankTrim(subtree2, base2, new_lo, hi);
			return equals(rtr3.subtree, rtr3.base,
				      rtr4.subtree, rtr4.base, new_lo, hi);
		    }
		}
	    }
	}

	boolean isSubset (Subtree subtree2, int lo, int hi) {
	    if (subtree2 == null) return false;
	    else if (!isSubset(left, trim(subtree2, lo, hash), lo, hash)) return false;
	    else {
		Object elt2 = findEquiv(subtree2, hash);
		if (!equivIsSubset(element, elt2)) return false;
		else return isSubset(right, trim(subtree2, hash, hi), hash, hi);
	    }
	}

	Subtree split (int lo, int hi) {
	    if (hash <= lo) {
		if (hi == POSITIVE_INFINITY && hash == lo) return right;
		else return split(right, lo, hi);
	    } else if (hash >= hi) {
		if (lo == NEGATIVE_INFINITY && hash == hi) return left;
		else return split(left, lo, hi);
	    } else {
		Subtree new_left = split(left, lo, POSITIVE_INFINITY);
		Subtree new_right = split(right, NEGATIVE_INFINITY, hi);
		if (new_left == left && new_right == right) return this;
		else return concat(element, hash, new_left, new_right);
	    }
	}

	Subtree trim (int lo, int hi) {
	    if (hash > lo) {
		if (hash < hi) return this;
		else return trim(left, lo, hi);
	    } else return trim(right, lo, hi);
	}

	Object rankElement (int rank) {
	    int left_size = size(left);
	    if (rank < left_size) return left.rankElement(rank);
	    else {
		int elt_size = elementSize(element);
		if (rank < left_size + elt_size) return element;
		else return right.rankElement(rank - (left_size + elt_size));
	    }
	}

	int rankHash (int rank) {
	    int left_size = size(left);
	    if (rank < left_size) return left.rankHash(rank);
	    else {
		int elt_size = elementSize(element);
		if (rank < left_size + elt_size) return hash;
		else return right.rankHash(rank - (left_size + elt_size));
	    }
	}

	Object findEquiv (int ehash) {
	    if (ehash < hash) return findEquiv(left, ehash);
	    else if (ehash > hash) return findEquiv(right, ehash);
	    else return element;
	}

	public int hashCode () {
	    return hashCode(left) + hashCode(right) +
		   (elementSize(element) * hash);
	}

    }


    /****************/
    /* Leaf */
    static final class Leaf extends Subtree {
	Leaf (Object[] _elements, int[] _hashes) {
	    elements = _elements;
	    hashes = _hashes;
	}
	Object[] elements;
	// You might be tempted to subtract two hash codes and check the sign of the
	// result.  Don't!  The difference may not fit in 32 bits!
	int[] hashes;

	int size () { return elements.length; }

	Object arb () { return elements[elements.length >> 1]; }

	boolean contains (Object elt, int ehash) {
	    int bin_srch_res = binarySearch(hashes, ehash);
	    if ((bin_srch_res & BIN_SEARCH_FOUND_MASK) == BIN_SEARCH_FOUND) {
		Object e = elements[bin_srch_res >> BIN_SEARCH_INDEX_SHIFT];
		return elt == null ? e == null : elt.equals(e);
	    } else return false;
	}

	Subtree with (Object elt, int ehash) {
	    int bin_srch_res = binarySearch(hashes, ehash);
	    int found = bin_srch_res & BIN_SEARCH_FOUND_MASK;
	    int idx = bin_srch_res >> BIN_SEARCH_INDEX_SHIFT;
	    int next = (found == BIN_SEARCH_FOUND ? idx + 1 : idx);
	    int len = elements.length;
	    if (found == BIN_SEARCH_FOUND && !(elt instanceof EquivalentSet) &&
		(elt == null ? elements[idx] == null : elt.equals(elements[idx])))
		return this;
	    else if (found == BIN_SEARCH_NOT_FOUND  &&
		     len + 1 < MAX_LEAF_ARRAY_LENGTH  &&
		     !(elt instanceof EquivalentSet))
		return new Leaf(insertObj(elements, idx, elt),
				insertInt(hashes, idx, ehash));
	    else return makeNode((found == BIN_SEARCH_FOUND
				  ? equivUnion(elements[idx], elt)
				  : elt),
				 ehash,
				 makeLeaf(subseqObj(elements, 0, idx),
					  subseqInt(hashes, 0, idx)),
				 makeLeaf(subseqObj(elements, next, len),
					  subseqInt(hashes, next, len)));
	}

	Subtree less (Object elt, int ehash) {
	    int bin_srch_res = binarySearch(hashes, ehash);
	    int found = bin_srch_res & BIN_SEARCH_FOUND_MASK;
	    int idx = bin_srch_res >> BIN_SEARCH_INDEX_SHIFT;
	    if (found == BIN_SEARCH_FOUND) {
		if (elt == null ? elements[idx] == null : elt.equals(elements[idx]))
		    return makeLeaf(removeObj(elements, idx),
				    removeInt(hashes, idx));
		else return this;
	    } else return this;
	}

	Object minElement () { return elements[0]; }

	int minHash () { return hashes[0]; }

	Subtree lessMin () {
	    return makeLeaf(subseqObj(elements, 1, elements.length),
			    subseqInt(hashes, 1, hashes.length));
	}

	Subtree union (Subtree subtree2, int lo, int hi) {
	    // Too bad Java doesn't have multi-methods.
	    if (subtree2 instanceof Leaf) {
		Leaf leaf2 = ((Leaf)subtree2);
		return union(elements, hashes, leaf2.elements, leaf2.hashes,
			     lo, hi);
	    } else return subtree2.union(this, lo, hi);
	}

	Subtree intersection (Subtree subtree2, int lo, int hi) {
	    if (subtree2 instanceof Leaf) {
		Leaf leaf2 = (Leaf) subtree2;
		return intersection(elements, hashes, leaf2.elements, leaf2.hashes,
				    lo, hi);
	    } else return intersection(trim(subtree2, lo, hi), this, lo, hi);
	}

	Subtree difference (Subtree subtree2, int lo, int hi) {
	    if (subtree2 instanceof Leaf) {
		Leaf leaf2 = (Leaf)subtree2;
		return difference(elements, hashes, leaf2.elements, leaf2.hashes,
				  lo, hi);
	    } else {
		// Can't use the reversing trick here because difference is not
		// commutative.  A multi-method would be particularly handy here.
		Node node2 = (Node)subtree2;
		int hash2 = node2.hash;
		Object elt1 = findEquiv(hash2);
		Subtree new_left = difference(trim(lo, hash2),
					      trim(node2.left, lo, hash2),
					      lo, hash2);
		Subtree new_right = difference(trim(hash2, hi),
					       trim(node2.right, hash2, hi),
					       hash2, hi);
		Object diff = equivDiff(elt1, node2.element);
		if (diff == NO_ELEMENT) return join(new_left, new_right);
		else return concat(diff, hash2, new_left, new_right);
	    }
	}

	int compareTo (int base, Subtree subtree2, int base2, int lo, int hi) {
	    if (subtree2 instanceof Leaf) {
		Leaf leaf2 = (Leaf)subtree2;
		for (int i = lo; i < hi; ++i) {
		    int hash1 = hashes[i - base];
		    int hash2 = leaf2.hashes[i - base2];
		    if (hash1 < hash2) return -1;
		    else if (hash1 > hash2) return 1;
		}
		return 0;
	    } else return - subtree2.compareTo(base2, this, base, lo, hi);
	}

	boolean equals (int base, Subtree subtree2, int base2, int lo, int hi) {
	    if (subtree2 instanceof Leaf) {
		Leaf leaf2 = (Leaf)subtree2;
		for (int i = lo; i < hi; ++i) {
		    if (hashes[i - base] != leaf2.hashes[i - base2]) return false;
		    Object elt = elements[i - base], elt2 = leaf2.elements[i - base2];
		    if (elt == null ? elt2 != null : !elt.equals(elt2)) return false;
		}
		return true;
	    } else return equals(subtree2, base2, this, base, lo, hi);
	}

	boolean isSubset (Subtree subtree2, int lo, int hi) {
	    if (subtree2 == null) return isSubset(elements, hashes, null, null, lo, hi);
	    else if (subtree2 instanceof Leaf) {
		Leaf leaf2 = (Leaf)subtree2;
		return isSubset(elements, hashes, leaf2.elements, leaf2.hashes, lo, hi);
	    } else {
		Node node2 = (Node)subtree2;
		int hash2 = node2.hash;
		if (!isSubset(trim(lo, hash2), node2.left, lo, hash2)) return false;
		else {
		    Object elt1 = findEquiv(hash2);
		    if (!equivIsSubset(elt1, node2.element)) return false;
		    else return isSubset(trim(hash2, hi), node2.right, hash2, hi);
		}
	    }
	}

	Subtree split (int lo, int hi) {
	    int len = elements.length;
	    int lo_split = (lo == NEGATIVE_INFINITY ? 0 : binarySearchLo(hashes, lo));
	    int hi_split = (hi == POSITIVE_INFINITY ? len : binarySearchHi(hashes, hi));
	    if (lo_split >= hi_split) return null;
	    else if (lo_split == 0 && hi_split == len) return this;
	    else return makeLeaf(subseqObj(elements, lo_split, hi_split),
				 subseqInt(hashes, lo_split, hi_split));
	}

	Subtree trim (int lo, int hi) {
	    // If the array is completely out of range, drop it.
	    if (hashes[0] >= hi || hashes[hashes.length - 1] <= lo)
		return null;
	    else return this;
	}

	Object rankElement (int rank) { return elements[rank]; }

	int rankHash (int rank) { return hashes[rank]; }

	Object findEquiv (int ehash) {
	    int bin_srch_res = binarySearch(hashes, ehash);
	    if ((bin_srch_res & BIN_SEARCH_FOUND_MASK) == BIN_SEARCH_FOUND)
		return elements[bin_srch_res >> BIN_SEARCH_INDEX_SHIFT];
	    else return NO_ELEMENT;
	}

	public int hashCode () {
	    int result = 0, len = hashes.length;
	    for (int i = 0; i < len; ++i) result += hashes[i];
	    return result;
	}

	/* Internal array manipulation routines.  These all assume their index
	 * parameters are within bounds.  Of course, despite what the names of some
	 * might suggest, they all make new arrays. */

	static Object[] insertObj (Object[] ary, int idx, Object elt) {
	    int len = ary.length + 1;
	    Object[] a = new Object[len];
	    for (int i = 0; i < idx; ++i) a[i] = ary[i];
	    a[idx] = elt;
	    for (int i = idx + 1; i < len; ++i) a[i] = ary[i - 1];
	    return a;
	}

	static int[] insertInt (int[] ary, int idx, int elt) {
	    int len = ary.length + 1;
	    int[] a = new int[len];
	    for (int i = 0; i < idx; ++i) a[i] = ary[i];
	    a[idx] = elt;
	    for (int i = idx + 1; i < len; ++i) a[i] = ary[i - 1];
	    return a;
	}

	static Object[] removeObj (Object[] ary, int idx) {
	    int len = ary.length - 1;
	    if (len == 0) return null;
	    else {
		Object[] a = new Object[len];
		for (int i = 0; i < idx; ++i) a[i] = ary[i];
		for (int i = idx; i < len; ++i) a[i] = ary[i + 1];
		return a;
	    }
	}

	static int[] removeInt (int[] ary, int idx) {
	    int len = ary.length - 1;
	    if (len == 0) return null;
	    else {
		int[] a = new int[len];
		for (int i = 0; i < idx; ++i) a[i] = ary[i];
		for (int i = idx; i < len; ++i) a[i] = ary[i + 1];
		return a;
	    }
	}

	static Object[] subseqObj (Object[] ary, int lo, int hi) {
	    if (lo >= hi) return null;
	    else {
		int len = hi - lo;
		Object[] a = new Object[len];
		for (int i = 0; i < len; ++i)
		    a[i] = ary[i + lo];
		return a;
	    }
	}

	static int[] subseqInt (int[] ary, int lo, int hi) {
	    if (lo >= hi) return null;
	    else {
		int len = hi - lo;
		int[] a = new int[len];
		for (int i = 0; i < len; ++i)
		    a[i] = ary[i + lo];
		return a;
	    }
	}

	static Subtree union (Object[] elements1, int[] hashes1,
			      Object[] elements2, int[] hashes2,
			      int lo, int hi) {
	    int i1 = 0, i2 = 0, len1 = elements1.length, len2 = elements2.length;
	    // We do these with linear rather than binary search because frequently,
	    // the ends of the vectors will already be in range (the worst case for
	    // binary search).
	    while (i1 < len1 && lo >= hashes1[i1]) ++i1;
	    while (i2 < len2 && lo >= hashes2[i2]) ++i2;
	    while (i1 < len1 && hi <= hashes1[len1 - 1]) --len1;
	    while (i2 < len2 && hi <= hashes2[len2 - 1]) --len2;
	    // The version in `PureTreeSet' uses an `ArrayList', but we have to use
	    // an `int[]' for the hashes, so we may as well use arrays for both.
	    Object[] elements = new Object[(len1 - i1) + (len2 - i2)];
	    int[] hashes = new int[(len1 - i1) + (len2 - i2)];
	    int nelts = 0;
	    boolean any_equiv = false;
	    while (true) {
		if (i1 == len1) {
		    while (i2 < len2) {
			elements[nelts] = elements2[i2];
			hashes[nelts++] = hashes2[i2];
			++i2;
		    }
		    break;
		} else if (i2 == len2) {
		    while (i1 < len1) {
			elements[nelts] = elements1[i1];
			hashes[nelts++] = hashes1[i1];
			++i1;
		    }
		    break;
		} else {
		    Object e1 = elements1[i1], e2 = elements2[i2];
		    int h1 = hashes1[i1], h2 = hashes2[i2];
		    if (h1 < h2) {
			elements[nelts] = e1;
			hashes[nelts++] = h1;
			++i1;
		    } else if (h1 > h2) {
			elements[nelts] = e2;
			hashes[nelts++] = h2;
			++i2;
		    } else {
			if (e1 == null ? e2 == null : e1.equals(e2)) {
			    elements[nelts] = e1;
			    hashes[nelts++] = h1;
			} else {
			    elements[nelts] = equivUnion(e1, e2);
			    hashes[nelts++] = h1;
			    any_equiv = true;
			}
			++i1;
			++i2;
		    }
		}
	    }
	    if (any_equiv) {
		Subtree t = null;
		// We could attempt a better algorithm, but this shouldn't happen often.
		for (int i = 0; i < nelts; ++i)
		    t = with(t, elements[i], hashes[i]);
		return t;
	    } else {
		if (nelts > MAX_LEAF_ARRAY_LENGTH) {
		    int idx = nelts >> 1;
		    return makeNode(elements[idx], hashes[idx],
				    makeLeaf(subseqObj(elements, 0, idx),
					     subseqInt(hashes, 0, idx)),
				    makeLeaf(subseqObj(elements, idx + 1, nelts),
					     subseqInt(hashes, idx + 1, nelts)));
		} else return makeLeaf(subseqObj(elements, 0, nelts),
				       subseqInt(hashes, 0, nelts));
	    }
	}

	static Subtree intersection (Object[] elements1, int[] hashes1,
				     Object[] elements2, int[] hashes2,
				     int lo, int hi) {
	    int i1 = 0, i2 = 0, len1 = elements1.length, len2 = elements2.length;
	    while (i1 < len1 && lo >= hashes1[i1]) ++i1;
	    while (i1 < len1 && hi <= hashes1[len1 - 1]) --len1;
	    Object[] elements = new Object[len1 - i1];
	    int[] hashes = new int[len1 - i1];
	    int nelts = 0;
	    while (i1 < len1 && i2 < len2) {
		int h1 = hashes1[i1], h2 = hashes2[i2];
		if (h1 < h2) ++i1;
		else if (h1 > h2) ++i2;
		else {
		    Object e1 = elements1[i1], e2 = elements2[i2];
		    if (e1 == null ? e2 == null : e1.equals(e2)) {
			elements[nelts] = e1;
			hashes[nelts++] = h1;
		    }
		    ++i1;
		    ++i2;
		}
	    }
	    if (nelts == 0) return null;
	    else return new Leaf(subseqObj(elements, 0, nelts),
				 subseqInt(hashes, 0, nelts));
	}

	static Subtree difference (Object[] elements1, int[] hashes1,
				   Object[] elements2, int[] hashes2,
				   int lo, int hi) {
	    int i1 = 0, i2 = 0, len1 = elements1.length, len2 = elements2.length;
	    while (i1 < len1 && lo >= hashes1[i1]) ++i1;
	    while (i1 < len1 && hi <= hashes1[len1 - 1]) --len1;
	    Object[] elements = new Object[len1 - i1];
	    int[] hashes = new int[len1 - i1];
	    int nelts = 0;
	    while (i1 < len1) {
		Object e1 = elements1[i1];
		int h1 = hashes1[i1], h2 = i2 < len2 ? hashes2[i2] : POSITIVE_INFINITY;
		if (h1 < h2) {
		    elements[nelts] = e1;
		    hashes[nelts++] = h1;
		    ++i1;
		} else if (h1 > h2) ++i2;
		else {
		    if (e1 == null ? elements2[i2] != null : !e1.equals(elements2[i2])) {
			elements[nelts] = e1;
			hashes[nelts++] = h1;
		    }
		    ++i1;
		    ++i2;
		}
	    }
	    if (nelts == 0) return null;
	    else return new Leaf(subseqObj(elements, 0, nelts),
				 subseqInt(hashes, 0, nelts));
	}

	// `elements2' and `hashes2' may be null (at the same time).
	static boolean isSubset (Object[] elements1, int[] hashes1,
				 Object[] elements2, int[] hashes2,
				 int lo, int hi) {
	    int i1 = 0, i2 = 0, len1 = elements1.length;
	    int len2 = elements2 == null ? 0 : elements2.length;
	    while (i1 < len1 && lo >= hashes1[i1]) ++i1;
	    while (i1 < len1 && hi <= hashes1[len1 - 1]) --len1;
	    while (i1 < len1 && i2 < len2) {
		Object e1 = elements1[i1];
		int h1 = hashes1[i1], h2 = hashes2[i2];
		if (h1 < h2) return false;
		else if (h1 > h2) ++i2;
		else {
		    Object e2 = elements2[i2];
		    if (e1 == null ? e2 != null : !e1.equals(e2)) return false;
		    ++i1;
		    ++i2;
		}
	    }
	    return !(i1 < len1);
	}

	/* Binary search returns two pieces of information: (1) whether the element or
	 * an equivalent one was found; and (2) the index at which the element (or an
	 * equivalent one) either was found or should be inserted.  Since these are both
	 * small integers, we pack them into a single `int', to save us from having to
	 * allocate a result object.  [Editorial: Java needs multiple values very
	 * badly!] */
	static final int BIN_SEARCH_NOT_FOUND = 0;
	static final int BIN_SEARCH_FOUND = 1;
	static final int BIN_SEARCH_FOUND_MASK = 0x1;
	static final int BIN_SEARCH_INDEX_SHIFT = 1;

	static int binarySearch (int[] hashes, int ehash) {
	    int lo = 0, hi = hashes.length - 1;
	    while (lo <= hi) {
		int mid = (lo + hi) / 2;
		int hash = hashes[mid];
		if (ehash == hash)
		    return (mid << BIN_SEARCH_INDEX_SHIFT) | BIN_SEARCH_FOUND;
		else if (ehash < hash) hi = mid - 1;
		else lo = mid + 1;
	    }
	    return (lo << BIN_SEARCH_INDEX_SHIFT) | BIN_SEARCH_NOT_FOUND;
	}

	// Returns the index of the left edge of the first member of `hashes' that is
	// above `lo'.
	static int binarySearchLo (int[] hashes, int lo) {
	    int bin_srch_res = binarySearch(hashes, lo);
	    int idx = bin_srch_res >> BIN_SEARCH_INDEX_SHIFT;
	    if ((bin_srch_res & BIN_SEARCH_FOUND_MASK) == BIN_SEARCH_FOUND)
		return idx + 1;
	    else return idx;
	}

	// Returns the index of the right edge of the last member of `hashes' that is
	// below `hi'.
	static int binarySearchHi (int[] hashes, int hi) {
	    int bin_srch_res = binarySearch(hashes, hi);
	    return bin_srch_res >> BIN_SEARCH_INDEX_SHIFT;
	}

    }

    /* The maximum size to which we let the equality cache grow. */
    private static final int EQUALITY_CACHE_MAX = 4;

    protected void cacheEqual (PureSet other) {
	int len = equality_cache == null ? 0 : equality_cache.length;
	for (int i = 0; i < len; ++i) {
	    WeakReference r = equality_cache[i];
	    if (r == null || r.get() == null) --len;
	}
	int new_len = Math.min(len + 1, EQUALITY_CACHE_MAX);
	WeakReference ref = new WeakReference(other);
	WeakReference[] ary = new WeakReference[new_len];
	ary[0] = ref;
	for (int i = 0, j = 1; i < len && j < new_len; ++i) {
	    WeakReference r = equality_cache[i];
	    // We can wind up with a null entry in the array if a 'WeakReference'
	    // is cleared between the test a few lines above and this one.  In that
	    // case we won't fill up the new array with `WeakReference's.
	    if (r != null && r.get() != null) ary[j++] = r;
	}
	// We don't bother synchronizing; if two threads do this at the same time,
	// we'll simply lose one of the cache entries.  No big deal.
	equality_cache = ary;
    }

    private boolean cachedEquals (Object obj) {
	if (equality_cache != null) {
	    for (int i = 0, len = equality_cache.length; i < len; ++i) {
		WeakReference r = equality_cache[i];
		if (r != null && r.get() == obj) return true;
	    }
	}
	return false;
    }

    private static String dump (Object thing) {
	if (thing == null) return "[null]";
	else if (thing == NO_ELEMENT) return "NADA";
	else if (thing instanceof EquivalentSet) {
	    ArrayList al = ((EquivalentSet)thing).contents;
	    String res = "[";
	    for (int i = 0, size = al.size(); i < size; ++i) {
		if (i > 0) res = res + ", ";
		res = res + al.get(i);
	    }
	    return res + "]";
	} else if (thing instanceof Leaf) {
	    Leaf leaf = (Leaf)thing;
	    StringBuffer str_buf = new StringBuffer("{");
	    for (int i = 0; i < leaf.elements.length; ++i) {
		str_buf.append(dump(leaf.elements[i]) +
			       "(" + dump(leaf.hashes[i]) + ")");
		if (i < leaf.elements.length - 1) str_buf.append(", ");
	    }
	    str_buf.append("}");
	    return str_buf.toString();
	} else if (thing instanceof Node) {
	    Node node = (Node)thing;
	    return "(" + node.size + ", " +
		dump(node.element) + "(" + node.hash + ");\n" +
		indent(dump(node.left), "  ") + ",\n" +
		indent(dump(node.right), "  ") + ")";
	} else return thing.toString();
    }

    private static String dump (int thing) {
	if (thing == NEGATIVE_INFINITY) return "-oo";
	else if (thing == POSITIVE_INFINITY) return "+oo";
	else return "" + thing;
    }

    private static String indent (String str, String prefix) {
	StringBuffer res = new StringBuffer(prefix);
	for (int i = 0, len = str.length(); i < len; ++i) {
	    char c = str.charAt(i);
	    res.append(c);
	    if (c == '\n' && i < len - 1) res.append(prefix);
	}
	return res.toString();
    }

    private boolean verify (Subtree subtree, int lo, int hi) {
	if (subtree == null) return true;
	else if (subtree instanceof Leaf) {
	    Leaf leaf = (Leaf)subtree;
	    int prev = lo;
	    for (int i = 0, len = leaf.elements.length; i < len; ++i) {
		int hash = leaf.hashes[i];
		if (leaf.elements[i] instanceof EquivalentSet) return false;
		if (prev >= hash) return false;
		prev = hash;
	    }
	    return prev < hi;
	} else {
	    Node node = (Node)subtree;
	    int sizl = Subtree.size(node.left);
	    int sizr = Subtree.size(node.right);
	    if (node.size != sizl + sizr + elementSize(node.element)) return false;
	    if (node.element instanceof EquivalentSet &&
		((EquivalentSet)node.element).contents.size() < 2) return false;
	    // Small subtrees may be unbalanced, because of our array-splitting heuristic
	    // and because of EquivalentSets.  As long as the size of any subtree that
	    // can be unbalanced is strictly bounded, we're still okay.
	    if ((sizr > 4 && sizl > sizr * BALANCE_FACTOR) ||
		(sizl > 4 && sizr > sizl * BALANCE_FACTOR))
		return false;
	    return verify(node.left, lo, node.hash) &&
		   verify(node.right, node.hash, hi);
	}
    }

    /****************/
    /* Equivalent sets */

    /* In the case where the set contains two or more elements which are distinct
     * but are equivalent according to the comparison method, the `element' field of
     * a `Node' will contain one of these.  We always build a `Node' in this case;
     * we never put equivalent elements in an array.  Here we don't worry about
     * the extra level(s) of structure because this is supposed to be a rare case.
     */
    static final class EquivalentSet {
	EquivalentSet(ArrayList _contents) {
	    contents = _contents;
	}
	ArrayList contents;
    }

    private static Object equivUnion (Object elt1, Object elt2) {
	if (elt1 == NO_ELEMENT) return elt2;
	else if (elt2 == NO_ELEMENT) return elt1;
	else if (elt1 instanceof EquivalentSet) {
	    ArrayList al1 = ((EquivalentSet)elt1).contents;
	    if (elt2 instanceof EquivalentSet) {
		ArrayList al2 = ((EquivalentSet)elt2).contents;
		int size2 = al2.size();
		ArrayList res_al = (ArrayList)al1.clone();
		for (int i = 0; i < size2; ++i) {
		    Object e2 = al2.get(i);
		    if (!res_al.contains(e2)) res_al.add(e2);
		}
		res_al.trimToSize();
		return new EquivalentSet(res_al);
	    } else {
		if (al1.contains(elt2)) return elt1;
		else {
		    // Too bad there's no `clone' method that accepts a capacity.
		    ArrayList res_al = (ArrayList)al1.clone();
		    res_al.add(elt2);
		    res_al.trimToSize();
		    return new EquivalentSet(res_al);
		}
	    }
	} else if (elt2 instanceof EquivalentSet) {
	    ArrayList al2 = ((EquivalentSet)elt2).contents;
	    if (al2.contains(elt1)) return elt2;
	    else {
		ArrayList al = (ArrayList)al2.clone();
		al.add(elt1);
		al.trimToSize();
		return new EquivalentSet(al);
	    }
	} else if (elt1 == null ? elt2 == null : elt1.equals(elt2)) return elt1;
	else {
	    ArrayList al = new ArrayList(2);
	    al.add(elt1);
	    al.add(elt2);
	    return new EquivalentSet(al);
	}
    }

    private static Object equivIntersect (Object elt1, Object elt2) {
	// (not sure we use this first case)
	if (elt1 == NO_ELEMENT || elt2 == NO_ELEMENT) return NO_ELEMENT;
	else if (elt1 instanceof EquivalentSet) {
	    ArrayList al1 = ((EquivalentSet)elt1).contents;
	    if (elt2 instanceof EquivalentSet) {
		ArrayList al2 = ((EquivalentSet)elt2).contents;
		ArrayList al = new ArrayList();
		int size1 = al1.size();
		for (int i = 0; i < size1; ++i) {
		    Object e1 = al1.get(i);
		    if (al2.contains(e1)) al.add(e1);
		}
		if (al.size() == 0) return NO_ELEMENT;
		else if (al.size() == 1) return al.get(0);
		else {
		    al.trimToSize();
		    return new EquivalentSet(al);
		}
	    } else {
		if (al1.contains(elt2)) return elt2;
		else return NO_ELEMENT;
	    }
	} else if (elt2 instanceof EquivalentSet) {
	    ArrayList al2 = ((EquivalentSet)elt2).contents;
	    if (al2.contains(elt1)) return elt1;
	    else return NO_ELEMENT;
	} else if (elt1 == null ? elt2 == null : elt1.equals(elt2)) return elt1;
	else return NO_ELEMENT;
    }

    private static Object equivDiff (Object elt1, Object elt2) {
	if (elt1 == NO_ELEMENT) return NO_ELEMENT;
	else if (elt2 == NO_ELEMENT) return elt1;
	else if (elt1 instanceof EquivalentSet) {
	    ArrayList al1 = ((EquivalentSet)elt1).contents;
	    ArrayList al2 = null;
	    if (elt2 instanceof EquivalentSet)
		al2 = ((EquivalentSet)elt2).contents;
	    ArrayList al = new ArrayList();
	    int size1 = al1.size();
	    for (int i = 0; i < size1; ++i) {
		Object e1 = al1.get(i);
		if (al2 == null ?
		    (e1 == null ? elt2 != null : !e1.equals(elt2)) :
		    (!al2.contains(e1)))
		    al.add(e1);
	    }
	    if (al.size() == 0) return NO_ELEMENT;
	    else if (al.size() == 1) return al.get(0);
	    else {
		al.trimToSize();
		return new EquivalentSet(al);
	    }
	} else if (elt2 instanceof EquivalentSet) {
	    if (!((EquivalentSet)elt2).contents.contains(elt1))
		return elt1;
	    else return NO_ELEMENT;
	} else if (elt1 == null ? elt2 != null : !elt1.equals(elt2))
	    return elt1;
	else return NO_ELEMENT;
    }

    private static boolean equivEquals (Object elt1, Object elt2) {
	if (elt1 == elt2) return true;
	else if (elt1 == null || elt2 == null) return false;
	else if (elt1 instanceof EquivalentSet) {
	    if (elt2 instanceof EquivalentSet) {
		ArrayList al1 = ((EquivalentSet)elt1).contents;
		ArrayList al2 = ((EquivalentSet)elt2).contents;
		int size1 = al1.size();
		if (size1 != al2.size()) return false;
		else {
		    for (int i = 0; i < size1; ++i)
			if (!al2.contains(al1.get(i))) return false;
		    return true;
		}
	    } else return false;
	} else if (elt2 instanceof EquivalentSet) return false;
	else return elt1.equals(elt2);
    }

    private static boolean equivIsSubset (Object elt1, Object elt2) {
	if (elt1 == elt2) return true;
	else if (elt1 == NO_ELEMENT) return true;
	else if (elt2 == NO_ELEMENT) return false;
	else if (elt1 instanceof EquivalentSet) {
	    ArrayList al1 = ((EquivalentSet)elt1).contents;
	    if (elt2 instanceof EquivalentSet) {
		ArrayList al2 = ((EquivalentSet)elt2).contents;
		return al2.containsAll(al1);
	    } else return false;
	} else if (elt2 instanceof EquivalentSet) {
	    ArrayList al2 = ((EquivalentSet)elt2).contents;
	    return al2.contains(elt1);
	} else return elt1 == null ? elt2 == null : elt1.equals(elt2);
    }

    /****************/
    // Iterator class

    private static final class PCHSIterator implements Iterator {

	private static final class IteratorNode {
	    IteratorNode (Subtree _subtree, int _index, IteratorNode _parent) {
		subtree = _subtree;
		index = _index;
		parent = _parent;
	    }
	    Subtree subtree;
	    int index;
	    IteratorNode parent;
	}

	private IteratorNode inode;

	PCHSIterator(Subtree subtree) {
	    inode = new IteratorNode(subtree, 0, null);
	    canonicalize();
	}

	private void canonicalize () {
	    while (true) {
		if (inode == null) break;
		else if (inode.subtree == null) {
		    inode = inode.parent;
		    if (inode == null) break;
		    else ++inode.index;
		} else if (inode.subtree instanceof Leaf) {
		    if (inode.index < ((Leaf)inode.subtree).elements.length) break;
		    else {
			inode = inode.parent;
			if (inode == null) break;
			else ++inode.index;
		    }
		} else {
		    Node node = (Node)inode.subtree;
		    if (inode.index == 0) inode = new IteratorNode(node.left, 0, inode);
		    else if (inode.index == elementSize(node.element) + 1)
			inode = new IteratorNode(node.right, 0, inode.parent);
		    else break;
		}
	    }
	}

	public boolean hasNext () {
	    return inode != null;
	}

	public Object next () {
	    Object elt;
	    if (inode == null) throw new NoSuchElementException();
	    else if (inode.subtree instanceof Leaf)
		elt = ((Leaf)inode.subtree).elements[inode.index];
	    else {
		Node node = (Node)inode.subtree;
		if (node.element instanceof EquivalentSet) {
		    ArrayList al = ((EquivalentSet)node.element).contents;
		    elt = al.get(inode.index - 1);
		} else elt = node.element;
	    }
	    inode.index++;
	    canonicalize();
	    return elt;
	}

	public void remove () {
	    throw new UnsupportedOperationException();
	}
    }

    /**
     * Saves the state of this <code>PureCachedHashSet</code> to a stream.
     *
     * @serialData Emits the internal data of the set, including the
     * comparator it uses; the size of the set [<code>int</code>]; and the
     * elements in order [<code>Object</code>s].
     */
    private void writeObject (java.io.ObjectOutputStream strm)
        throws java.io.IOException {
	strm.defaultWriteObject();	// currently does nothing
        strm.writeInt(size());
	for (Iterator it=iterator(); it.hasNext(); )
            strm.writeObject(it.next());
    }

    /**
     * Reconstitutes the <code>PureCachedHashSet</code> instance from a stream.
     */
    private void readObject (java.io.ObjectInputStream strm)
        throws java.io.IOException, ClassNotFoundException {
	strm.defaultReadObject();	// currently does nothing
        int size = strm.readInt();
	Object[] ary = new Object[size];
	for (int i = 0; i < size; ++i)
	    ary[i] = strm.readObject();
	tree = new PureTreeList(ary).toPureCachedHashSet().tree;
    }

}

