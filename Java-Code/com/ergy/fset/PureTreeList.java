/*
 * PureTreeList.java
 *
 * Copyright (c) 2013, 2014 Scott L. Burson.
 *
 * This file is licensed under the Library GNU Public License (LGPL), v. 2.1.
 */


package com.ergy.fset;
import java.util.*;

/**
 * A pure list implemented as a tree.
 *
 * <p>Time costs: <code>isEmpty</code> and <code>size</code> take O(1) (constant)
 * time.  <code>get</code>, <code>with</code>, <code>insert</code>,
 * <code>prepend</code>, <code>append</code>, <code>delete</code>,
 * <code>subseq</code>, <code>concat</code>, and <code>subList</code> take O(log
 * <i>n</i>) time.  Construction from an existing collection or array,
 * <code>reverse</code>, <code>contains</code>, <code>indexOf</code>,
 * <code>lastIndexOf</code>, <code>compareTo</code>, and <code>equals</code> take
 * O(n) (linear) time.  <code>sort</code> takes O(<i>n</i> log <i>n</i>) time.
 *
 * <p>Space costs: <code>PureTreeSet</code> uses a heterogeneous binary tree
 * structure with bounded-length arrays at the leaves.  It uses much less space than
 * traditional homogeneous binary trees; typical space consumption is roughly twice
 * that of a plain array, or even less.
 *
 * <p><code>PureTreeList</code> accepts the null element.
 *
 * <p><code>PureTreeList</code> implements {@link RandomAccess}, even though the
 * rule-of-thumb criterion suggested in the documentation for
 * <code>RandomAccess</code> is not satisfied: iterating through the list with the
 * iterator takes linear time, but doing so with <code>elt</code> takes O(<i>n</i>
 * log <i>n</i>) time.  However, this criterion does not correspond well to the
 * decision facing an implementor of a List algorithm.  Given a sequential-access
 * list like {@link LinkedList}, it is imperative to use the
 * <code>ListIterator</code> for all accesses, to avoid quadratic behavior; but with
 * <code>PureTreeList</code>, it is much better to use <code>elt</code> unless the
 * access pattern really is sequential.  For a good example, see the code for {@link
 * Collections.binarySearch} in the J2SDK sources.
 *
 * @author Scott L. Burson, Sympoiesis, Inc.
 * @see PureList
 */

public class PureTreeList<Elt>
    extends AbstractPureList<Elt>
    implements Comparable<PureTreeList<Elt>>, java.io.Serializable
{

    /**
     * Constructs a new, empty <code>PureTreeList</code>, whose
     * <code>compareTo</code> method uses the natural ordering of its elements.
     */
    public PureTreeList() {
	tree = null;
	elt_comp = null;
    }

    /**
     * Constructs a new, empty <code>PureTreeList</code>, whose
     * <code>compareTo</code> method uses the supplied <code>Comparator</code> to
     * compare elements.
     *
     * @param c the comparator
     */
    public PureTreeList(Comparator<? super Elt> c) {
	tree = null;
	elt_comp = (Comparator<Elt>)c;		// we know Comparator is pure
    }

    /**
     * Constructs a new <code>PureTreeList</code> containing the elements of
     * <code>coll</code>, and whose <code>compareTo</code> method uses the natural
     * ordering of its elements.
     *
     * @param coll the collection to use the elements of
     */
    public PureTreeList(Collection<? extends Elt> coll) {
	elt_comp = null;
	if (coll instanceof PureTreeList) tree = ((PureTreeList)coll).tree;
	else tree = fromCollection(coll);
    }

    /**
     * Constructs a new <code>PureTreeList</code> containing the elements of
     * <code>coll</code>, and whose <code>compareTo</code> method uses the supplied
     * <code>Comparator</code> to compare elements.
     *
     * @param coll the collection to use the elements of
     * @param c the comparator
     */
    public PureTreeList(Collection<? extends Elt> coll, Comparator<? super Elt> c) {
	elt_comp = (Comparator<Elt>)c;
	if (coll instanceof PureTreeList) tree = ((PureTreeList)coll).tree;
	else tree = fromCollection(coll);
    }

    /**
     * Constructs a new <code>PureTreeList</code> containing the components of
     * <code>ary</code>, and whose <code>compareTo</code> method uses the natural
     * ordering of its elements.  That is, the elements are <code>ary[0]</code>,
     * <code>ary[1]</code>, etc.  (So, if <code>ary</code> is multidimensional, the
     * elements of the list will be the inner arrays, not the objects which are
     * ultimately found after multiple levels of indexing.)
     *
     * @param ary the array
     */
    public <T extends Elt> PureTreeList(T[] ary) {
	elt_comp = null;
	tree = fromCollection(ary);
    }

    /**
     * Constructs a new <code>PureTreeList</code> containing the components of
     * <code>ary</code>, and whose <code>compareTo</code> method uses the supplied
     * comparator to compare elements.  That is, the elements are
     * <code>ary[0]</code>, <code>ary[1]</code>, etc.  (So, if <code>ary</code> is
     * multidimensional, the elements of the list will be the inner arrays, not the
     * objects which are ultimately found after multiple levels of indexing.)
     *
     * @param ary the array
     * @param c the comparator
     */
    public <T extends Elt> PureTreeList(T[] ary, Comparator<? super Elt> c) {
	elt_comp = (Comparator<Elt>)c;
	tree = fromCollection(ary);
    }

    Object fromCollection(Object coll_or_array) {
	// Wicked clever linear-time, garbage-free algorithm for tree construction.
	int siz;
	Iterator coll_it = null;
	Object[] array = null;
	if (coll_or_array instanceof Object[]) {
	    array = (Object[])coll_or_array;
	    siz = array.length;
	} else {
	    Collection coll = (Collection)coll_or_array;
	    siz = coll.size();
	    coll_it = coll.iterator();
	}
	if (siz == 0) return null;
	int npieces = ((siz / MAX_LEAF_ARRAY_LENGTH) +		// ceiling(siz, MAX...)
		       (siz % MAX_LEAF_ARRAY_LENGTH) > 0 ? 1 : 0);
	int piece_len = siz / npieces;
	int rmdr = siz % npieces;
	int base = 0;
	// We use an `ArrayList' rather than a `Stack' because the latter is
	// synchronized, which we don't need.
	ArrayList<Object> stack = new ArrayList<Object>();
	for (int ipiece = 0; ipiece < npieces; ++ipiece) {
	    int pl = ipiece < rmdr ? piece_len + 1 : piece_len;
	    Object[] piece = new Object[pl];
	    if (coll_it != null)
		for (int i = 0; i < pl; ++i) piece[i] = coll_it.next();
	    else for (int i = 0; i < pl; ++i) piece[i] = array[i + base];
	    base += pl;
	    stack.add(piece);
	    // Since we're building a binary tree, we use the binary representation
	    // of an integer to guide us.  The number of nodes we need to build is
	    // equal to the number of low-order 1 bits in `ipiece'.
	    for (int i = ipiece; (i & 1) == 1; i >>= 1) {
		Object right = stack.remove(stack.size() - 1);	// `stack.pop()'
		Object left = stack.remove(stack.size() - 1);
		stack.add(makeNode(left, right));
	    }
	}
	while (stack.size() > 1) {
	    Object right = stack.remove(stack.size() - 1);
	    Object left = stack.remove(stack.size() - 1);
	    stack.add(makeNode(left, right));
	}
	return stack.get(0);
    }

    // Used by the `PureTreeSet(Collection[, Comparator])' constructors.
    PureTreeSet<Elt> toPureTreeSet(Comparator<? super Elt> comp) {
	Object t = toPureTreeSet(tree, new PureTreeSet<Elt>(comp));
	return new PureTreeSet(t, comp);
    }

    private static <Elt> Object toPureTreeSet(Object tree, PureTreeSet<Elt> empty) {
	if (tree == null) return null;
	else if (!(tree instanceof Node)) {
	    Object[] ary = (Object[])tree;
	    return toPureTreeSet(ary, empty, 0, ary.length);
	} else {
	    Node node = (Node)tree;
	    return empty.union(toPureTreeSet(node.left, empty),
			       toPureTreeSet(node.right, empty));
	}
    }

    private static Object toPureTreeSet(Object[] ary, PureTreeSet empty, int lo, int hi) {
	if (lo == hi) return null;	// (shouldn't happen)
	else if (lo + 1 == hi)		// speed up most common case
	    return empty.with(null, ary[lo]);
	else {
	    int mid = (lo + hi) >> 1;
	    Object left = toPureTreeSet(ary, empty, lo, mid);
	    Object right = toPureTreeSet(ary, empty, mid, hi);
	    return empty.union(left, right);
	}
    }

    // Used by the `PureHashSet(Collection)' constructor.
    PureHashSet<Elt> toPureHashSet() {
	return new PureHashSet(toPureHashSet(tree));
    }

    private static Object toPureHashSet(Object tree) {
	if (tree == null) return null;
	else if (!(tree instanceof Node)) {
	    Object[] ary = (Object[])tree;
	    return toPureHashSet(ary, 0, ary.length);
	} else {
	    Node node = (Node)tree;
	    return PureHashSet.union(toPureHashSet(node.left),
				     toPureHashSet(node.right));
	}
    }

    private static Object toPureHashSet(Object[] ary, int lo, int hi) {
	if (lo == hi) return null;	// (shouldn't happen)
	else if (lo + 1 == hi) {
	    Object elt = ary[lo];
	    return PureHashSet.with(null, elt, PureHashSet.hashCode(elt));
	} else {
	    int mid = (lo + hi) >> 1;
	    Object left = toPureHashSet(ary, lo, mid);
	    Object right = toPureHashSet(ary, mid, hi);
	    return PureHashSet.union(left, right);
	}
    }
/*
    // Used by the `PureCachedHashSet(Collection)' constructor.
    PureCachedHashSet toPureCachedHashSet() {
	return new PureCachedHashSet(toPureCachedHashSet(tree));
    }

    private static PureCachedHashSet.Subtree toPureCachedHashSet (Object tree) {
	if (tree == null) return null;
	else if (!(tree instanceof Node)) {
	    Object[] ary = (Object[])tree;
	    return toPureCachedHashSet(ary, 0, ary.length);
	} else {
	    Node node = (Node)tree;
	    return PureCachedHashSet.union(toPureCachedHashSet(node.left),
					   toPureCachedHashSet(node.right));
	}
    }

    private static PureCachedHashSet.Subtree toPureCachedHashSet (Object[] ary,
								  int lo, int hi) {
	if (lo == hi) return null;	// (shouldn't happen)
	else if (lo + 1 == hi)
	    return PureCachedHashSet.with(null, ary[lo]);
	else {
	    int mid = (lo + hi) >> 1;
	    PureCachedHashSet.Subtree left = toPureCachedHashSet(ary, lo, mid);
	    PureCachedHashSet.Subtree right = toPureCachedHashSet(ary, mid, hi);
	    return PureCachedHashSet.union(left, right);
	}
    }
*/
    public boolean isEmpty() {
	return tree == null;
    }

    public int size() {
	return treeSize(tree);
    }

    public Elt get(int index) {
	if (index < 0 || index >= treeSize(tree))
	    throw new IndexOutOfBoundsException();
	else return (Elt)get(tree, index);
    }

    public Iterator<Elt> iterator() {
	return new PTLIterator(tree);
    }

    public ListIterator<Elt> listIterator() {
	return new PTLIterator(tree);
    }

    public ListIterator<Elt> iterator(int index) {
	if (index < 0 || index > size()) throw new IndexOutOfBoundsException();
	return new PTLIterator(tree, index);
    }

    public ListIterator<Elt> listIterator(int index) {
	if (index < 0 || index > size()) throw new IndexOutOfBoundsException();
	return new PTLIterator(tree, index);
    }

    public PureTreeList<Elt> with(int index, Elt elt) {
	int size = treeSize(tree);
	if (index < 0 || index > size)
	    throw new IndexOutOfBoundsException();
	else if (index == size)
	    return new PureTreeList<Elt>(insert(tree, index, elt), elt_comp);
	else return new PureTreeList<Elt>(with(tree, index, elt), elt_comp);
    }

    public PureTreeList<Elt> withInserted(int index, Elt elt) {
	if (index < 0 || index > treeSize(tree))
	    throw new IndexOutOfBoundsException();
	else return new PureTreeList<Elt>(insert(tree, index, elt), elt_comp);
    }

    public PureTreeList<Elt> withFirst(Elt elt) {
	return new PureTreeList<Elt>(insert(tree, 0, elt), elt_comp);
    }

    public PureTreeList<Elt> withLast(Elt elt) {
	return new PureTreeList<Elt>(insert(tree, treeSize(tree), elt), elt_comp);
    }

    public PureTreeList<Elt> less(int index) {
	if (index < 0 || index >= treeSize(tree))
	    throw new IndexOutOfBoundsException();
	return new PureTreeList<Elt>(less(tree, index), elt_comp);
    }

    public PureTreeList<Elt> concat(List<? extends Elt> list) {
	if (list instanceof PureTreeList)
	    return new PureTreeList<Elt>(concat(tree, ((PureTreeList)list).tree), elt_comp);
	else return new PureTreeList<Elt>(concat(tree, fromCollection(list)), elt_comp);
    }

    public PureTreeList<Elt> reverse() {
	if (tree == null) return this;
	else return new PureTreeList<Elt>(reverse(tree), elt_comp);
    }

    public PureTreeList<Elt> subseq(int fromIndex, int toIndex) {
	int siz = treeSize(tree);
	if (fromIndex < 0 || fromIndex > siz ||
	    toIndex < 0 || toIndex > siz) throw new IndexOutOfBoundsException();
	else return new PureTreeList<Elt>(subseq(tree, fromIndex, toIndex), elt_comp);
    }

    public PureList<Elt> subList(int fromIndex, int toIndex) {
	if (toIndex < fromIndex) throw new IllegalArgumentException();
	return subseq(fromIndex, toIndex);
    }

    public PureTreeList<Elt> sort(Comparator<? super Elt> comp) {
	// We could do our own tree sort -- the code is very similar (though alas, not
	// identical) to the list-to-set conversion code -- but this is probably pretty
	// close.
	Object[] ary = toArray();
	Arrays.sort(ary, (Comparator<Object>)comp);
	return new PureTreeList<Elt>((Elt[])ary);
    }

    public PureTreeList sort() {
	return sort(null);
    }

    public boolean contains(Object elt) {
	return indexOf(tree, elt) >= 0;
    }

    public int indexOf(Object elt) {
	return indexOf(tree, elt);
    }

    public int lastIndexOf(Object elt) {
	return lastIndexOf(tree, elt);
    }

    public int compareTo(PureTreeList<Elt> obj) {
	if (obj == this) return 0;
	else if (obj == null ||
		 !(obj instanceof PureTreeList) ||
		 !eql(elt_comp, ((PureTreeList)obj).elt_comp))
	    throw new ClassCastException();
	else return compareTo(tree, ((PureTreeList)obj).tree);
    }

    public boolean equals(Object obj) {
	if (obj == this) return true;
	else if (obj instanceof PureTreeList) {
	    PureTreeList ptl = (PureTreeList)obj;
	    return equals(tree, ptl.tree);
	} else if (!(obj instanceof List)) return false;
	else {
	    List<Object> list = (List<Object>)obj;
	    if (size() != list.size()) return false;
	    Iterator<Elt> it1 = iterator();
	    Iterator<Object> it2 = list.iterator();
	    while (it1.hasNext()) {
		Object elt1 = it1.next(), elt2 = it2.next();
		if (!eql(elt1, elt2)) return false;
	    }
	    return true;
	}
    }

    public int hashCode() {
	if (hash_code == Integer.MIN_VALUE) hash_code = hashCode(tree, 1);
	return hash_code;
    }

    // For debugging.
    String dump() {
	return dump(tree);
    }

    boolean verify() {
	return verify(tree);
    }

    /******************************************************************************/
    /* Internals */

    // Inspired by Stephen Adams' paper on weight-balanced binary trees.  As an additional
    // development, these trees are heterogeneous: instead of consisting entirely of nodes,
    // the lowest two to three levels of the tree are stored in bounded-length vectors.
    // This cuts space requirements roughly in half without costing much (if any) time.

    /* Instance variables */
    private transient Object tree;
    private Comparator<Elt> elt_comp;
    private transient int hash_code = Integer.MIN_VALUE;	// cache

    /*package*/ PureTreeList(Object _tree, Comparator<? super Elt> _elt_comp) {
	tree = _tree;
	elt_comp = (Comparator<Elt>)_elt_comp;
    }

    /* The threshold length above which tree nodes will be built. */
    private static final int MAX_LEAF_ARRAY_LENGTH = 8;

    /* The factor by which one subtree may outweigh another.  See Adams.  Don't
     * change this unless you understand his analysis. */
    private static final int BALANCE_FACTOR = 4;

    private static final class Node {
	public Node (int _size, Object _left, Object _right) {
	    size = _size;
	    left = _left;
	    right = _right;
	}
	public int size;	// the number of elements in the subtree
	public Object left;	// a subtree
	public Object right;	// a subtree
    }

    private static Object makeNode(Object left, Object right) {
	if (left == null) return right;
	else if (right == null) return left;
	else return new Node(treeSize(left) + treeSize(right), left, right);
    }

    private static int treeSize(Object subtree) {
	if (subtree == null) return 0;
	else if (!(subtree instanceof Node)) return ((Object[])subtree).length;
	else return ((Node)subtree).size;
    }

    private static Object get(Object subtree, int index) {
	if (!(subtree instanceof Node)) return ((Object[])subtree)[index];
	else {
	    Node node = (Node)subtree;
	    int sizl = treeSize(node.left);
	    if (index < sizl) return get(node.left, index);
	    else return get(node.right, index - sizl);
	}
    }

    private static Object with(Object subtree, int index, Object elt) {
	if (!(subtree instanceof Node)) return update((Object[])subtree, index, elt);
	else {
	    Node node = (Node)subtree;
	    int sizl = treeSize(node.left);
	    if (index < sizl)
		return makeNode(with(node.left, index, elt), node.right);
	    else return makeNode(node.left, with(node.right, index - sizl, elt));
	}
    }

    private static Object insert(Object subtree, int index, Object elt) {
	if (subtree == null) {
	    Object[] ary = new Object[1];
	    ary[0] = elt;
	    return ary;
	} else if (!(subtree instanceof Node)) {
	    Object[] ary = (Object[])subtree;
	    int len = ary.length;
	    if (len < MAX_LEAF_ARRAY_LENGTH) return insert(ary, index, elt);
	    else if (index * 2 < len)
		return makeNode(subseqInsert(ary, 0, index, index, elt),
				subseq(ary, index, len));
	    else return makeNode(subseq(ary, 0, index),
				 subseqInsert(ary, index, len, 0, elt));
	} else {
	    Node node = (Node)subtree;
	    int sizl = treeSize(node.left);
	    if (index < sizl)
		return buildNode(insert(node.left, index, elt), node.right);
	    else return buildNode(node.left, insert(node.right, index - sizl, elt));
	}
    }

    private static Object less(Object subtree, int index) {
	if (!(subtree instanceof Node)) return less((Object[])subtree, index);
	else {
	    Node node = (Node)subtree;
	    int sizl = treeSize(node.left);
	    if (index < sizl)
		return buildNode(less(node.left, index), node.right);
	    else return buildNode(node.left, less(node.right, index - sizl));
	}
    }

    private static Object reverse(Object subtree) {
	if (!(subtree instanceof Node)) return reverse((Object[])subtree);
	else {
	    Node node = (Node)subtree;
	    return makeNode(reverse(node.right), reverse(node.left));
	}
    }

    private int compareTo(Object tree1, Object tree2) {
	if (tree1 == tree2) return 0;
	else {
	    int size1 = treeSize(tree1), size2 = treeSize(tree2);
	    if (size1 < size2) return -1;
	    else if (size1 > size2) return 1;
	    else return compareTo(tree1, 0, tree2, 0, 0, size1);
	}
    }

    private int compareTo(Object subtree1, int base1, Object subtree2, int base2,
			  int lo, int hi) {
	if (lo == hi) return 0;
	else if (!(subtree1 instanceof Node)) {
	    if (!(subtree2 instanceof Node)) {
		Object[] ary1 = (Object[])subtree1, ary2 = (Object[])subtree2;
		for (int i = lo; i < hi; ++i) {
		    int comp_res = compareElements(ary1[i - base1], ary2[i - base2]);
		    if (comp_res != 0) return comp_res;
		}
		return 0;
	    } else return - compareTo(subtree2, base2, subtree1, base1, lo, hi);
	} else {
	    Node node1 = (Node)subtree1;
	    Object left1 = node1.left;
	    int sizl1 = treeSize(left1);
	    int new_hi = base1 + sizl1;
	    RankTrimResult rtr1 = rankTrim(left1, base1, lo, new_hi);
	    RankTrimResult rtr2 = rankTrim(subtree2, base2, lo, new_hi);
	    int left_comp_res = compareTo(rtr1.subtree, rtr1.base,
					  rtr2.subtree, rtr2.base, lo, new_hi);
	    if (left_comp_res != 0) return left_comp_res;
	    else {
		int new_lo = new_hi;
		RankTrimResult rtr3 = rankTrim(node1.right, new_lo, new_lo, hi);
		RankTrimResult rtr4 = rankTrim(subtree2, base2, new_lo, hi);
		return compareTo(rtr3.subtree, rtr3.base,
				 rtr4.subtree, rtr4.base, new_lo, hi);
	    }
	}
    }

    private boolean equals (Object tree1, Object tree2) {
	if (tree1 == tree2) return true;
	int size1 = treeSize(tree1), size2 = treeSize(tree2);
	if (size1 != size2) return false;
	else return equals(tree1, 0, tree2, 0, 0, size1);
    }

    private boolean equals(Object subtree1, int base1, Object subtree2, int base2,
			   int lo, int hi) {
	if (lo == hi) return true;
	else if (!(subtree1 instanceof Node)) {
	    if (!(subtree2 instanceof Node)) {
		Object[] ary1 = (Object[])subtree1, ary2 = (Object[])subtree2;
		for (int i = lo; i < hi; ++i) {
		    Object elt1 = ary1[i - base1], elt2 = ary2[i - base2];
		    if (!eql(elt1, elt2)) return false;
		}
		return true;
	    } else return equals(subtree2, base2, subtree1, base1, lo, hi);
	} else {
	    Node node1 = (Node)subtree1;
	    Object left1 = node1.left;
	    int sizl1 = treeSize(left1);
	    int new_hi = base1 + sizl1;
	    RankTrimResult rtr1 = rankTrim(left1, base1, lo, new_hi);
	    RankTrimResult rtr2 = rankTrim(subtree2, base2, lo, new_hi);
	    if (!equals(rtr1.subtree, rtr1.base, rtr2.subtree, rtr2.base, lo, new_hi))
		return false;
	    else {
		int new_lo = base1 + sizl1;
		RankTrimResult rtr3 = rankTrim(node1.right, new_lo, new_lo, hi);
		RankTrimResult rtr4 = rankTrim(subtree2, base2, new_lo, hi);
		return equals(rtr3.subtree, rtr3.base,
			      rtr4.subtree, rtr4.base, new_lo, hi);
	    }
	}
    }

    private static final class RankTrimResult {
	public RankTrimResult (Object _subtree, int _base) {
	    subtree = _subtree;
	    base = _base;
	}
	public Object subtree;
	public int base;
    }

    private RankTrimResult rankTrim(Object subtree, int base, int lo, int hi) {
	while (subtree != null && subtree instanceof Node) {
	    Node node = (Node)subtree;
	    int nrank = base + treeSize(node.left);
	    if (nrank >= lo) {
		if (nrank < hi) break;
		else subtree = node.left;
	    } else {
		base = nrank;
		subtree = node.right;
	    }
	}
	return new RankTrimResult(subtree, base);
    }

    private static Object concat(Object left, Object right) {
	if (left == null) return right;
	else if (right == null) return left;
	else {
	    int sizl = treeSize(left);
	    int sizr = treeSize(right);
	    if (left instanceof Node && sizl > sizr * BALANCE_FACTOR) {
		Node l = (Node)left;
		return buildNode(l.left, concat(l.right, right));
	    } else if (right instanceof Node && sizr > sizl * BALANCE_FACTOR) {
		Node r = (Node)right;
		return buildNode(concat(left, r.left), r.right);
	    } else return buildNode(left, right);
	}
    }

    private static Object buildNode(Object left, Object right) {
	if (left == null) return right;
	else if (right == null) return left;
	else if (!(left instanceof Node) && !(right instanceof Node)) {
	    Object[] lary = (Object[])left, rary = (Object[])right;
	    if (lary.length + rary.length < MAX_LEAF_ARRAY_LENGTH)
		return concat(lary, rary);
	    else return makeNode(left, right);
	} else {
	    int sizl = treeSize(left);
	    int sizr = treeSize(right);
	    if (right instanceof Node && sizr > sizl * BALANCE_FACTOR) {
		Node r = (Node)right;
		Object rl = r.left;
		Object rr = r.right;
		if (!(rl instanceof Node) || treeSize(rl) <= treeSize(rr))
		    return makeNode(buildNode(left, rl), rr);
		else {
		    Node rln = (Node)rl;
		    return makeNode(buildNode(left, rln.left),
				    buildNode(rln.right, rr));
		}
	    } else if (left instanceof Node && sizl > sizr * BALANCE_FACTOR) {
		Node l = (Node)left;
		Object ll = l.left;
		Object lr = l.right;
		if (!(lr instanceof Node) || treeSize(lr) <= treeSize(ll))
		    return makeNode(ll, buildNode(lr, right));
		else {
		    Node lrn = (Node)lr;
		    return makeNode(buildNode(ll, lrn.left),
				    buildNode(lrn.right, right));
		}
	    } else return makeNode(left, right);
	}
    }

    private int compareElements(Object x, Object y) {
	// `null' is treated as being less than every object.
	if (x == null) return (y == null ? 0 : -1);
	else if (y == null) return 1;
	else if (elt_comp != null) return elt_comp.compare((Elt)x, (Elt)y);
	else {
	    // Will throw `ClassCastException' if the objects don't implement
	    // `Comparable' -- this is correct.
	    Comparable cx = (Comparable)x;
	    Comparable cy = (Comparable)y;
	    return cx.compareTo(cy);
	}
    }

    private static Object subseq(Object subtree, int lo, int hi) {
	if (lo >= hi) return null;
	else if (lo == 0 && hi == treeSize(subtree)) return subtree;
	else if (!(subtree instanceof Node))
	    return subseq((Object[])subtree, lo, hi);
	else {
	    Node node = (Node)subtree;
	    int sizl = treeSize(node.left);
	    if (hi <= sizl) return subseq(node.left, lo, hi);
	    else if (lo >= sizl) return subseq(node.right, lo - sizl, hi - sizl);
	    else return concat(subseq(node.left, lo, sizl),
			       subseq(node.right, 0, hi - sizl));
	}
    }

    private static int indexOf(Object subtree, Object elt) {
	if (subtree == null) return -1;
	else if (!(subtree instanceof Node)) {
	    Object[] ary = (Object[])subtree;
	    for (int i = 0, len = ary.length; i < len; ++i) {
		if (eql(elt, ary[i])) return i;
	    }
	    return -1;
	} else {
	    Node node = (Node)subtree;
	    int l_pos = indexOf(node.left, elt);
	    if (l_pos >= 0) return l_pos;
	    else {
		int r_pos = indexOf(node.right, elt);
		if (r_pos >= 0) return r_pos + treeSize(node.left);
		else return -1;
	    }
	}
    }

    private static int lastIndexOf(Object subtree, Object elt) {
	if (subtree == null) return -1;
	else if (!(subtree instanceof Node)) {
	    Object[] ary = (Object[])subtree;
	    for (int i = ary.length; --i >= 0; ) {
		if (eql(elt, ary[i])) return i;
	    }
	    return -1;
	} else {
	    Node node = (Node)subtree;
	    int r_pos = lastIndexOf(node.right, elt);
	    if (r_pos >= 0) return r_pos + treeSize(node.left);
	    else {
		int l_pos = lastIndexOf(node.left, elt);
		if (l_pos >= 0) return l_pos;
		else return -1;
	    }
	}
    }

    private static Object[] concat(Object[] left, Object[] right) {
	int llen = (left == null ? 0 : left.length);
	int rlen = (right == null ? 0 : right.length);
	int len = llen + rlen;
	Object[] a = new Object[len];
	for (int i = 0; i < llen; ++i) a[i] = left[i];
	for (int i = 0; i < rlen; ++i) a[i + llen] = right[i];
	return a;
    }

    private static Object[] update(Object[] ary, int idx, Object elt) {
	int len = ary.length;
	Object[] a = new Object[len];
	for (int i = 0; i < len; ++i) a[i] = ary[i];
	a[idx] = elt;
	return a;
    }

    private static Object[] insert(Object[] ary, int idx, Object elt) {
	int len = ary.length + 1;
	Object[] a = new Object[len];
	for (int i = 0; i < idx; ++i) a[i] = ary[i];
	a[idx] = elt;
	for (int i = idx + 1; i < len; ++i) a[i] = ary[i - 1];
	return a;
    }

    // Called `remove' in `PureTreeSet', but here I wanted to be consistent with
    // its caller.
    private static Object[] less(Object[] ary, int idx) {
	int len = ary.length - 1;
	if (len == 0) return null;
	else {
	    Object[] a = new Object[len];
	    for (int i = 0; i < idx; ++i) a[i] = ary[i];
	    for (int i = idx; i < len; ++i) a[i] = ary[i + 1];
	    return a;
	}
    }

    private static Object[] reverse(Object[] ary) {
	int len = ary.length;
	Object[] a = new Object[len];
	for (int i = 0; i < len; ++i) a[i] = ary[len - i - 1];
	return a;
    }

    private static Object[] subseq(Object[] ary, int lo, int hi) {
	if (lo >= hi) return null;
	else {
	    int len = hi - lo;
	    Object[] a = new Object[len];
	    for (int i = 0; i < len; ++i)
		a[i] = ary[i + lo];
	    return a;
	}
    }

    // Takes the subsequence of `ary' from `lo' to `hi', then at `idx' within
    // the result, inserts `elt', returning the new array.
    private static Object[] subseqInsert(Object[] ary, int lo, int hi, int idx, Object elt) {
	int len = hi - lo;
	Object[] a = new Object[len + 1];
	for (int i = 0; i < idx; ++i) a[i] = ary[i + lo];
	a[idx] = elt;
	for (int i = idx; i < len; ++i) a[i + 1] = ary[i + lo];
	return a;
    }

    private static int hashCode(Object subtree, int hash) {
	if (subtree == null) return hash;
	else if (!(subtree instanceof Node)) {
	    Object[] ary = (Object[]) subtree;
	    for (int i = 0, len = ary.length; i < len; ++i) {
		Object x = ary[i];
		hash = 31 * hash + (x == null ? 0 : x.hashCode());
	    }
	} else {
	    Node node = (Node)subtree;
	    hash = hashCode(node.left, hash);
	    hash = hashCode(node.right, hash);
	}
	return hash;
    }

    private static String dump(Object thing) {
	if (thing == null) return "null";
	else if (!(thing instanceof Node)) {
	    StringBuffer str_buf = new StringBuffer("{");
	    Object[] ary = (Object[])thing;
	    for (int i = 0; i < ary.length; ++i) {
		str_buf.append(dump(ary[i]));
		if (i < ary.length - 1) str_buf.append(", ");
	    }
	    str_buf.append("}");
	    return str_buf.toString();
	} else if (thing instanceof Node) {
	    Node node = (Node)thing;
	    return "(" + node.size + ";\n" +
		indent(dump(node.left), "  ") + ",\n" +
		indent(dump(node.right), "  ") + ")";
	} else return thing.toString();
    }

    private static String indent(String str, String prefix) {
	StringBuffer res = new StringBuffer(prefix);
	for (int i = 0, len = str.length(); i < len; ++i) {
	    char c = str.charAt(i);
	    res.append(c);
	    if (c == '\n' && i < len - 1) res.append(prefix);
	}
	return res.toString();
    }

    private boolean verify(Object subtree) {
	if (subtree == null) return true;
	else if (!(subtree instanceof Node)) {
	    Object[] ary = (Object[])subtree;
	    if (ary.length > MAX_LEAF_ARRAY_LENGTH) return false;
	} else {
	    Node node = (Node)subtree;
	    if (node.left == null || node.right == null) return false;
	    int sizl = treeSize(node.left);
	    int sizr = treeSize(node.right);
	    if (node.size != sizl + sizr) return false;
	    // Small subtrees may be unbalanced, because of our array-splitting heuristic.
	    // As long as the size of any subtree that can be unbalanced is strictly
	    // bounded, we're still okay.
	    if ((sizr > 4 && sizl > sizr * BALANCE_FACTOR) ||
		(sizl > 4 && sizr > sizl * BALANCE_FACTOR))
		return false;
	    return verify(node.left) && verify(node.right);
	}
	return true;
    }

    private static boolean eql(Object x, Object y) {
	return x == null ? y == null : x.equals(y);
    }

    /****************/
    // Iterator class

    private static final class PTLIterator<Elt> implements ListIterator<Elt> {

	private static final class IteratorNode {
	    public IteratorNode (Object _subtree, int _index, IteratorNode _parent) {
		subtree = _subtree;
		index = _index;
		parent = _parent;
	    }
	    public Object subtree;
	    public int index;
	    public IteratorNode parent;
	}

	private IteratorNode inode;
	private boolean at_start = false, at_end = false;

	private PTLIterator(Object subtree) {
	    inode = new IteratorNode(subtree, 0, null);
	    at_start = true;
	    if (subtree == null) at_end = true;
	    canonicalizeFwd();
	}

	private PTLIterator(Object subtree, int index) {
	    inode = new IteratorNode(subtree, 0, null);
	    at_start = (index == 0);
	    at_end = (index == treeSize(subtree));
	    if (at_end) {
		if (subtree instanceof Node) inode.index = 2;
		else inode.index = index;
		return;
	    }
	    while (inode.subtree instanceof Node) {
		Node node = (Node)inode.subtree;
		int sizl = treeSize(node.left);
		if (index < sizl) {
		    inode.index = 0;
		    inode = new IteratorNode(node.left, 0, inode);
		} else {
		    index -= sizl;
		    inode.index = 1;
		    inode = new IteratorNode(node.right, 0, inode);
		}
	    }
	    inode.index = index;
	}		

	private void canonicalizeFwd() {
	    if (at_end) return;
	    while (true) {
		if (!(inode.subtree instanceof Node)) {
		    if (inode.index < ((Object[])inode.subtree).length) break;
		    else {
			if (inode.parent == null) {
			    at_end = true;
			    break;
			}
			inode = inode.parent;
			++inode.index;
		    }
		} else {
		    Node node = (Node)inode.subtree;
		    if (inode.index == 0) inode = new IteratorNode(node.left, 0, inode);
		    else if (inode.index == 1)
			inode = new IteratorNode(node.right, 0, inode);
		    else if (inode.parent == null) {
			at_end = true;
			break;
		    } else {
			inode = inode.parent;
			++inode.index;
		    }
		}
	    }
	}

	public boolean hasNext() {
	    return !at_end;
	}

	public Elt next() {
	    if (at_end) throw new NoSuchElementException();
	    else {
		Object elt = ((Object[])inode.subtree)[inode.index++];
		at_start = false;
		canonicalizeFwd();
		return (Elt)elt;
	    }
	}

	public int nextIndex() {
	    int result = 0;
	    IteratorNode tmp_in = inode;
	    while (tmp_in != null) {
		if (!(tmp_in.subtree instanceof Node)) result += tmp_in.index;
		else if (tmp_in.index == 1)
		    result += treeSize(((Node)tmp_in.subtree).left);
		else if (tmp_in.index == 2)
		    result += treeSize(tmp_in.subtree);
		tmp_in = tmp_in.parent;
	    }
	    return result;
	}

	public int previousIndex() {
	    return nextIndex() - 1;
	}

	static final int RIGHT_END = Integer.MAX_VALUE;

	private void canonicalizeRev() {
	    if (at_start) return;
	    while (true) {
		if (!(inode.subtree instanceof Node)) {
		    if (inode.index == RIGHT_END)
			inode.index = ((Object[])inode.subtree).length;
		    if (inode.index > 0) break;
		    else {
			IteratorNode tmp_in = inode.parent;
			while (tmp_in != null) {
			    if (tmp_in.index > 0) break;
			    else tmp_in = tmp_in.parent;
			}
			if (tmp_in == null) {
			    at_start = true;
			    break;
			} else {
			    inode = tmp_in;
			    --inode.index;
			}
		    }
		} else {
		    Node node = (Node)inode.subtree;
		    // `inode.index' is 2 when we're at the very end.
		    if (inode.index == 2 || inode.index == RIGHT_END) {
			inode.index = 1;
			inode = new IteratorNode(node.right, RIGHT_END, inode);
		    } else if (inode.index == 0)
			inode = new IteratorNode(node.left, RIGHT_END, inode);
		    else throw new RuntimeException("Bug in `PureTreeList.PTLIterator." +
						    "canonicalizeRev'");
		}
	    }
	}

	public boolean hasPrevious() {
	    return !at_start;
	}

	public Elt previous() {
	    canonicalizeRev();
	    if (at_start) throw new NoSuchElementException();
	    else {
		Object elt = ((Object[])inode.subtree)[--inode.index];
		if (inode.index == 0) {
		    IteratorNode tmp_in = inode.parent;
		    while (tmp_in != null) {
			if (tmp_in.index > 0) break;
			else tmp_in = tmp_in.parent;
		    }
		    if (tmp_in == null) at_start = true;
		}
		at_end = false;
		return (Elt)elt;
	    }
	}

	public void add(Object o) {
	    throw new UnsupportedOperationException();
	}

	public void remove() {
	    throw new UnsupportedOperationException();
	}

	public void set(Object o) {
	    throw new UnsupportedOperationException();
	}
    }

    /**
     * Saves the state of this <code>PureTreeList</code> to a stream.
     *
     * @serialData Emits the internal data of the list, including the comparator it
     * uses; the size of the list [<code>int</code>]; and the elements in order
     * [<code>Object</code>s].
     */
    private void writeObject(java.io.ObjectOutputStream strm)
        throws java.io.IOException {
	strm.defaultWriteObject();	// writes `elt_comp'
        strm.writeInt(size());
	for (Iterator it=iterator(); it.hasNext(); )
            strm.writeObject(it.next());
    }

    /**
     * Reconstitutes the <code>PureTreeSet</code> instance from a stream.
     */
    private void readObject(java.io.ObjectInputStream strm)
        throws java.io.IOException, ClassNotFoundException {
	strm.defaultReadObject();	// reads `elt_comp'
        int size = strm.readInt();
	Object[] ary = new Object[size];
	for (int i = 0; i < size; ++i)
	    ary[i] = strm.readObject();
	tree = fromCollection(ary);
    }

}
