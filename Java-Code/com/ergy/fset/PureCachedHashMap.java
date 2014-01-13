/*
 * PureCachedHashMap.java
 *
 * Copyright (c) 2003 Sympoiesis, Inc.  All rights reserved.
 *
 */


package com.sympoiesis.FSet;
import java.util.*;
import java.lang.ref.*;

/**
 * A pure map that uses hash codes to order objects.  The name notwithstanding, it
 * is implemented as a tree, not a hash table, but its ordering function is
 * implemented by calling <code>hashCode</code>.  Thus the key objects need not
 * implement {@link Comparable}; they need not even have any superclass in common
 * other than <code>Object</code>.  In the case where distinct keys have the
 * same hash code, the implementation is correct, but less efficient; so it is
 * important to make the hash codes distinct whenever possible.  No guarantee is
 * made as to the order in which entries are returned by the iterator.
 *
 * <p>Time costs: <code>isEmpty</code>, <code>size</code>, <code>arb</code>, and
 * <code>entrySet</code> take O(1) (constant) time.  <code>containsKey</code>,
 * <code>with</code>, and <code>less</code> take O(log <i>n</i>) time.
 * <code>domain</code>, <code>keySet</code>, and <code>containsValue</code> take O(n)
 * (linear) time.  <code>toSet</code>, <code>range</code>, and <code>values</code>
 * take O(<i>n</i> log <i>n</i>) time.  <code>merge</code>, <code>restrict</code>,
 * and <code>restrictNot</code> take O(n) (linear) time if the other map involved is
 * also a <code>PureCachedHashMap</code>; otherwise, they take O(<i>n</i> log
 * <i>n</i>) time.  <code>compareTo</code> (called, for instance, if this map is an
 * element in a containing <code>PureTreeSet</code>) takes O(n) (linear) time.
 *
 * <p>Space costs: <code>PureCachedHashMap</code> uses a heterogeneous binary tree
 * structure with bounded-length arrays at the leaves.  It uses much less space than
 * traditional homogeneous binary trees; typical space consumption is roughly three
 * times that of a pair of plain arrays.
 *
 * <p><code>PureCachedHashMap</code> accepts the null key and the null value.
 *
 * <p><code>PureCachedHashMap</code> provides a variety of constructors for various cases,
 * including two that take an <code>Object[][]</code>.  These are intended for
 * convenience when initializing maps in code; one may write, for instance,
 *
 * <pre>
 *     Object[][] map_init = { { "x", "1" }, { "y", "2" } }
 *     PureMap map = new PureCachedHashMap(map_init);
 * </pre>
 *
 * to get a map that maps "x" to "1" and "y" to "2".
 *
 * <p><code>PureCachedHashMap</code> also provides, corresponding to each
 * constructor, a static factory method <code>withDefault</code> which, in addition
 * to the functionality of the constructor, also allows the specification of a
 * default value to be returned by the <code>get</code> method when it is called
 * with a key which is not in the map.  (Otherwise, <code>get</code> returns
 * <code>null</code> in that case.)  This is particularly handy for map
 * <i>chaining</i>, in which the range values in one map are themselves maps.  For
 * example, if the outer map is created like this:
 *
 * <pre>
 *     PureMap map = PureCachedHashMap.withDefault(new PureCachedHashMap());
 * </pre>
 *
 * the chained mapping <code>key1 -> key2 -> val</code> can then be added like this:
 *
 * <pre>
 *     map = map.with(key1, ((PureMap)map.get(key1)).with(key2, val));
 * </pre>
 *
 * which works even if <code>map</code> does not already contain an entry for
 * <code>key1</code>.  (Of course, the outer and inner maps do not have to be of the
 * same class.)
 *
 * <p><code>PureCachedHashMap</code> implements {@link java.io.Serializable}; an
 * instance of it is serializable provided that all keys and values it contains, and
 * the default value if nonnull, are serializable.
 *
 * @author Scott L. Burson, Sympoiesis, Inc.
 * @see PureMap
 * @see PureTreeMap
 */


public class PureCachedHashMap
    extends AbstractPureMap
    implements Comparable, java.io.Serializable
{

    /**
     * Constructs an empty <code>PureCachedHashMap</code>.
     */
    public PureCachedHashMap () {
	tree = null;
    }

    /**
     * Constructs a new <code>PureCachedHashMap</code> containing the same entries as
     * <code>map</code>.
     *
     * @param map the map to use the entries of
     */
    public PureCachedHashMap (Map map) {
	if (map instanceof PureCachedHashMap) tree = ((PureCachedHashMap)map).tree;
	else {
	    // &&& Could be improved along the lines of the `PureCachedHashSet'
	    // comstructor.
	    Subtree t = null;
	    for (Iterator it = map.entrySet().iterator(); it.hasNext(); ) {
		Map.Entry ent = (Map.Entry)it.next();
		Object key = ent.getKey();
		t = Subtree.with(t, key, hashCode(key), ent.getValue());
	    }
	    tree = t;
	}
    }

    /**
     * Constructs a <code>PureCachedHashMap</code>, initializing it from
     * <code>ary</code>, which should be an array of key/value pairs represented as
     * arrays of length 2, containing the key at index 0 and the value at index 1.
     *
     * @param ary the array of pairs
     */
    public PureCachedHashMap (Object[][] ary) {
	tree = fromArray(ary, 0, ary.length);
    }

    private Subtree fromArray (Object[][] ary, int lo, int hi) {
	if (lo == hi) return null;
	else if (lo + 1 == hi) {
	    Object[] pr = ary[lo];
	    // While in most cases we could just use `pr', let's protect ourselves
	    // against it being too long or getting altered later.
	    Object[] kv = new Object[2];
	    kv[0] = pr[0];
	    kv[1] = pr[1];
	    int[] h = new int[1];
	    h[0] = hashCode(kv[0]);
	    return new Leaf(kv, h);
	} else {
	    int mid = (lo + hi) >> 1;
	    return merge(fromArray(ary, lo, mid),
			 fromArray(ary, mid, hi));
	}
    }

    /**
     * Constructs and returns an empty <code>PureCachedHashMap</code> with default
     * <code>dflt</code>.  The resulting map's <code>get</code> method returns
     * <code>dflt</code> when called with a key which is not in the map.
     *
     * @param dflt the default value
     * @return the new <code>PureCachedHashMap</code>
     */
    public static PureCachedHashMap withDefault (Object dflt) {
	PureCachedHashMap m = new PureCachedHashMap();
	m.dflt = dflt;
	return m;
    }

    /**
     * Constructs and returns a <code>PureCachedHashMap</code> with default
     * <code>dflt</code>, containing the same entries as <code>map</code>.  The
     * resulting map's <code>get</code> method returns <code>dflt</code> when called
     * with a key which is not in the map.
     *
     * @param map the map to use the entries of
     * @param dflt the default value
     * @return the new <code>PureCachedHashMap</code>
     */
    public static PureCachedHashMap withDefault (Map map, Object dflt) {
	PureCachedHashMap m = new PureCachedHashMap(map);
	m.dflt = dflt;
	return m;
    }

    /**
     * Constructs and returns a <code>PureCachedHashMap</code> with default
     * <code>dflt</code>, initializing it from <code>ary</code>.  <code>ary</code>
     * should be an array of key/value pairs represented as arrays of length 2,
     * containing the key at index 0 and the value at index 1.  The resulting map's
     * <code>get</code> method returns <code>dflt</code> when called with a key
     * which is not in the map.
     *
     * @param ary the array of pairs
     * @param dflt the default value
     * @return the new <code>PureCachedHashMap</code>
     */
    public static PureCachedHashMap withDefault (Object[][] ary, Object dflt) {
	PureCachedHashMap m = new PureCachedHashMap(ary);
	m.dflt = dflt;
	return m;
    }

    public boolean isEmpty () {
	return tree == null;
    }

    public int size () {
	return Subtree.size(tree);
    }

    public Map.Entry arb () {
	if (tree == null) return null;
	else return tree.arb();
    }

    public boolean containsKey (Object key) {
	return Subtree.containsKey(tree, key, hashCode(key));
    }

    /**
     * Returns the value to which this map maps <code>key</code>.  If this map
     * contains no entry for <code>key</code>, returns this map's default value,
     * which is normally <code>null</code>, but may be a different value if the map
     * was originally created by the <code>withDefault</code> static factory
     * method. */
    public Object get (Object key) {
	return Subtree.get(tree, key, hashCode(key), dflt);
    }

    public PureMap with (Object key, Object value) {
	Subtree t = Subtree.with(tree, key, hashCode(key), value);
	if (t == tree) return this;
	else return new PureCachedHashMap(t, dflt);
    }

    public PureMap less (Object key) {
	Subtree t = Subtree.less(tree, key, hashCode(key));
	if (t == tree) return this;
	else return new PureCachedHashMap(t, dflt);
    }

    public PureSet domain () {
	PureCachedHashSet.Subtree dom = Subtree.domain(tree);
	return new PureCachedHashSet(dom);
    }

    public Set keySet () {
	return domain();
    }

    public PureSet toSet () {
	return toSet(new PureCachedHashSet());
    }

    public PureSet toSet (PureSet initial_set) {
	// Gives us an empty set of the right class and comparator.
	PureSet s = initial_set.difference(initial_set);
	for (Iterator it = iterator(); it.hasNext(); )
	    s = s.with(it.next());
	return s;
    }

    public Set entrySet () {
	return new AbstractSet() {
		public Iterator iterator () {
		    return PureCachedHashMap.this.iterator();
		}
		public int size () {
		    return PureCachedHashMap.this.size();
		}
		public boolean contains (Object x) {
		    if (!(x instanceof Map.Entry)) return false;
		    else {
			Map.Entry ent = (Map.Entry)x;
			Object ekey = ent.getKey();
			Object eval = ent.getValue();
			// This could be improved, but I don't think it's important...
			if (containsKey(ekey)) {
			    Object mval = get(ekey);
			    return mval == null ? eval == null : mval.equals(eval);
			} else return false;
		    }
		}
		public boolean remove (Object x) {
		    throw new UnsupportedOperationException();
		}
		public void clear () {
		    throw new UnsupportedOperationException();
		}
	    };
    }

    /**
     * Returns the range of the map (the set of values it contains).  A synonym for
     * <code>values</code>, except that the return type is <code>PureSet</code>.
     * The returned set is a {@link PureCachedHashSet}.
     *
     * @return the range set of this map
     */
    public PureSet range () {
	return Subtree.range(tree, new PureCachedHashSet());
    }

    public PureSet range (PureSet initial_set) {
	// Gives us an empty set of the right class and comparator.
	initial_set = initial_set.difference(initial_set);
	return Subtree.range(tree, initial_set);
    }

    public PureMap merge (PureMap with_map) {
	PureCachedHashMap with_phm = new PureCachedHashMap(with_map);
	Subtree t = merge(tree, with_phm.tree);
	return new PureCachedHashMap(t, dflt);
    }

    public PureMap restrict (PureSet set) {
	PureCachedHashSet phs = new PureCachedHashSet(set);
	Subtree t = restrict(tree, phs.tree);
	return new PureCachedHashMap(t, dflt);
    }

    public PureMap restrictNot (PureSet set) {
	PureCachedHashSet phs = new PureCachedHashSet(set);
	Subtree t = restrictNot(tree, phs.tree);
	return new PureCachedHashMap(t, dflt);
    }

    public Object getDefault () {
	return dflt;
    }

    public Iterator iterator () {
	return new PHMIterator(tree);
    }

    public int compareTo (Object obj) {
	if (obj == this) return 0;
	else if (obj == null || !(obj instanceof PureCachedHashMap))
	    throw new ClassCastException();
	else {
	    Subtree tree2 = ((PureCachedHashMap)obj).tree;
	    if (tree == tree2) return 0;
	    int size = Subtree.size(tree), size2 = Subtree.size(tree2);
	    // Start by comparing the sizes; smaller maps are considered less than
	    // larger ones.  Only if the sizes are equal do we have to do the
	    // lexicographic comparison.
	    if (size < size2) return -1;
	    else if (size > size2) return 1;
	    else return Subtree.compareTo(tree, 0, tree2, 0, 0, size);
	}
    }

    /* We don't bother caching equality results on sets smaller than this. */
    private static final int EQUALS_CACHE_THRESHOLD = 8;

    public boolean equals (Object obj) {
	if (obj == this || cachedEquals(obj)) return true;
	else if (obj instanceof PureCachedHashMap) {
	    PureCachedHashMap phm = (PureCachedHashMap)obj;
	    boolean is_equal = equals(tree, phm.tree);
	    if (is_equal && size() >= EQUALS_CACHE_THRESHOLD) {
		cacheEqual(phm);
		phm.cacheEqual(this);
	    }
	    return is_equal;
	} else if (!(obj instanceof Map)) return false;
	else {
	    // Not a PureCachedHashMap.
	    Map map = (Map)obj;
	    if (size() != map.size()) return false;
	    for (Iterator it = map.entrySet().iterator(); it.hasNext(); ) {
		Map.Entry ent = (Map.Entry)it.next();
		Object ekey = ent.getKey();
		Object eval = ent.getValue();
		if (!containsKey(ekey)) return false;
		if (eval == null ? get(ekey) != null
		    : !eval.equals(get(ekey))) return false;
	    }
	    if (size() >= EQUALS_CACHE_THRESHOLD) {
		if (obj instanceof PureMap) {
		    cacheEqual((PureMap)obj);
		    if (obj instanceof AbstractPureMap)
			((AbstractPureMap)obj).cacheEqual(this);
		}
	    }
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
    private transient Subtree tree;
    private Object dflt = null;
    private transient int hash_code = Integer.MIN_VALUE;	// cache
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

    static class Entry implements Map.Entry {
	public Entry (Object _key, Object _value) {
	    key = _key;
	    value = _value;
	}
	Object key;
	Object value;
	public Object getKey () { return key; }
	public Object getValue () { return value; }
	public Object setValue (Object newval) {
	    throw new UnsupportedOperationException();
	}
	public int hashCode () {
	    return ((key == null ? 0 : key.hashCode()) ^
		    (value == null ? 0 : value.hashCode()));
	}
	public boolean equals (Object obj) {
	    if (obj == null) return false;
	    else if (!(obj instanceof Map.Entry)) return false;
	    else {
		Map.Entry ent = (Map.Entry)obj;
		return ((key == null ? ent.getKey() == null : key.equals(ent.getKey())) &&
			(value == null ? ent.getValue() == null :
			 value.equals(ent.getValue())));
	    }
	}
    }

    // Used internally.
    static class EntryWithHash extends Entry {
	EntryWithHash (Object _key, int _hash, Object _value) {
	    super(_key, _value);
	    hash = _hash;
	}
	int hash;
    }

    /****************/

    private PureCachedHashMap (Subtree _tree, Object _dflt) {
	tree = _tree;
	dflt = _dflt;
    }

    private static int keySize (Object key) {
	if (key instanceof EquivalentMap)
	    return ((EquivalentMap)key).contents.size();
	else return 1;
    }

    private static int hashCode (Subtree subtree) {
	if (subtree == null) return 0;
	else return subtree.hashCode();
    }

    private static Subtree merge (Subtree tree1, Subtree tree2) {
	return Subtree.merge(tree1, tree2, NEGATIVE_INFINITY, POSITIVE_INFINITY);
    }

    private static Subtree restrict (Subtree tree1, PureCachedHashSet.Subtree tree2) {
	return Subtree.restrict(tree1, tree2, NEGATIVE_INFINITY, POSITIVE_INFINITY);
    }

    private static Subtree restrictNot (Subtree tree1, PureCachedHashSet.Subtree tree2) {
	return Subtree.restrictNot(tree1, tree2, NEGATIVE_INFINITY, POSITIVE_INFINITY);
    }

    private static boolean equals (Subtree tree1, Subtree tree2) {
	if (tree1 == tree2) return true;
	int size1 = Subtree.size(tree1), size2 = Subtree.size(tree2);
	if (size1 != size2) return false;
	else return Subtree.equals(tree1, 0, tree2, 0, 0, size1);
    }

    /****************/
    /* A subtree can be either null, a `Node', or a `Leaf'.  In `PureTreeMap' we
     * used a bare `Object[]' for a leaf, but that doesn't work here because we need
     * to cache the hash codes as well; since those are ints, they can't go in the
     * `Object[]'.  (Making `Integer's for them would more than chew up the space we
     * gain by using arrays at the leaves in the first place.)  So this time we go
     * with proper OO style rather than all those `instanceof' tests -- probably
     * gaining some speed as well (though we still do `instanceof' to check for
     * `EquivalentMap's). */
    static abstract class Subtree {

	static Node makeNode (Object key, int khash, Object value,
			      Subtree left, Subtree right) {
	    if (key instanceof Entry) {		// convenience for some callers
		Entry ent = (Entry)key;
		value = ent.value;
		key = ent.key;
	    }
	    return new Node(size(left) + size(right) + keySize(key),
			    key, khash, value, left, right);
	}

	static Leaf makeLeaf (Object[] keys_vals, int[] hashes) {
	    if (keys_vals == null) return null;
	    else return new Leaf(keys_vals, hashes);
	}

	static Leaf buildLeaf (Object key, int khash, Object value,
			       Leaf left, Leaf right) {
	    int llen = (left == null ? 0 : left.hashes.length);
	    int rlen = (right == null ? 0 : right.hashes.length);
	    int len = llen + 1 + rlen;
	    Object[] keys_vals = new Object[len << 1];
	    int[] hashes = new int[len];
	    for (int i = 0; i < llen; ++i) {
		keys_vals[i] = left.keys_vals[i];
		keys_vals[i + len] = left.keys_vals[i + llen];
		hashes[i] = left.hashes[i];
	    }
	    keys_vals[llen] = key;
	    keys_vals[llen + len] = value;
	    hashes[llen] = khash;
	    for (int i = 0; i < rlen; ++i) {
		keys_vals[i + llen + 1] = right.keys_vals[i];
		keys_vals[i + llen + 1 + len] = right.keys_vals[i + rlen];
		hashes[i + llen + 1] = right.hashes[i];
	    }
	    return new Leaf(keys_vals, hashes);
	}

	abstract int size();

	static int size (Subtree subtree) {
	    if (subtree == null) return 0;
	    else return subtree.size();
	}

	abstract Map.Entry arb();

	abstract boolean containsKey(Object key, int khash);

	static boolean containsKey (Subtree subtree, Object key, int khash) {
	    if (subtree == null) return false;
	    else return subtree.containsKey(key, khash);
	}

	abstract Object get(Object key, int khash, Object dflt);

	static Object get (Subtree subtree, Object key, int khash, Object dflt) {
	    if (subtree == null) return dflt;
	    else return subtree.get(key, khash, dflt);
	}

	// `key' may be an `EquivalentMap', or an `Entry'.
	abstract Subtree with(Object key, int khash, Object value);

	static Subtree with (Subtree subtree, Object key, int khash, Object value) {
	    // &&& This case seems to originate in `join' -- might be best to deal with
	    // it there.
	    if (key instanceof Entry) {
		Entry ent = (Entry)key;
		key = ent.key;
		value = ent.value;
	    }
	    if (subtree == null) {
		if (!(key instanceof EquivalentMap)) {
		    Object[] keys_vals = new Object[2];
		    keys_vals[0] = key;
		    keys_vals[1] = value;
		    int[] hashes = new int[1];
		    hashes[0] = khash;
		    return new Leaf(keys_vals, hashes);
		} else return makeNode(key, khash, value, null, null);
	    } else return subtree.with(key, khash, value);
	}

	abstract Subtree less(Object key, int khash);

	static Subtree less (Subtree subtree, Object key, int khash) {
	    if (subtree == null) return null;
	    else return subtree.less(key, khash);
	}

	abstract EntryWithHash min();
	abstract Subtree lessMin();

	abstract PureCachedHashSet.Subtree domain();

	static PureCachedHashSet.Subtree domain(Subtree subtree) {
	    if (subtree == null) return null;
	    else return subtree.domain();
	}

	abstract PureSet range(PureSet initial);

	static PureSet range (Subtree subtree, PureSet initial) {
	    if (subtree == null) return initial;
	    else return subtree.range(initial);
	}

	abstract Subtree merge(Subtree subtree2, int lo, int hi);

	static Subtree merge (Subtree subtree1, Subtree subtree2, int lo, int hi) {
	    if (subtree1 == null) return split(subtree2, lo, hi);
	    else if (subtree2 == null) return split(subtree1, lo, hi);
	    else return subtree1.merge(subtree2, lo, hi);
	}

	abstract Subtree restrict(PureCachedHashSet.Subtree set_subtree, int lo, int hi);

	static Subtree restrict (Subtree map_subtree,
				 PureCachedHashSet.Subtree set_subtree,
				 int lo, int hi){
	    if (map_subtree == null) return null;
	    else if (set_subtree == null) return null;
	    else return map_subtree.restrict(set_subtree, lo, hi);
	}

	abstract Subtree restrictNot(PureCachedHashSet.Subtree set_subtree,
				     int lo, int hi);

	static Subtree restrictNot (Subtree map_subtree,
				    PureCachedHashSet.Subtree set_subtree,
				    int lo, int hi){
	    if (map_subtree == null) return null;
	    else if (set_subtree == null) return split(map_subtree, lo, hi);
	    else return map_subtree.restrictNot(set_subtree, lo, hi);
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

	abstract Subtree split(int lo, int hi);

	// Returns a new tree all of whose keys are greater than `lo' and less
	// than `hi'.  (Contrast `trim'.)
	static Subtree split (Subtree subtree, int lo, int hi) {
	    if (subtree == null) return null;
	    else return subtree.split(lo, hi);
	}

	abstract Subtree trim(int lo, int hi);

	// Returns the largest subtree of `subtree' whose root key is greater than `lo'
	// and less than `hi'.  (Contrast `split'.)
	static Subtree trim (Subtree subtree, int lo, int hi) {
	    if (subtree == null) return null;
	    else return subtree.trim(lo, hi);
	}

	abstract EntryWithHash rankEntry(int rank);

	static final class RankTrimResult {
	    RankTrimResult (Subtree _subtree, int _base) {
		subtree = _subtree;
		base = _base;
	    }
	    Subtree subtree;
	    int base;
	}

	static RankTrimResult rankTrim (Subtree subtree, int base, int lo, int hi) {
	    while (subtree instanceof Node) {
		Node node = (Node)subtree;
		int nrank = base + size(node.left);
		if (nrank >= lo) {
		    if (nrank < hi) break;
		    else subtree = node.left;
		} else {
		    int rbase = nrank + keySize(node.key);
		    if (rbase > lo) break;
		    else {
			subtree = node.right;
			base = rbase;
		    }
		}
	    }
	    return new RankTrimResult(subtree, base);
	}

	abstract Entry findEquiv(int hash);

	static Entry findEquiv (Subtree subtree, int hash) {
	    if (subtree == null) return null;
	    else return subtree.findEquiv(hash);
	}

	// Assumes that all keys of `left' are less than `key', and all keys of
	// `right' are greater than `key'; returns a new tree containing all these
	// keys and their values.  This does more rebalancing than `buildNode',
	// which otherwise has the same contract.  `key' may be an `EquivalentMap'.
	static Subtree concat (Object key, int khash, Object value,
			       Subtree left, Subtree right) {
	    if (left == null) return with(right, key, khash, value);
	    else if (right == null) return with(left, key, khash, value);
	    else {
		int sizl = size(left);
		int sizr = size(right);
		if (left instanceof Node && sizl > sizr * BALANCE_FACTOR) {
		    Node l = (Node)left;
		    return buildNode(l.key, l.hash, l.value, l.left,
				     concat(key, khash, value, l.right, right));
		} else if (right instanceof Node && sizr > sizl * BALANCE_FACTOR) {
		    Node r = (Node)right;
		    return buildNode(r.key, r.hash, r.value,
				     concat(key, khash, value, left, r.left), r.right);
		} else return buildNode(key, khash, value, left, right);
	    }
	}

	static Subtree join (Subtree left, Subtree right) {
	    if (left == null) return right;
	    else if (right == null) return left;
	    else {
		EntryWithHash ewh = right.min();
		return concat(ewh.key, ewh.hash, ewh.value, left,
			      right.lessMin());
	    }
	}

	static Subtree buildNode (Object key, int khash, Object value,
				  Subtree left, Subtree right) {
	    if (key instanceof Entry) {		// convenience for some callers
		Entry ent = (Entry)key;
		key = ent.key;
		value = ent.value;
	    }
	    if ((left == null || left instanceof Leaf) &&
		(right == null || right instanceof Leaf)) {
		Leaf lleaf = (Leaf)left, rleaf = (Leaf)right;
		if (!(key instanceof EquivalentMap) &&
		    (left == null ? 0 : lleaf.hashes.length) + 1 +
		    (right == null ? 0 : rleaf.hashes.length) < MAX_LEAF_ARRAY_LENGTH)
		    return buildLeaf(key, khash, value, lleaf, rleaf);
		else return makeNode(key, khash, value, left, right);
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
			return makeNode(r.key, r.hash, r.value,
					buildNode(key, khash, value, left, rl),
					rr);
		    else {
			Node rln = (Node)rl;
			return makeNode(rln.key, rln.hash, rln.value,
					buildNode(key, khash, value, left, rln.left),
					buildNode(r.key, r.hash, r.value,
						  rln.right, rr));
		    }
		} else if (left instanceof Node && sizl > sizr * BALANCE_FACTOR) {
		    Node l = (Node)left;
		    Subtree ll = l.left;
		    Subtree lr = l.right;
		    if (lr instanceof Leaf || size(lr) <= size(ll))
			return makeNode(l.key, l.hash, l.value, ll,
					buildNode(key, khash, value, lr, right));
		    else {
			Node lrn = (Node)lr;
			return makeNode(lrn.key, lrn.hash, lrn.value,
					buildNode(l.key, l.hash, l.value,
						  ll, lrn.left),
					buildNode(key, khash, value, lrn.right, right));
		    }
		} else return makeNode(key, khash, value, left, right);
	    }
	}

	public static int hashCode (Subtree subtree) {
	    if (subtree == null) return 0;
	    else return subtree.hashCode();
	}
    }

    /****************/
    /* Node */

    // In `PureTreeMap', `Node' extends `Entry', but this isn't convenient here since
    // we need this `Subtree' class and Java doesn't have multiple inheritance from
    // classes.
    static final class Node extends Subtree {
	Node (int _size, Object _key, int _hash, Object _value,
	      Subtree _left, Subtree _right) {
	    size = _size;
	    key = _key;
	    hash = _hash;
	    value = _value;
	    left = _left;
	    right = _right;
	}
	int size;	// the number of keys_vals in the subtree
	Object key;
	int hash;	// `hashCode(key)', cached
	Object value;
	Subtree left;	// a subtree
	Subtree right;	// a subtree

	int size () { return size; }

	Map.Entry arb () {
	    if (key instanceof EquivalentMap)
		return (Map.Entry)((EquivalentMap)key).contents.get(0);
	    else return new Entry(key, value);
	}

	boolean containsKey (Object k, int khash) {
	    if (khash == hash) {
		if (key instanceof EquivalentMap) {
		    ArrayList al = ((EquivalentMap)key).contents;
		    for (int i = 0, len = al.size(); i < len; ++i) {
			Object ekey = ((Entry)al.get(i)).key;
			if (ekey == null ? k == null : ekey.equals(k)) return true;
		    }
		    return false;
		} else return key == null ? k == null : key.equals(k);
	    } else if (khash < hash) return containsKey(left, k, khash);
	    else return containsKey(right, k, khash);

	}
	
	Object get (Object k, int khash, Object dflt) {
	    if (khash == hash) {
		if (key instanceof EquivalentMap) {
		    ArrayList al = ((EquivalentMap)key).contents;
		    for (int i = 0, len = al.size(); i < len; ++i) {
			Entry ent = (Entry)al.get(i);
			if (k == null ? ent.key == null : k.equals(ent.key))
			    return ent.value;
		    }
		    return dflt;
		} else if (key == null ? k == null : key.equals(k))
		    return value;
		else return dflt;
	    } else if (khash < hash) return get(left, k, khash, dflt);
	    else return get(right, k, khash, dflt);
	}

	Subtree with (Object k, int khash, Object val) {
	    if (khash == hash) {
		if (!(key instanceof EquivalentMap) &&
		    !(k instanceof EquivalentMap) &&
		    (key == null ? k == null : key.equals(k)))
		    if (val == null ? value == null : val.equals(value))
			return this;
		    else return makeNode(key, hash, val, left, right);
		else return makeNode(equivMerge(key, value, k, val),
				     hash, val, left, right);
	    } else if (khash < hash) {
		Subtree new_left = with(left, k, khash, val);
		if (new_left == left) return this;
		else return buildNode(key, hash, value, new_left, right);
	    } else {
		Subtree new_right = with(right, k, khash, val);
		if (new_right == right) return this;
		else return buildNode(key, hash, value, left, new_right);
	    }
	}

	Subtree less (Object k, int khash) {
	    if (khash == hash) {
		if (!(key instanceof EquivalentMap)) {
		    if (key == null ? k != null : !key.equals(k))
			return this;
		    else return join(left, right);
		} else return buildNode(equivLess(key, k), hash, null, left, right);
	    } else if (khash < hash) {
		Subtree new_left = less(left, k, khash);
		if (new_left == left) return this;
		else return buildNode(key, hash, value, new_left, right);
	    } else {
		Subtree new_right = less(right, k, khash);
		if (new_right == right) return this;
		else return buildNode(key, hash, value, left, new_right);
	    }
	}

	EntryWithHash min () {
	    if (left == null) return new EntryWithHash(key, hash, value);
	    else return left.min();
	}

	Subtree lessMin () {
	    if (left == null) return right;
	    else return concat(key, hash, value, left.lessMin(), right);
	}

	PureCachedHashSet.Subtree domain () {
	    PureCachedHashSet.Subtree ldom = domain(left), rdom = domain(right);
	    if (key instanceof EquivalentMap) {
		ArrayList al = ((EquivalentMap)key).contents;
		ArrayList dom = new ArrayList(al.size());
		for (int i = 0; i < al.size(); ++i) dom.add(((Entry)al.get(i)).key);
		return PureCachedHashSet.Subtree.makeNode
			 (new PureCachedHashSet.EquivalentSet(dom),
			  hash, ldom, rdom);
	    } else return PureCachedHashSet.Subtree.makeNode(key, hash, ldom, rdom);
	}

	PureSet range (PureSet initial) {
	    if (key instanceof EquivalentMap) {
		ArrayList al = ((EquivalentMap)key).contents;
		for (int i = 0; i < al.size(); ++i)
		    initial = initial.with(((Entry)al.get(i)).value);
		return initial;
	    } else return (range(left, initial)
			   .with(value)
			   .union(range(right, initial)));
	}

	Subtree merge (Subtree subtree2, int lo, int hi) {
	    Subtree new_left = merge(left,
				     trim(subtree2, lo, hash),
				     lo, hash);
	    Subtree new_right = merge(right,
				      trim(subtree2, hash, hi),
				      hash, hi);
	    Entry entry2 = findEquiv(subtree2, hash);
	    if (entry2 == null) return concat(key, hash, value, new_left, new_right);
	    else {
		Object k = equivMerge(key, value, entry2.key, entry2.value);
		return concat(k, hash, null, new_left, new_right);
	    }
	}

	Subtree restrict (PureCachedHashSet.Subtree set_subtree, int lo, int hi) {
	    Subtree new_left = restrict(left,
					PureCachedHashSet.Subtree.trim
					  (set_subtree, lo, hash),
					lo, hash);
	    Subtree new_right = restrict(right,
					 PureCachedHashSet.Subtree.trim
					   (set_subtree, hash, hi),
					 hash, hi);
	    Object set_elt = PureCachedHashSet.Subtree.findEquiv(set_subtree, hash);
	    if (set_elt == PureCachedHashSet.NO_ELEMENT)
		return join(new_left, new_right);
	    else {
		Object k = equivRestrict(key, value, set_elt);
		if (k == null) return join(new_left, new_right);
		else if (k instanceof EquivalentMap)
		    return concat(k, hash, null, new_left, new_right);
		else {
		    Entry ent = (Entry)k;
		    return concat(ent.key, hash, ent.value, new_left, new_right);
		}
	    }
	}

	Subtree restrictNot (PureCachedHashSet.Subtree set_subtree, int lo, int hi) {
	    Subtree new_left =
		restrictNot(left, PureCachedHashSet.Subtree.trim(set_subtree, lo, hash),
			    lo, hash);
	    Subtree new_right =
		restrictNot(right, PureCachedHashSet.Subtree.trim(set_subtree, hash, hi),
			    hash, hi);
	    Object set_elt = PureCachedHashSet.Subtree.findEquiv(set_subtree, hash);
	    if (set_elt == PureCachedHashSet.NO_ELEMENT)
		return concat(key, hash, value, new_left, new_right);
	    else {
		Object k = equivRestrictNot(key, value, set_elt);
		if (k == null) return join(new_left, new_right);
		else if (k instanceof EquivalentMap)
		    return concat(k, hash, null, new_left, new_right);
		else {
		    Entry ent = (Entry)k;
		    return concat(ent.key, hash, ent.value, new_left, new_right);
		}
	    }
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
		EntryWithHash ewh2 = subtree2.rankEntry(new_hi - base2);
		if (hash < ewh2.hash) return -1;
		else if (hash > ewh2.hash) return 1;
		else {
		    int comp_res = equivCompare(key, value, ewh2.key, ewh2.value);
		    if (comp_res != 0) return comp_res;
		    else {
			int new_lo = base + lsize + keySize(key);
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
		EntryWithHash ewh2 = subtree2.rankEntry(new_hi - base2);
		if (hash != ewh2.hash) return false;
		else if (key == null ? ewh2.key != null : !equivEquals(key, ewh2.key))
		    return false;
		else {
		    int key_size = keySize(key);
		    int new_lo = base + lsize + key_size;
		    RankTrimResult rtr3 = rankTrim(right, new_lo, new_lo, hi);
		    RankTrimResult rtr4 = rankTrim(subtree2, base2, new_lo, hi);
		    return equals(rtr3.subtree, rtr3.base,
				  rtr4.subtree, rtr4.base, new_lo, hi);
		}
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
		else return concat(key, hash, value, new_left, new_right);
	    }
	}

	Subtree trim (int lo, int hi) {
	    if (hash > lo) {
		if (hash < hi) return this;
		else return trim(left, lo, hi);
	    } else return trim(right, lo, hi);
	}

	EntryWithHash rankEntry (int rank) {
	    int left_size = Subtree.size(left);
	    if (rank < left_size) return left.rankEntry(rank);
	    else {
		int key_size = keySize(key);
		if (rank < left_size + key_size)
		    return new EntryWithHash(key, hash, value);
		else return right.rankEntry(rank - (left_size + key_size));
	    }
	}

	Entry findEquiv (int khash) {
	    if (khash < hash) return findEquiv(left, khash);
	    else if (khash > hash) return findEquiv(right, khash);
	    else return new Entry(key, value);
	}

	public int hashCode () {
	    int result = hashCode(left) + hashCode(right);
	    if (key instanceof EquivalentMap) {
		ArrayList al = ((EquivalentMap)key).contents;
		for (int i = 0, siz = al.size(); i < siz; ++i) {
		    Object val = ((Entry)al.get(i)).value;
		    result += hash ^ (val == null ? 0 : val.hashCode());
		}
	    } else result += hash ^ (value == null ? 0 : value.hashCode());
	    return result;
	}

    }

    /****************/
    /* Leaf */
    static final class Leaf extends Subtree {
	Leaf (Object[] _keys_vals, int[] _hashes) {
	    keys_vals = _keys_vals;
	    hashes = _hashes;
	}
	Object[] keys_vals;	// double length: keys first, then values
	// You might be tempted to subtract two hash codes and check the sign of the
	// result.  Don't!  The difference may not fit in 32 bits!
	// `hashes.length' is the number of entries in the leaf.
	int[] hashes;

	int size () { return hashes.length; }

	Map.Entry arb () {
	    int nkeys = hashes.length;
	    int idx = nkeys >> 1;
	    return new Entry(keys_vals[idx], keys_vals[idx + nkeys]);
	}

	boolean containsKey (Object key, int khash) {
	    int bin_srch_res = binarySearch(hashes, khash);
	    if ((bin_srch_res & BIN_SEARCH_FOUND_MASK) == BIN_SEARCH_FOUND) {
		Object k = keys_vals[bin_srch_res >> BIN_SEARCH_INDEX_SHIFT];
		return key == null ? k == null : key.equals(k);
	    } else return false;
	}

	Object get (Object key, int khash, Object dflt) {
	    int bin_srch_res = binarySearch(hashes, khash);
	    if ((bin_srch_res & BIN_SEARCH_FOUND_MASK) == BIN_SEARCH_FOUND) {
		int idx = bin_srch_res >> BIN_SEARCH_INDEX_SHIFT;
		Object k = keys_vals[idx];
		if (key == null ? k == null : key.equals(k))
		    return keys_vals[idx + hashes.length];
		else return dflt;
	    } else return dflt;
	}

	Subtree with (Object key, int khash, Object value) {
	    int bin_srch_res = binarySearch(hashes, khash);
	    int found = bin_srch_res & BIN_SEARCH_FOUND_MASK;
	    int idx = bin_srch_res >> BIN_SEARCH_INDEX_SHIFT;
	    int next = (found == BIN_SEARCH_FOUND ? idx + 1 : idx);
	    int nkeys = hashes.length;
	    if (found == BIN_SEARCH_FOUND && !(key instanceof EquivalentMap) &&
		(key == null ? keys_vals[idx] == null : key.equals(keys_vals[idx])))
		return new Leaf(updateObj2(keys_vals, idx, value), hashes);
	    else if (found == BIN_SEARCH_NOT_FOUND  &&
		     nkeys + 1 < MAX_LEAF_ARRAY_LENGTH  &&
		     !(key instanceof EquivalentMap))
		return new Leaf(insertObj2(keys_vals, idx, key, value),
				insertInt(hashes, idx, khash));
	    else return makeNode((found == BIN_SEARCH_FOUND
				  ? equivMerge(keys_vals[idx], keys_vals[idx + nkeys],
					       key, value)
				  : key),
				 khash, value,
				 makeLeaf(subseqObj2(keys_vals, 0, idx),
					  subseqInt(hashes, 0, idx)),
				 makeLeaf(subseqObj2(keys_vals, next, nkeys),
					  subseqInt(hashes, next, nkeys)));
	}

	Subtree less (Object key, int khash) {
	    int bin_srch_res = binarySearch(hashes, khash);
	    int found = bin_srch_res & BIN_SEARCH_FOUND_MASK;
	    int idx = bin_srch_res >> BIN_SEARCH_INDEX_SHIFT;
	    if (found == BIN_SEARCH_FOUND) {
		if (key == null ? keys_vals[idx] == null : key.equals(keys_vals[idx]))
		    return makeLeaf(removeObj2(keys_vals, idx),
				    removeInt(hashes, idx));
		else return this;
	    } else return this;
	}

	EntryWithHash min () {
	    return new EntryWithHash(keys_vals[0], hashes[0], keys_vals[hashes.length]);
	}

	Subtree lessMin () {
	    return makeLeaf(subseqObj2(keys_vals, 1, hashes.length),
			    subseqInt(hashes, 1, hashes.length));
	}

	PureCachedHashSet.Subtree domain () {
	    int len = hashes.length;
	    Object[] dom = new Object[len];
	    for (int i = 0; i < len; ++i) dom[i] = keys_vals[i];
	    return PureCachedHashSet.Subtree.makeLeaf(dom, hashes);
	}

	PureSet range (PureSet initial) {
	    int len = hashes.length;
	    for (int i = 0; i < len; ++i)
		initial = initial.with(keys_vals[i + len]);
	    return initial;
	}

	Subtree merge (Subtree subtree2, int lo, int hi) {
	    if (subtree2 instanceof Leaf) {
		Leaf leaf2 = (Leaf)subtree2;
		return mergeLeaves(keys_vals, hashes, leaf2.keys_vals, leaf2.hashes,
				   lo, hi);
	    } else {
		Node node2 = (Node)subtree2;
		int hash2 = node2.hash;
		Subtree new_left = merge(trim(lo, hash2),	// i.e. `this.trim'
					 trim(node2.left, lo, hash2),
					 lo, hash2);
		Subtree new_right = merge(trim(hash2, hi),
					  trim(node2.right, hash2, hi),
					  hash2, hi);
		Entry entry1 = findEquiv(hash2);		// i.e. `this.findEquiv'
		if (entry1 == null)
		    return concat(node2.key, node2.hash, node2.value,
				  new_left, new_right);
		else {
		    Object k = equivMerge(entry1.key, entry1.value,
					  node2.key, node2.value);
		    return concat(k, hash2, null, new_left, new_right);
		}
	    }
	}

	Subtree restrict (PureCachedHashSet.Subtree set_subtree, int lo, int hi) {
	    if (set_subtree instanceof PureCachedHashSet.Leaf) {
		PureCachedHashSet.Leaf set_leaf = (PureCachedHashSet.Leaf)set_subtree;
		return restrictLeaves(keys_vals, hashes,
				      set_leaf.elements, set_leaf.hashes,
				      lo, hi);
	    } else {
		PureCachedHashSet.Node set_node = (PureCachedHashSet.Node)set_subtree;
		Object raw_elt = set_node.element;
		int set_hash = set_node.hash;
		Object set_elt;
		if (raw_elt instanceof PureCachedHashSet.EquivalentSet)
		    set_elt = ((PureCachedHashSet.EquivalentSet)raw_elt).contents.get(0);
		else set_elt = raw_elt;
		Subtree new_left = restrict(trim(lo, set_hash),
					    set_node.left, lo, set_hash);
		Subtree new_right = restrict(trim(set_hash, hi),
					     set_node.right, set_hash, hi);
		Entry entry = findEquiv(set_hash);
		if (entry == null) return join(new_left, new_right);
		else {
		    Object k = equivRestrict(entry.key, entry.value, raw_elt);
		    if (k == null) return join(new_left, new_right);
		    else if (k instanceof EquivalentMap)
			return concat(k, set_hash, null, new_left, new_right);
		    else {
			Entry ent = (Entry)k;
			return concat(ent.key, set_hash, ent.value, new_left, new_right);
		    }
		}
	    }
	}

	Subtree restrictNot (PureCachedHashSet.Subtree set_subtree, int lo, int hi) {
	    if (set_subtree instanceof PureCachedHashSet.Leaf) {
		PureCachedHashSet.Leaf set_leaf = (PureCachedHashSet.Leaf)set_subtree;
		return restrictNotLeaves(keys_vals, hashes,
					 set_leaf.elements, set_leaf.hashes,
					 lo, hi);
	    } else {
		PureCachedHashSet.Node set_node = (PureCachedHashSet.Node)set_subtree;
		Object raw_elt = set_node.element;
		int set_hash = set_node.hash;
		Object set_elt;
		if (raw_elt instanceof PureCachedHashSet.EquivalentSet)
		    set_elt = ((PureCachedHashSet.EquivalentSet)raw_elt).contents.get(0);
		else set_elt = raw_elt;
		Subtree new_left = restrictNot(trim(lo, set_hash),
					       set_node.left, lo, set_hash);
		Subtree new_right = restrictNot(trim(set_hash, hi),
						set_node.right, set_hash, hi);
		Entry entry = findEquiv(set_hash);
		if (entry == null) return join(new_left, new_right);
		else {
		    Object k = equivRestrictNot(entry.key, entry.value, raw_elt);
		    if (k == null) return join(new_left, new_right);
		    else if (k instanceof EquivalentMap)
			return concat(k, set_hash, null, new_left, new_right);
		    else {
			Entry ent = (Entry)k;
			return concat(ent.key, set_hash, ent.value, new_left, new_right);
		    }
		}
	    }
	}

	int compareTo (int base, Subtree subtree2, int base2, int lo, int hi) {
	    if (subtree2 instanceof Leaf) {
		Leaf leaf2 = (Leaf)subtree2;
		for (int i = lo; i < hi; ++i) {
		    int hash = hashes[i - base];
		    int hash2 = leaf2.hashes[i - base2];
		    if (hash < hash2) return -1;
		    else if (hash > hash2) return 1;
		    int vhash =
			PureCachedHashMap.hashCode(keys_vals[i - base + hashes.length]);
		    int v2hash =
			PureCachedHashMap.hashCode(leaf2.keys_vals[i - base2 +
								   leaf2.hashes.length]);
		    if (vhash < v2hash) return -1;
		    else if (vhash > v2hash) return 1;
		}
		return 0;
	    } else return - subtree2.compareTo(base2, this, base, lo, hi);
	}

	boolean equals (int base, Subtree subtree2, int base2, int lo, int hi) {
	    if (subtree2 instanceof Leaf) {
		Leaf leaf2 = (Leaf)subtree2;
		for (int i = lo; i < hi; ++i) {
		    if (hashes[i - base] != leaf2.hashes[i - base2]) return false;
		    Object key = keys_vals[i - base], key2 = leaf2.keys_vals[i - base2];
		    if (key == null ? key2 != null : !key.equals(key2)) return false;
		    Object val = keys_vals[i - base + hashes.length];
		    Object val2 = leaf2.keys_vals[i - base2 + leaf2.hashes.length];
		    if (val == null ? val2 != null : !val.equals(val2)) return false;
		}
		return true;
	    } else return equals(subtree2, base2, this, base, lo, hi);
	}

	Subtree split (int lo, int hi) {
	    int len = hashes.length;
	    int lo_split = (lo == NEGATIVE_INFINITY ? 0 : binarySearchLo(hashes, lo));
	    int hi_split = (hi == POSITIVE_INFINITY ? len : binarySearchHi(hashes, hi));
	    if (lo_split >= hi_split) return null;
	    else if (lo_split == 0 && hi_split == len) return this;
	    else return makeLeaf(subseqObj2(keys_vals, lo_split, hi_split),
				 subseqInt(hashes, lo_split, hi_split));
	}

	Subtree trim (int lo, int hi) {
	    // If the array is completely out of range, drop it.
	    if (hashes[0] >= hi || hashes[hashes.length - 1] <= lo)
		return null;
	    else return this;
	}

	EntryWithHash rankEntry (int rank) {
	    return new EntryWithHash(keys_vals[rank], hashes[rank],
				     keys_vals[rank + hashes.length]);
	}

	Entry findEquiv (int khash) {
	    int bin_srch_res = binarySearch(hashes, khash);
	    if ((bin_srch_res & BIN_SEARCH_FOUND_MASK) == BIN_SEARCH_FOUND) {
		int idx = bin_srch_res >> BIN_SEARCH_INDEX_SHIFT;
		return new Entry(keys_vals[idx], keys_vals[idx + hashes.length]);
	    } else return null;
	}

	public int hashCode () {
	    int result = 0, len = hashes.length;
	    for (int i = 0; i < len; ++i) {
		Object val = keys_vals[i + len];
		result += hashes[i] ^ (val == null ? 0 : val.hashCode());
	    }
	    return result;
	}

	/* Internal array manipulation routines.  These all assume their index
	 * parameters are within bounds.  Of course, despite what the names of some
	 * might suggest, they all make new arrays. */

	static Object[] insertObj2 (Object[] ary, int idx, Object key, Object value) {
	    int nkeys = ary.length >> 1;
	    Object[] a = new Object[ary.length + 2];
	    for (int i = 0; i < idx; ++i) {
		a[i] = ary[i];
		a[i + nkeys + 1] = ary[i + nkeys];
	    }
	    a[idx] = key;
	    a[idx + nkeys + 1] = value;
	    for (int i = idx; i < nkeys; ++i) {
		a[i + 1] = ary[i];
		a[i + nkeys + 2] = ary[i + nkeys];
	    }
	    return a;
	}

	static int[] insertInt (int[] ary, int idx, int key) {
	    int len = ary.length + 1;
	    int[] a = new int[len];
	    for (int i = 0; i < idx; ++i) a[i] = ary[i];
	    a[idx] = key;
	    for (int i = idx + 1; i < len; ++i) a[i] = ary[i - 1];
	    return a;
	}

	static Object[] removeObj2 (Object[] ary, int idx) {
	    int nkeys = ary.length >> 1;
	    if (nkeys == 1) return null;
	    else {
		Object[] a = new Object[ary.length - 2];
		for (int i = 0; i < idx; ++i) {
		    a[i] = ary[i];
		    a[i + nkeys - 1] = ary[i + nkeys];
		}
		for (int i = idx + 1; i < nkeys; ++i) {
		    a[i - 1] = ary[i];
		    a[i + nkeys - 2] = ary[i + nkeys];
		}
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

	static Object[] subseqObj2 (Object[] ary, int lo, int hi) {
	    if (lo >= hi) return null;
	    else {
		int ary_nkeys = ary.length >> 1;
		int nkeys = hi - lo;
		Object[] a = new Object[nkeys << 1];
		for (int i = 0; i < nkeys; ++i) {
		    a[i] = ary[i + lo];
		    a[i + nkeys] = ary[i + lo + ary_nkeys];
		}
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

	// Updates the value for the key at `idx'.
	static Object[] updateObj2 (Object[] ary, int idx, Object value) {
	    int len = ary.length;
	    Object[] a = new Object[len];
	    for (int i = 0; i < len; ++i) a[i] = ary[i];
	    a[idx + (len >> 1)] = value;
	    return a;
	}

	// Does a merge on `ary1' and `ary2', omitting any pairs whose keys are not
	// greater than `lo' and less than `hi'.  If the result is too long to be a
	// leaf, splits it and makes a node.  Also, if any equivalent keys are found,
	// makes a node.
	static Subtree mergeLeaves (Object[] keys_vals1, int[] hashes1,
				    Object[] keys_vals2, int[] hashes2,
				    int lo, int hi) {
	    int i1 = 0, i2 = 0;
	    int nkeys1 = hashes1.length, nkeys2 = hashes2.length;
	    int len1 = nkeys1, len2 = nkeys2;
	    while (i1 < len1 && lo >= hashes1[i1]) ++i1;
	    while (i2 < len2 && lo >= hashes2[i2]) ++i2;
	    while (i1 < len1 && hi <= hashes1[len1 - 1]) --len1;
	    while (i2 < len2 && hi <= hashes2[len2 - 1]) --len2;
	    // The version in `PureTreeMap' uses `ArrayList's, but we have to use
	    // an `int[]' for the hashes, so we may as well use arrays for both.
	    int len = (len1 - i1) + (len2 - i2);
	    Object[] keys_vals = new Object[len << 1];
	    int[] hashes = new int[len];
	    int nelts = 0;
	    boolean any_equiv = false;
	    while (true) {
		if (i1 == len1) {
		    while (i2 < len2) {
			keys_vals[nelts] = keys_vals2[i2];
			keys_vals[nelts + len] = keys_vals2[i2 + nkeys2];
			hashes[nelts++] = hashes2[i2];
			++i2;
		    }
		    break;
		} else if (i2 == len2) {
		    while (i1 < len1) {
			keys_vals[nelts] = keys_vals1[i1];
			keys_vals[nelts + len] = keys_vals1[i1 + nkeys1];
			hashes[nelts++] = hashes1[i1];
			++i1;
		    }
		    break;
		} else {
		    Object k1 = keys_vals1[i1], k2 = keys_vals2[i2];
		    int h1 = hashes1[i1], h2 = hashes2[i2];
		    if (h1 < h2) {
			keys_vals[nelts] = k1;
			keys_vals[nelts + len] = keys_vals1[i1 + nkeys1];
			hashes[nelts++] = h1;
			++i1;
		    } else if (h1 > h2) {
			keys_vals[nelts] = k2;
			keys_vals[nelts + len] = keys_vals2[i2 + nkeys2];
			hashes[nelts++] = h2;
			++i2;
		    } else {
			if (k1 == null ? k2 == null : k1.equals(k2)) {
			    keys_vals[nelts] = k2;
			    keys_vals[nelts + len] = keys_vals2[i2 + nkeys2];
			    hashes[nelts++] = h2;
			} else {
			    keys_vals[nelts] = equivMerge(k1, keys_vals1[i1 + nkeys1],
							  k2, keys_vals2[i2 + nkeys2]);
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
		    t = with(t, keys_vals[i], hashes[i], keys_vals[i + len]);
		return t;
	    } else {
		if (nelts > MAX_LEAF_ARRAY_LENGTH) {
		    int idx = nelts >> 1;
		    return makeNode(keys_vals[idx], hashes[idx], keys_vals[idx + len],
				    makeLeaf(subseqObj2(keys_vals, 0, idx),
					     subseqInt(hashes, 0, idx)),
				    makeLeaf(subseqObj2(keys_vals, idx + 1, nelts),
					     subseqInt(hashes, idx + 1, nelts)));
		} else return makeLeaf(subseqObj2(keys_vals, 0, nelts),
				       subseqInt(hashes, 0, nelts));
	    }
	}

	private Subtree restrictLeaves (Object[] map_keys_vals, int[] map_hashes,
					Object[] set_ary, int[] set_hashes,
					int lo, int hi) {
	    int i1 = 0, i2 = 0;
	    int nkeys = map_hashes.length, nelts = set_ary.length;
	    int len1 = nkeys, len2 = nelts;
	    while (i1 < len1 && lo >= map_hashes[i1]) ++i1;
	    while (i2 < len2 && lo >= set_hashes[i2]) ++i2;
	    while (i1 < len1 && hi <= map_hashes[len1 - 1]) --len1;
	    while (i2 < len2 && hi <= set_hashes[len2 - 1]) --len2;
	    int len = len1 - i1;
	    Object[] keys_vals = new Object[len << 1];
	    int[] hashes = new int[len];
	    int nres = 0;
	    while (i1 < len1 && i2 < len2) {
		int mh = map_hashes[i1];
		int sh = set_hashes[i2];
		if (mh < sh) ++i1;
		else if (mh > sh) ++i2;
		else {
		    Object k = map_keys_vals[i1];
		    Object e = set_ary[i2];
		    if (k == null ? e == null : k.equals(e)) {
			keys_vals[nres] = k;
			keys_vals[nres + len] = map_keys_vals[i1 + nkeys];
			hashes[nres++] = mh;
		    }
		    ++i1;
		    ++i2;
		}
	    }
	    if (nres == 0) return null;
	    else return makeLeaf(subseqObj2(keys_vals, 0, nres),
				 subseqInt(hashes, 0, nres));
	}

	private Subtree restrictNotLeaves (Object[] map_keys_vals, int[] map_hashes,
					   Object[] set_ary, int[] set_hashes,
					   int lo, int hi) {
	    int i1 = 0, i2 = 0;
	    int nkeys = map_hashes.length, nelts = set_ary.length;
	    int len1 = nkeys, len2 = nelts;
	    while (i1 < len1 && lo >= map_hashes[i1]) ++i1;
	    while (i2 < len2 && lo >= set_hashes[i2]) ++i2;
	    while (i1 < len1 && hi <= map_hashes[len1 - 1]) --len1;
	    while (i2 < len2 && hi <= set_hashes[len2 - 1]) --len2;
	    int len = len1 - i1;
	    Object[] keys_vals = new Object[len << 1];
	    int[] hashes = new int[len];
	    int nres = 0;
	    while (i1 < len1 && i2 < len2) {
		int mh = map_hashes[i1];
		int sh = set_hashes[i2];
		Object k = map_keys_vals[i1];
		if (mh < sh) {
		    keys_vals[nres] = k;
		    keys_vals[nres + len] = map_keys_vals[i1 + nkeys];
		    hashes[nres++] = mh;
		    ++i1;
		} else if (mh > sh) ++i2;
		else {
		    Object e = set_ary[i2];
		    if (k == null ? e != null : !k.equals(e)) {
			keys_vals[nres] = k;
			keys_vals[nres + len] = map_keys_vals[i1 + nkeys];
			hashes[nres++] = mh;
		    }
		    ++i1;
		    ++i2;
		}
	    }
	    while (i1 < len1) {
		keys_vals[nres] = map_keys_vals[i1];
		keys_vals[nres + len] = map_keys_vals[i1 + nkeys];
		hashes[nres++] = map_hashes[i1];
		++i1;
	    }
	    if (nres == 0) return null;
	    else return makeLeaf(subseqObj2(keys_vals, 0, nres),
				 subseqInt(hashes, 0, nres));
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

	static int binarySearch (int[] hashes, int khash) {
	    int lo = 0, hi = hashes.length - 1;
	    while (lo <= hi) {
		int mid = (lo + hi) / 2;
		int hash = hashes[mid];
		if (khash == hash)
		    return (mid << BIN_SEARCH_INDEX_SHIFT) | BIN_SEARCH_FOUND;
		else if (khash < hash) hi = mid - 1;
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
	else if (thing instanceof EquivalentMap) {
	    ArrayList al = ((EquivalentMap)thing).contents;
	    String res = "[";
	    for (int i = 0, size = al.size(); i < size; ++i) {
		if (i > 0) res = res + ", ";
		res = res + dump(al.get(i));
	    }
	    return res + "]";
	} else if (thing instanceof Leaf) {
	    Leaf leaf = (Leaf)thing;
	    int nkeys = leaf.hashes.length;
	    StringBuffer str_buf = new StringBuffer("{");
	    for (int i = 0; i < nkeys; ++i) {
		str_buf.append(dump(leaf.keys_vals[i]) +
			       " -> " + dump(leaf.keys_vals[i + nkeys]) +
			       "(" + dump(leaf.hashes[i]) + ")");
		if (i < nkeys - 1) str_buf.append(", ");
	    }
	    str_buf.append("}");
	    return str_buf.toString();
	} else if (thing instanceof Node) {
	    Node node = (Node)thing;
	    return "(" + node.size + ", " + dump(node.key) +
		(node.key instanceof EquivalentMap ? "" : " -> " + dump(node.value)) +
		"(" + dump(node.hash) + ");\n" +
		indent(dump(node.left), "  ") + ",\n" +
		indent(dump(node.right), "  ") + ")";
	} else if (thing instanceof Entry) {
	    Entry ent = (Entry)thing;
	    return dump(ent.key) + " -> " + dump(ent.value);
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
	    for (int i = 0, len = leaf.hashes.length; i < len; ++i) {
		int hash = leaf.hashes[i];
		if (leaf.keys_vals[i] instanceof EquivalentMap) return false;
		if (prev >= hash) return false;
		prev = hash;
	    }
	    return prev < hi;
	} else {
	    Node node = (Node)subtree;
	    int sizl = Subtree.size(node.left);
	    int sizr = Subtree.size(node.right);
	    if (node.size != sizl + sizr + keySize(node.key)) return false;
	    if (node.key instanceof Entry) return false;
	    if (node.key instanceof EquivalentMap &&
		((EquivalentMap)node.key).contents.size() < 2) return false;
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
    /* Equivalent maps */

    /* In the case where the map contains two or more keys which are distinct but
     * are equivalent according to the comparison method, the `key' field of a
     * `Node' will contain one of these.  We always build a `Node' in this case; we
     * never put equivalent elements in an array.  Here we don't worry about the
     * extra level(s) of structure because this is supposed to be a rare case.
     */
    /* The name is too cryptic, but `MapWithEquivalentKeys' is too long. */
    private static final class EquivalentMap {
	public EquivalentMap(ArrayList _contents) {
	    contents = _contents;
	}
	ArrayList contents;	// `ArrayList<Entry>'
    }

    // The "2" pair takes precedence.  Returns either an `EquivalentMap' or an `Entry'.
    private static Object equivMerge (Object key1, Object value1,
				      Object key2, Object value2) {
	if (key1 instanceof EquivalentMap) {
	    ArrayList al1 = ((EquivalentMap)key1).contents;
	    if (key2 instanceof EquivalentMap) {
		ArrayList al2 = ((EquivalentMap)key2).contents;
		ArrayList al = (ArrayList)al2.clone();
		for (int i1 = 0, siz1 = al1.size(); i1 < siz1; ++i1) {
		    Entry ent1 = (Entry)al1.get(i1);
		    boolean found = false;
		    for (int i = 0, siz = al.size(); i < siz && !found; ++i) {
			Entry e = (Entry)al.get(i);
			if (e.key == null ? ent1.key == null : e.key.equals(ent1.key))
			    found = true;
		    }
		    if (!found) al.add(ent1);
		}
		al.trimToSize();
		return new EquivalentMap(al);
	    } else {
		ArrayList al = (ArrayList)al1.clone();
		int found_at = -1;
		for (int i = 0, siz = al.size(); i < siz && found_at < 0; ++i) {
		    Entry e = (Entry)al.get(i);
		    if (key2 == null ? e.key == null : key2.equals(e.key))
			found_at = i;
		}
		if (found_at >= 0) al.set(found_at, new Entry(key2, value2));
		else al.add(new Entry(key2, value2));
		al.trimToSize();
		return new EquivalentMap(al);
	    }
	} else if (key2 instanceof EquivalentMap) {
	    ArrayList al2 = ((EquivalentMap)key2).contents;
	    boolean found = false;
	    for (int i = 0, siz = al2.size(); i < siz && !found; ++i) {
		Entry e = (Entry)al2.get(i);
		if (key1 == null ? e.key == null : key1.equals(e.key))
		    found = true;
	    }
	    if (!found) {
		ArrayList al = (ArrayList)al2.clone();
		al.add(new Entry(key1, value1));
		al.trimToSize();
		return new EquivalentMap(al);
	    } else return new EquivalentMap(al2);
	} else if (key1 == null ? key2 == null : key1.equals(key2))
	    return new Entry(key2, value2);
	else {
	    ArrayList al = new ArrayList(2);
	    al.add(new Entry(key1, value1));
	    al.add(new Entry(key2, value2));
	    return new EquivalentMap(al);
	}
    }

    private static Object equivLess (Object eqm, Object key) {
	ArrayList al = ((EquivalentMap)eqm).contents;
	int found_at = -1;
	for (int i = 0, siz = al.size(); i < siz && found_at < 0; ++i) {
	    Entry e = (Entry)al.get(i);
	    if (key == null ? e.key == null : key.equals(e.key))
		found_at = i;
	}
	if (found_at >= 0) {
	    al = (ArrayList)al.clone();
	    al.remove(found_at);
	    if (al.size() == 1) return al.get(0);
	    else return new EquivalentMap(al);
	} else return eqm;
    }

    private static Object equivRestrict (Object map_key, Object map_val,
					 Object set_elt) {
	if (map_key instanceof EquivalentMap) {
	    ArrayList map_al = ((EquivalentMap)map_key).contents;
	    if (set_elt instanceof PureCachedHashSet.EquivalentSet) {
		ArrayList set_al = ((PureCachedHashSet.EquivalentSet)set_elt).contents;
		ArrayList al = new ArrayList();
		for (int i = 0, siz = map_al.size(); i < siz; ++i) {
		    Entry e = (Entry)map_al.get(i);
		    if (set_al.contains(e.key)) al.add(e);
		}
		if (al.size() == 0) return null;
		else if (al.size() == 1) return al.get(0);
		else {
		    al.trimToSize();
		    return new EquivalentMap(al);
		}
	    } else {
		for (int i = 0, siz = map_al.size(); i < siz; ++i) {
		    Entry e = (Entry)map_al.get(i);
		    if (set_elt == null ? e.key == null : set_elt.equals(e.key))
			return e;
		}
		return null;
	    }
	} else {
	    if (set_elt instanceof PureCachedHashSet.EquivalentSet) {
		ArrayList set_al = ((PureCachedHashSet.EquivalentSet)set_elt).contents;
		if (set_al.contains(map_key))
		    return new Entry(map_key, map_val);
		else return null;
	    } else {
		if (map_key == null ? set_elt == null : map_key.equals(set_elt))
		    return new Entry(map_key, map_val);
		else return null;
	    }
	}
    }

    private static Object equivRestrictNot (Object map_key, Object map_val,
					    Object set_elt) {
	if (map_key instanceof EquivalentMap) {
	    ArrayList map_al = ((EquivalentMap)map_key).contents;
	    ArrayList al = new ArrayList();
	    if (set_elt instanceof PureCachedHashSet.EquivalentSet) {
		ArrayList set_al = ((PureCachedHashSet.EquivalentSet)set_elt).contents;
		for (int i = 0, siz = map_al.size(); i < siz; ++i) {
		    Entry e = (Entry)map_al.get(i);
		    if (!set_al.contains(e.key)) al.add(e);
		}
	    } else {
		for (int i = 0, siz = map_al.size(); i < siz; ++i) {
		    Entry e = (Entry)map_al.get(i);
		    if (set_elt == null ? e.key != null : !set_elt.equals(e.key))
			al.add(e);
		}
	    }
	    if (al.size() == 0) return null;
	    else if (al.size() == 1) return al.get(0);
	    else {
		al.trimToSize();
		return new EquivalentMap(al);
	    }
	} else {
	    if (set_elt instanceof PureCachedHashSet.EquivalentSet) {
		ArrayList set_al = ((PureCachedHashSet.EquivalentSet)set_elt).contents;
		if (!set_al.contains(map_key))
		    return new Entry(map_key, map_val);
		else return null;
	    } else {
		if (map_key == null ? set_elt != null : !map_key.equals(set_elt))
		    return new Entry(map_key, map_val);
		else return null;
	    }
	}
    }

    private static int equivCompare (Object key1, Object val1, Object key2, Object val2) {
	if (key1 instanceof EquivalentMap) {
	    ArrayList al1 = ((EquivalentMap)key1).contents;
	    if (key2 instanceof EquivalentMap) {
		ArrayList al2 = ((EquivalentMap)key2).contents;
		int siz1 = al1.size(), siz2 = al2.size();
		if (siz1 < siz2) return 1;
		else if (siz1 > siz2) return -1;
		else {
		    PureSet vals1 = new PureCachedHashSet();
		    PureSet vals2 = new PureCachedHashSet();
		    for (int i = 0; i < siz1; ++i)
			vals1 = vals1.with(((Entry)al1.get(i)).value);
		    for (int i = 0; i < siz2; ++i)
			vals2 = vals2.with(((Entry)al2.get(i)).value);
		    return ((PureCachedHashSet)vals1).compareTo(vals2);
		}
	    } else return -1;
	} else if (key2 instanceof EquivalentMap) return 1;
	else {
	    int v1hash = hashCode(val1), v2hash = hashCode(val2);
	    if (v1hash < v2hash) return -1;
	    else if (v1hash > v2hash) return 1;
	    else return 0;
	}
    }

    private static boolean equivEquals (Object key1, Object key2) {
	if (key1 instanceof EquivalentMap) {
	    ArrayList al1 = ((EquivalentMap)key1).contents;
	    if (key2 instanceof EquivalentMap) {
		ArrayList al2 = ((EquivalentMap)key2).contents;
		int siz1 = al1.size(), siz2 = al2.size();
		if (siz1 != siz2) return false;
		else {
		    for (int i1 = 0; i1 < siz1; ++i1) {
			boolean found = false;
			Entry ent1 = (Entry)al1.get(i1);
			for (int i2 = 0; i2 < siz2 && !found; ++i2) {
			    Entry ent2 = (Entry)al2.get(i2);
			    if ((ent1.key == null ? ent2.key == null
				 : ent1.key.equals(ent2.key)) &&
				(ent1.value == null ? ent2.value == null
				 : ent1.value.equals(ent2.value))) found = true;
			}
			if (!found) return false;
		    }
		    return true;
		}
	    } else return false;
	} else if (key2 instanceof EquivalentMap) return false;
	else return key1 == null ? key2 == null : key1.equals(key2);
    }


    /****************/
    // Iterator class

    private static final class PHMIterator implements Iterator {

	private static final class IteratorNode {
	    public IteratorNode (Subtree _subtree, int _index, IteratorNode _parent) {
		subtree = _subtree;
		index = _index;
		parent = _parent;
	    }
	    public Subtree subtree;
	    public int index;
	    public IteratorNode parent;
	}

	private IteratorNode inode;

	public PHMIterator(Subtree subtree) {
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
		    if (inode.index < ((Leaf)inode.subtree).hashes.length) break;
		    else {
			inode = inode.parent;
			if (inode == null) break;
			else ++inode.index;
		    }
		} else {
		    Node node = (Node)inode.subtree;
		    if (inode.index == 0) inode = new IteratorNode(node.left, 0, inode);
		    else if (inode.index == keySize(node.key) + 1)
			inode = new IteratorNode(node.right, 0, inode.parent);
		    else break;
		}
	    }
	}

	public boolean hasNext () {
	    return inode != null;
	}

	public Object next () {
	    Object entry;
	    if (inode == null) throw new NoSuchElementException();
	    else if (inode.subtree instanceof Leaf) {
		Leaf leaf = (Leaf)inode.subtree;
		entry = new Entry(leaf.keys_vals[inode.index],
				  leaf.keys_vals[inode.index + leaf.hashes.length]);
	    } else {
		Node node = (Node)inode.subtree;
		if (node.key instanceof EquivalentMap) {
		    ArrayList al = ((EquivalentMap)node.key).contents;
		    entry = al.get(inode.index - 1);
		} else entry = new Entry(node.key, node.value);
	    }
	    inode.index++;
	    canonicalize();
	    return entry;
	}

	public void remove () {
	    throw new UnsupportedOperationException();
	}
    }

    /**
     * Saves the state of this <code>PureCachedHashMap</code> to a stream.
     *
     * @serialData Emits the internal data of the map, including the default it
     * uses; the size of the map [<code>int</code>]; and the key/value pairs in key
     * order [<code>Object</code>s].
     */
    private void writeObject (java.io.ObjectOutputStream strm)
        throws java.io.IOException {
	strm.defaultWriteObject();	// writes `dflt'
        strm.writeInt(size());
	for (Iterator it = iterator(); it.hasNext(); ) {
	    Entry ent = (Entry)it.next();
            strm.writeObject(ent.key);
	    strm.writeObject(ent.value);
	}
    }

    /**
     * Reconstitutes the <code>PureCachedHashMap</code> instance from a stream.
     */
    private void readObject (java.io.ObjectInputStream strm)
        throws java.io.IOException, ClassNotFoundException {
	strm.defaultReadObject();	// reads `dflt'
        int size = strm.readInt();
	Object[][] ary = new Object[size][2];
	for (int i = 0; i < size; ++i) {
	    ary[i][0] = strm.readObject();
	    ary[i][1] = strm.readObject();
	}
	tree = fromArray(ary, 0, size);
    }

}

