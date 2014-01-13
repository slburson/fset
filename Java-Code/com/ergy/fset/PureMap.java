/*
 * PureMap.java
 *
 * Copyright (c) 2013 Scott L. Burson.
 *
 * This file is licensed under the Library GNU Public License (LGPL).
 */


package com.ergy.fset;
import java.util.*;

/**
 * A map for which the update operators are all pure (functional): they return a
 * new map rather than modifying the existing one.
 *
 * <p>Although this interface extends {@link Map} of the Java Collections Framework,
 * and thus is somewhat integrated into that framework, it is not used the same way
 * as the <code>java.util</code> classes that implement <code>Map</code>.  It does
 * not support the update operators declared by <code>Map</code> (which are
 * documented as optional, anyway); in their place it adds several new operators
 * which are "pure", in the sense that rather than modifying the map in place, they
 * construct and return a new map.
 *
 * <p>Classes implementing <code>PureMap</code> may also provide, corresponding to
 * each constructor, a static factory method <code>withDefault</code> which, in
 * addition to the functionality of the constructor, also allows the specification
 * of a default value to be returned by the <code>get</code> method when it is
 * called with a key which is not in the map.  (Otherwise, <code>get</code> returns
 * <code>null</code> in that case.)  This is particularly handy for map
 * <i>chaining</i>, in which the range values in one map are themselves maps.  For
 * example, if the outer map is created like this:
 *
 * <pre>
 *     PureMap<K1, PureMap<K2, V>> map =
 *         PureTreeMap.<K1, PureMap<K2, V>>withDefault(new PureTreeMap<K2, V>());
 * </pre>
 *
 * the chained mapping <code>key1 -> key2 -> val</code> can then be added like this:
 *
 * <pre>
 *     map = map.with(key1, map.get(key1).with(key2, val));
 * </pre>
 *
 * which works even if <code>map</code> does not already contain an entry for
 * <code>key1</code>.
 *
 * @author Scott L. Burson.
 */

public interface PureMap<Key, Val>
    extends Map<Key, Val>, Iterable<Map.Entry<Key, Val>>
{

    /**
     * Returns an arbitrary pair of the map as a {@link java.util.Map.Entry}, or
     * null if the map is empty.  <i>All</i> this guarantees is that if the map is
     * nonempty, the returned pair will be in the map; no other assumptions should
     * be made.  Specifically, it is not required to select a different pair when
     * invoked repeatedly on a given map, nor is it required to return the same pair
     * when invoked on two equal maps.
     *
     * (Note that the behavior of this method when this map is empty contrasts with
     * that of {@link SortedMap#firstKey} and {@link SortedMap#lastKey}, which throw
     * <code>NoSuchElementException</code>.)
     *
     * @return some pair of the map, or null if none */
    public Map.Entry<Key, Val> arb();

    /**
     * Returns a new map which maps <code>key</code> to <code>value</code>, and
     * which otherwise contains exactly the same mappings as this map.
     *
     * <p>The default value of the result (the value returned by <code>get</code> of
     * a key not in the map) is that of this map.
     *
     * @param key the key whose value is to be added or changed
     * @param value the new value
     * @return the updated map
     * @throws ClassCastException if the class of the key or value prevents it from
     * being stored in this map (most likely because the key was unacceptable to the
     * map's comparison method)
     * @throws NullPointerException if this map does not permit null keys or values,
     * and the key or value was <code>null</code>.
     * @throws IllegalArgumentException if some aspect of the specified key or value
     * makes it incompatible with the map
     */
    public PureMap<Key, Val> with(Key key, Val value);

    /**
     * Returns a new map which contains no mapping for <code>key</code>, and which
     * otherwise contains exactly the same mappings as this map.  (No exception is
     * thrown if this map did not contain a mapping for <code>key</code>; in that
     * case, the returned map is equal to this map.)
     *
     * <p>The default value of the result (the value returned by <code>get</code> of
     * a key not in the map) is that of this map.
     *
     * @param key the key to be removed
     * @return the updated map
     * @throws ClassCastException if the class of the key or value prevents it from
     * being stored in this map (most likely because the key was unacceptable to the
     * map's comparison method)
     * @throws NullPointerException if this map does not permit null keys or values,
     * and the key or value was <code>null</code>.
     * @throws IllegalArgumentException if some aspect of the specified key or value
     * makes it incompatible with the map
     */
    public PureMap<Key, Val> less(Key key);

    /**
     * Returns the domain of this map (the set of keys it contains).  A synonym for
     * <code>keySet</code>.
     *
     * @return the domain set of this map
     */
    public PureSet<Key> domain();

    /**
     * Returns the range of the map (the set of values it contains).  A synonym for
     * <code>values</code>.
	 *
     * The class and comparator of the returned set depend on those of this map; but
     * see the second form of <code>range</code> below.
     *
     * @return the range set of this map
     */
    public PureSet<Val> range();

    /**
     * Returns the range of the map (the set of values it contains).  This version
     * allows control over the class and comparator used for the result.  The result
     * is of the same class and has the same comparator as <code>initialSet</code>.
     * (The contents of <code>initialSet</code> are ignored.)
     *
     * @param initial_set the set whose class and comparator will be used for the
     * result
     * @return the range set of this map
     */
    public PureSet<Val> range(PureSet<Val> initialSet);

    /**
     * Returns the map as a set of pairs, each pair being a <code>Map.Entry</code>.
     * The class and comparator of the returned set depend on those of this map; but
     * see the second form of <code>entrySet</code> below.
     *
     * @return the set of entries this map contains
     */
    public PureSet<Map.Entry<Key, Val>> toSet();

    /**
     * Returns the map as a set of pairs, each pair being a <code>Map.Entry</code>.
     * This version allows control over the class and comparator used for the result.
     * The result is of the same class and (if applicable) has the same comparator as
     * <code>initialSet</code>.  (The contents of <code>initialSet</code> are
     * ignored.)
     *
     * @param initial_set the set whose class and comparator will be used for the
     * result
     * @return the set of entries this map contains
     */
    public PureSet<Map.Entry<Key, Val>> toSet(PureSet<Map.Entry<Key, Val>> initialSet);

    /**
     * Adds the pairs of <code>withMap</code> to this map, returning the result.
     * The domain of the result is the union of the domains of the two maps; for
     * each key, if it appears in both maps, its value in the result is that in
     * <code>with_map</code>, else its value is that in whichever map it appeared
     * in.  That is, the values in <code>with_map</code> take precedence.
     *
     * <p>The default value of the result (the value returned by <code>get</code> of
     * a key not in the map) is that of this map.
     *
     * @param with_map the map to merge with
     * @return the result of the merge
     */
    public PureMap<Key, Val> union(PureMap<? extends Key, ? extends Val> withMap);

    /**
     * Returns a new map whose domain is the intersection of the domain of this
     * map and `set', and whose value for each key is the same as that of this
     * map.  That is, the result contains every pair of this map whose key 
     * appears in <code>set</code>, and no others.
     *
     * <p>The default value of the result (the value returned by <code>get</code> of
     * a key not in the map) is that of this map.
     *
     * @param set the set of keys to appear in the result
     * @return the restricted map
     */
    public PureMap<Key, Val> restrictedTo(PureSet<Key> set);

    /**
     * Returns a new map whose domain is the difference of the domain of this map
     * and <code>set</code>, and whose value for each key is the same as that of
     * this map.  That is, the result contains every pair of this map whose key does
     * <i>not</i> appear in <code>set</code>.
     *
     * <p>The default value of the result (the value returned by <code>get</code> of
     * a key not in the map) is that of this map.
     *
     * @param set the set of keys to appear in the result
     * @return the restricted map
     */
    public PureMap<Key, Val> restrictedFrom(PureSet<Key> set);

    /**
     * Returns the default value for the map.
     *
     * Returns the value that <code>get</code> will return when invoked on a key
     * which is not in the map.  This is <code>null</code> by default, but can be
     * some other value if the map was created with the <code>withDefault</code>
     * static factory method.
     *
     * @return the default value for the map
     */
    public Val getDefault();

    /**
     * Returns an iterator that enumerates the pairs of this map.
     *
     * <p>{@link Map} does not declare an <code>iterator</code> method.  Rather, it
     * requires clients to call <code>entrySet</code>, then call
     * <code>iterator</code> on the result.  We encourage <code>PureMap</code>
     * clients to call <code>iterator</code> directly on the map, though both
     * protocols are supported.
     *
     * @return the iterator
     */
    public Iterator<Map.Entry<Key, Val>> iterator();

}
