/*
 * PureSet.java
 *
 * Copyright (c) 2013, 2014 Scott L. Burson.
 *
 * This file is licensed under the Library GNU Public License (LGPL), v. 2.1.
 */


package com.ergy.fset;
import java.util.*;

/**
 * A set for which the update operators are all pure (functional): they return a
 * new set rather than modifying the one they were invoked on.
 *
 * <p>Although this interface extends {@link Set} of the Java Collections Framework,
 * and thus is somewhat integrated into that framework, it is not used the same way
 * as the <code>java.util</code> classes that implement <code>Set</code>.  It does
 * not support the update operators declared by <code>Set</code> (which are
 * documented as optional, anyway); in their place it adds several new operators
 * which are "pure", in the sense that rather than modifying the set in place, they
 * construct and return a new set.
 *
 * @author Scott L. Burson
 */

public interface PureSet<Elt> extends Set<Elt>
{

    /**
     * Returns an arbitrary element of the set, if the set is nonempty.  <i>All</i>
     * this guarantees is that if the set is nonempty, the returned object will be an
     * element of the set; no other assumptions should be made.  Specifically, it is
     * not required to select a different element when invoked repeatedly on a given
     * set, nor is it required to return the same element when invoked on two equal
     * sets.
     *
     * @return some element of the set
     * @throws NoSuchElementException if the set is empty
     */
    Elt arb();

    /**
     * Adds <code>elt</code>, returning a (possibly) new set.  More formally, if the
     * set already contains an element <code>e</code> such that <code>(elt == null ?
     * e == null : elt.equals(e)), returns <code>this</code> or a set equal to it;
     * otherwise returns a new set containing <code>elt</code> as well as all the
     * elements of this set.
     *
     * @param elt element to be added to the set
     * @return the possibly updated set
     * @throws ClassCastException if the class of <code>elt</code> is incompatible
     * with the set
     * @throws NullPointerException if <code>elt</code> is null and this set does
     * not support null elements
     * @throws IllegalArgumentException if some aspect of <code>elt</code> makes it
     * incompatible with the set
     */
    PureSet<Elt> with(Elt elt);

    /**
     * Removes <code>elt</code>, returning a (possibly) new set.  More formally, if
     * the set does not already contain an element <code>e</code> such that
     * <code>(elt == null ? e == null : elt.equals(e))</code>, returns
     * <code>this</code> or a set equal to it; otherwise returns a new set
     * containing all elements of this set except <code>e</code>.
     *
     * @param elt element to be removed from the set
     * @return the possibly updated set
     * @throws ClassCastException if the class of <code>elt</code> is incompatible
     * with the set
     * @throws NullPointerException if <code>elt</code> is null and this set does
     * not support null elements
     * @throws IllegalArgumentException if some aspect of <code>elt</code> makes it
     * incompatible with the set
     */
    PureSet<Elt> less(Elt elt);

    /**
     * Returns the union of this set with <code>coll</code>.  That is, returns a set
     * containing all elements that are in either this set, or <code>coll</code>, or
     * both.  The returned set is of the same class as this set and uses the same
     * ordering.
     *
     * @param coll the set to take the union with
     * @return the union of the two sets
     * @throws ClassCastException if <code>coll</code> contains elements whose class is
     * incompatible with this set or its <code>Comparator</code>
     */
    PureSet<Elt> union(Collection<? extends Elt> coll);

    /**
     * Returns the intersection of this set with <code>coll</code>.  That is, returns a
     * set containing all elements that are in both this set and <code>coll</code>.
     * The returned set is of the same class as this set and uses the same ordering.
     *
     * @param coll the set to take the intersection with
     * @return the intersection of the two sets
     * @throws ClassCastException if elements of this set are incompatible with
     * <code>coll</code> or its comparator, or conversely
     */
    PureSet<Elt> intersection(Collection<? extends Elt> coll);

    /**
     * Returns the difference of this set less <code>coll</code>.  That is, returns a
     * set containing all elements that are in this set and not in <code>coll</code>.
     * The returned set is of the same class as this set and uses the same ordering.
     *
     * @param coll the set to take the difference with
     * @return the difference of the two sets (this set less <code>coll</code>)
     * @throws ClassCastException if elements of this set are incompatible with
     * <code>coll</code> or its comparator, or conversely
     */
    PureSet<Elt> difference(Collection<? extends Elt> coll);

    /**
     * Returns true if this set is a subset of <code>coll</code>.  That is, returns
     * true if <code>coll</code> contains all elements of this set.  The inclusion need
     * not be proper; that is, this method returns true if the two sets are equal.
     *
     * @param coll the collection to compare against
     * @return whether this set is a subset of <code>coll</code>
     * @throws ClassCastException if elements of this set are incompatible with
     * <code>coll</code> or its comparator, or conversely
     */
    boolean isSubset(Collection<?> coll);

    /**
     * Returns true if this set is a superset of <code>coll</code>.  That is, returns
     * true if this set contains all elements of <code>coll</code>.  The inclusion need
     * not be proper; that is, this method returns true if the two sets are equal.
     * (Synonym for <code>containsAll</code>.)
     *
     * @param coll the collection to compare against
     * @return whether this set is a superset of <code>coll</code>
     * @throws ClassCastException if elements of this set are incompatible with
     * <code>coll</code> or its comparator, or conversely
     */
    boolean isSuperset(Collection<?> coll);

}
