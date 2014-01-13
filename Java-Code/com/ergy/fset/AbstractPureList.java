/*
 * AbstractPureList.java
 *
 * Copyright (c) 2013 Scott L. Burson.
 *
 * This file is licensed under the Library GNU Public License (LGPL).
 */


package com.ergy.fset;
import java.util.*;

/**
 * This class provides a skeletal pure (functional) implementation of the List
 * interface.
 * 
 * <p>The primary purpose of this class is to provide methods for all the mutating
 * operations which throw <code>UnsupportedOperationException</code>, and one for
 * <code>clone</code> which simply returns <code>this</code>.  It also declares the
 * <code>cacheEqual</code> method, a protected method used to cache the results of
 * equality tests in certain cases.
 *
 * @author Scott L. Burson
 */

public abstract class AbstractPureList<Elt>
    // can't extend AbstractPureList<Elt> because of subList return type
    extends AbstractCollection<Elt>
    implements PureList<Elt>, Cloneable {

    /**
     * Unsupported.
     */
    public final boolean add(Elt e) {
	throw new UnsupportedOperationException();
    }

    /**
     * Unsupported.
     */
    public final void add(int index, Elt e) {
	throw new UnsupportedOperationException();
    }

    /**
     * Unsupported.
     */
    public final boolean addAll(int index, Collection<? extends Elt> c) {
	throw new UnsupportedOperationException();
    }

    /**
     * Unsupported.
     */
    public final void clear() {
	throw new UnsupportedOperationException();
    }

    /**
     * Returns this map.
     * &&& No, we want to push this method down at least to PureSet etc., so we can
     * take advantage of the covariant return type.
     */
    public final AbstractPureList<Elt> clone() {
	return this;
    }

    /**
     * Unsupported.
     */
    public final Elt remove(int index) {
	throw new UnsupportedOperationException();
    }

    /**
     * Unsupported.
     */
    public final boolean removeRange(int fromIndex, int toIndex) {
	throw new UnsupportedOperationException();
    }

    /**
     * Unsupported.
     */
    public final Elt set(int index, Elt e) {
	throw new UnsupportedOperationException();
    }

}
