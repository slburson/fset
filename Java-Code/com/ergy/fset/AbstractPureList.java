/*
 * AbstractPureList.java
 *
 * Copyright (c) 2013, 2014 Scott L. Burson.
 *
 * This file is licensed under the Library GNU Public License (LGPL), v. 2.1.
 */


package com.ergy.fset;
import java.util.*;

/**
 * This class provides a skeletal pure (functional) implementation of the List
 * interface.
 * 
 * <p>The purpose of this class is to provide methods for all the mutating operations
 * which throw <code>UnsupportedOperationException</code>, and one for
 * <code>clone</code> which simply returns <code>this</code>.
 *
 * @author Scott L. Burson
 */

public abstract class AbstractPureList<Elt>
    // can't extend AbstractPureList<Elt> because of subList return type
    extends AbstractCollection<Elt>
    implements PureList<Elt> {

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
