/*
 * AbstractPureSet.java
 *
 * Copyright (c) 2013, 2014 Scott L. Burson.
 *
 * This file is licensed under the Library GNU Public License (LGPL), v. 2.1.
 */


package com.ergy.fset;
import java.util.*;

/**
 * This class provides a skeletal implementation of the PureSet interface.
 * 
 * <p>The purpose of this class is to provide methods for all the mutating
 * operations of the {@link Set} interface, that throw
 * <code>UnsupportedOperationException</code>, and to provide a method for
 * <code>clone</code> which simply returns <code>this</code>.
 *
 * @author Scott L. Burson
 */

public abstract class AbstractPureSet<Elt>
    extends AbstractSet<Elt>
    implements PureSet<Elt>
{

    /**
     * Unsupported.
     */
    public final boolean add(Elt e) {
	throw new UnsupportedOperationException();
    }

    /**
     * Unsupported.
     */
    public final boolean addAll(Collection<? extends Elt> c) {
	throw new UnsupportedOperationException();
    }

    /**
     * Unsupported.
     */
    public final void clear() {
	throw new UnsupportedOperationException();
    }

    /**
     * Unsupported.
     */
    public final boolean remove(Object e) {
	throw new UnsupportedOperationException();
    }

    /**
     * Unsupported.
     */
    public final boolean removeAll(Collection<?> c) {
	throw new UnsupportedOperationException();
    }

    /**
     * Unsupported.
     */
    public final boolean retainAll(Collection<?> c) {
	throw new UnsupportedOperationException();
    }

}

