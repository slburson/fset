/*
 * AbstractPureMap.java
 *
 * Copyright (c) 2013 Scott L. Burson.
 *
 * This file is licensed under the Library GNU Public License (LGPL).
 */


package com.ergy.fset;
import java.util.*;

/**
 * This class provides a skeletal implementation of the PureMap interface.  It
 * exists for three reasons: (1) to provide methods for all the mutating operations
 * which throw <code>UnsupportedOperationException</code>; (2) to provide a method
 * for <code>clone</code> which simply returns <code>this</code>; and (3) to declare
 * the <code>cacheEqual</code> method, a protected method used to cache the results
 * of equality tests in certain cases.
 *
 * @author Scott L. Burson
 */

public abstract class AbstractPureMap<Key, Val>
    extends AbstractMap<Key, Val>
    implements PureMap<Key, Val>, Cloneable
{

    /**
     * Unsupported.
     */
    public final void clear() {
	throw new UnsupportedOperationException();
    }

    /**
     * Returns this map.
     */
    public final AbstractPureMap<Key, Val> clone() {
	return this;
    }

    /**
     * Unsupported.
     */
    public final Val put(Key key, Val value) {
	throw new UnsupportedOperationException();
    }

    /**
     * Unsupported.
     */
    public final void putAll(Map<? extends Key, ? extends Val> m) {
	throw new UnsupportedOperationException();
    }

    /**
     * Unsupported.
     */
    public final Val remove(Object key) {
	throw new UnsupportedOperationException();
    }

}

