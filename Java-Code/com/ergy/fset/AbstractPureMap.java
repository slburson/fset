/*
 * AbstractPureMap.java
 *
 * Copyright (c) 2013, 2014 Scott L. Burson.
 *
 * This file is licensed under the Library GNU Public License (LGPL), v. 2.1.
 */


package com.ergy.fset;
import java.util.*;

/**
 * This class provides a skeletal implementation of the PureMap interface.
 * It exists to provide methods for all the mutating operations which throw
 * <code>UnsupportedOperationException</code>, and to provide a method for
 * <code>clone</code> which simply returns <code>this</code>.
 *
 * @author Scott L. Burson
 */

public abstract class AbstractPureMap<Key, Val>
    extends AbstractMap<Key, Val>
    implements PureMap<Key, Val>
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

