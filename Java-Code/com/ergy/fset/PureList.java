/*
 * PureList.java
 *
 * Copyright (c) 2013 Scott L. Burson.
 *
 * This file is licensed under the Library GNU Public License (LGPL).
 */


package com.ergy.fset;
import java.util.*;

/**
 * A list (sequence) for which the update operators are all pure (functional): they
 * return a new list rather than modifying the existing one.
 *
 * <p>Although this interface extends {@link List} of the Java Collections Framework,
 * and thus is somewhat integrated into that framework, it is not used the same way
 * as the <code>java.util</code> classes that implement <code>List</code>.  It does
 * not support the update operators declared by <code>List</code> (which are
 * documented as optional, anyway); in their place it adds several new operators
 * which are "pure", in the sense that rather than modifying the list in place, they
 * construct and return a new list.
 * 
 * @author Scott L. Burson.
 */

public interface PureList<Elt> extends List<Elt> {

    /**
     * Returns a new list which has <code>elt</code> at position <code>index</code>,
     * in place of the previous element.  The the other elements are the same as
     * those of this list.  If <code>index</code> is equal to the size of the list,
     * the returned list is one element longer.
     *
     * @param index the index of the element to substitute
     * @param elt the new element
     * @return the result list
     * @throws IndexOutOfBoundsException if <code>index < 0</code> or
     * <code>index > size()</code>
     */
    PureList<Elt> with(int index, Elt elt);

    /**
     * Returns a new list which has <code>elt</code> inserted at position
     * <code>index</code>, which may be equal to <code>size()</code>.  The size is
     * one greater than that of this list; the elements at positions
     * <code>index</code> and above are moved one position to the right.
     *
     * @param index the index at which to insert
     * @param elt the new element
     * @return the result list
     * @throws IndexOutOfBoundsException if <code>index < 0</code> or
     * <code>index > size()</code>
     */
    PureList<Elt> withInserted(int index, Elt elt);

    /**
     * Returns a new list which has <code>elt</code> prepended to the the contents
     * of this list.  The size is one greater than that of this list;
     * <code>elt</code> appears at position 0, and the elements of this list are
     * moved one position to the right.
     *
     * @param elt the element to be prepended
     * @return the result list
     */
    PureList<Elt> withFirst(Elt elt);

    /**
     * Returns a new list which has <code>elt</code> appended to the contents of
     * this list.  The size is one greater than that of this list; <code>elt</code>
     * appears at the end.
     *
     * @param elt the element to be appended
     * @return the result list
     */
    PureList<Elt> withLast(Elt elt);

    /**
     * Returns a new list from which the element at position <code>index</code> has
     * been omitted.  The size is one less than that of this list; the elements at
     * positions <code>index + 1</code> and above are moved one position to the
     * left.
     *
     * @param index the index of the element to omit
     * @return the result list
     * @throws IndexOutOfBoundsException if <code>index < 0</code> or
     * <code>index >= size()</code>
     */
    PureList<Elt> less(int index);

    /**
     * Returns the concatenation of this list with the argument list.  The size is
     * the sum of the sizes of this list and <code>list</code>; the elements of this
     * list appear first, followed by the elements of <code>list</code>.
     *
     * @param list the list to be concatenated
     * @return the result list
     */
    PureList<Elt> concat(List<? extends Elt> list);

    /**
     * Returns this list in reverse order.
     *
     * @return the reversed list
     */
    PureList<Elt> reverse();

    /**
     * Returns the portion of this list between position <code>fromIndex</code>,
     * inclusive, to position <code>toIndex</code>, exclusive.
     *
     * <p>The list returned is a <code>PureList</code>, though it can't be declared
     * as such because this method is overriding <code>List.SubList</code>, which is
     * declared to return a <code>List</code> (and Java does not (yet) allow
     * covariant return types on methods).
     *
     * @throws IndexOutOfBoundsException
     */
    PureList<Elt> subList(int fromIndex, int toIndex);

    /**
     * Returns the portion of this list between position <code>fromIndex</code>,
     * inclusive, to position <code>toIndex</code>, exclusive.
     *
     * <p>This is just like <code>subList</code> except that this method never throws
     * <code>IllegalArgumentException</code>; if the arguments are out of order, it
     * simply returns an empty list. 
     *
     * @throws IndexOutOfBoundsException
     */
    PureList<Elt> subseq (int fromIndex, int toIndex);

    /**
     * Returns a new list in which the elements of this list are sorted by their
     * natural ordering.  The elements of this list must implement
     * <code>Comparable</code>, and must be mutually acceptable to one another's
     * <code>compareTo</code> methods.  The sort is stable.
     *
     * @return the sorted list
     */
    PureList<Elt> sort();

    /**
     * Returns a new list in which the elements of this list are sorted according to
     * <code>comp</code>.  The sort is stable.
     *
     * @param comp the comparator to use for sorting
     * @return the sorted list
     */
    PureList<Elt> sort(Comparator<? super Elt> comp);

}
