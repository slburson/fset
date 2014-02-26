/*
 * TestSuite.java
 *
 * Copyright (c) 2013, 2014 Scott L. Burson.
 *
 * This file is licensed under the Library GNU Public License (LGPL), v. 2.1.
 */


package com.ergy.fset;
import java.util.*;
import java.io.*;

public class TestSuite {

    public static void main(String[] args) {
	if (false) {
	    /* Some basic tests to see if anything is working at all */
	    int[] test0 = { 130, 115, 90, 1025, 330, 475, 190, 515, 290, 1770, 1505, 25,
			    180, 115, 325, 660, 1920, 1440, 960, 1280 };
	    PureTreeSet<MyInteger> set0 = new PureTreeSet<MyInteger>(conv(test0));
	    println(set0.dump());
	    println(set0);
	    int[] test1 = { 215, 775, 180, 625, 1960, 25, 525, 415, 325, 705, 800, 360,
			    485, 270, 1025, 890, 830, 715, 665, 305, 240, 695, 215 };
	    PureTreeSet<MyInteger> set1 = new PureTreeSet<MyInteger>(conv(test1));
	    println(set1.dump());
	    println(set1);
	    PureTreeSet<MyInteger> set0u1 = set0.union(set1);
	    println(set0u1.dump());
	    println(set0u1);
	    PureTreeSet<MyInteger> set0i1 = set0.intersection(set1);
	    println(set0i1.dump());
	    println(set0i1);
	    PureTreeSet<MyInteger> set0d1 = set0.difference(set1);
	    println(set0d1.dump());
	    println(set0d1);
	}
	if (args.length != 1) {
	    println("Usage: java TestSuite [n_iterations]");
	    exit();
	}
	int n_iterations = Integer.decode(args[0]).intValue();
	// For now, use a fixed seed for repeatability.
	Random rand = new Random(0xdeadbeefcafeL);
	for (int i = 0; i < n_iterations; ++i) {
	    PureTreeSet<MyInteger> pts = testPureTreeSet(rand, i);
	    PureHashSet<MyInteger> phs = testPureHashSet(rand, i);
	    //PureSet pchs = testPureCachedHashSet(rand, i);
	    testPureTreeMap(rand, i, pts);
	    testPureHashMap(rand, i, phs);
	    //testPureCachedHashMap(rand, i, pchs);
	    testPureTreeList(rand, i);
	}
	println("All tests passed.");
    }

    static PureTreeSet<MyInteger> testPureTreeSet(Random rand, int i) {
	PureTreeSet<MyInteger> pts0 = new PureTreeSet<MyInteger>(TestComparator.Instance);
	TreeSet<MyInteger> ts0 = new TreeSet<MyInteger>();
	for (int j = 0; j < 100; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = new MyInteger(r);
	    PureTreeSet<MyInteger> tmp = pts0.with(R);
	    ts0.add(R);
	    if (!tmp.verify()) {
		println("PureTreeSet Verification failure on iteration " + i);
		println(pts0.dump());
		println("Adding " + R);
		println(tmp.dump());
		exit();
	    }
	    if (tmp.size() != ts0.size()) {
		println("PureTreeSet size failed on iteration " + i);
		exit();
	    }
	    if (!pts0.isSubset(tmp) || !tmp.isSuperset(pts0) ||
		(!pts0.contains(R) && (tmp.isSubset(pts0) || pts0.isSuperset(tmp)))) {
		println("PureTreeSet is{Sub,Super}set failed on iteration " + i);
		println(pts0.isSubset(tmp) + ", " + tmp.isSuperset(pts0) + ", " +
			pts0.contains(R) + ", " + tmp.isSubset(pts0) + ", " +
			pts0.isSuperset(tmp));
		exit();
	    }
	    pts0 = tmp;
	}
	PureTreeSet<MyInteger> pts1 = new PureTreeSet<MyInteger>(TestComparator.Instance);
	TreeSet<MyInteger> ts1 = new TreeSet<MyInteger>();
	for (int j = 0; j < 100; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = new MyInteger(r);
	    PureTreeSet<MyInteger> tmp = pts1.with(R);
	    ts1.add(R);
	    if (!tmp.verify()) {
		println("PureTreeSet Verification failure on iteration " + i);
		println(pts1.dump());
		println("Adding " + R);
		println(tmp.dump());
		exit();
	    }
	    if (tmp.size() != ts1.size()) {
		println("PureTreeSet size failed on iteration " + i);
		exit();
	    }
	    if (!pts1.isSubset(tmp) || !tmp.isSuperset(pts1) ||
		(!pts1.contains(R) && (tmp.isSubset(pts1) || pts1.isSuperset(tmp)))) {
		println("PureTreeSet is{Sub,Super}set failed on iteration " + i);
		println(pts1.isSubset(tmp) + ", " + tmp.isSuperset(pts1) + ", " +
			pts1.contains(R) + ", " + tmp.isSubset(pts1) + ", " +
			pts1.isSuperset(tmp));
		exit();
	    }
	    pts1 = tmp;
	}
	for (int j = 0; j < 20; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = new MyInteger(r);
	    if (pts0.contains(R) != ts0.contains(R)) {
		println("PureTreeSet contains failed (pts0) on iteration " + i);
		exit();
	    }
	    PureTreeSet<MyInteger> tmp = pts0.less(R);
	    ts0.remove(R);
	    if (!tmp.verify()) {
		println("PureTreeSet Verification failure on iteration " + i);
		println(pts0.dump());
		println("Removing " + R);
		println(tmp.dump());
		exit();
	    }
	    if (tmp.size() != ts0.size()) {
		println("PureTreeSet size failed on iteration " + i);
		exit();
	    }
	    pts0 = tmp;
	}
	for (int j = 0; j < 20; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = new MyInteger(r);
	    if (pts1.contains(R) != ts1.contains(R)) {
		println("PureTreeSet contains failed (pts1) on iteration " + i);
		exit();
	    }
	    PureTreeSet<MyInteger> tmp = pts1.less(R);
	    ts1.remove(R);
	    if (!tmp.verify()) {
		println("PureTreeSet Verification failure on iteration " + i);
		println(pts1.dump());
		println("Removing " + R);
		println(tmp.dump());
		exit();
	    }
	    if (tmp.size() != ts1.size()) {
		println("PureTreeSet size failed on iteration " + i);
		exit();
	    }
	    pts1 = tmp;
	}
	if (i == 0) {
	    PureTreeSet<MyInteger> tmp = pts0.with(null);
	    if (!tmp.verify() || !tmp.contains(null) || tmp.first() != null) {
		println("PureTreeSet Verification failure on iteration " + i);
		println(pts0.dump());
		println("Adding null");
		println(tmp.dump());
		exit();
	    }
	    tmp = tmp.less(null);
	    if (!tmp.verify() || tmp.contains(null)) {
		println("PureTreeSet Verification failure on iteration " + i);
		println(pts0.dump());
		println("Removing null");
		println(tmp.dump());
		exit();
	    }
	}		
	if (pts0.hashCode() != ts0.hashCode()) {
	    println("PureTreeSet hashCode failed on pts0 on iteration " + i);
	    println(pts0);
	    println(ts0);
	    exit();
	}
	if (pts1.hashCode() != ts1.hashCode()) {
	    println("PureTreeSet hashCode failed on pts1 on iteration " + i);
	    exit();
	}
	if (!pts0.equals(ts0)) {
	    println("PureTreeSet Equality failed (pts0, A) on iteration " + i);
	    exit();
	}
	if (!pts0.equals(new PureTreeSet<MyInteger>(ts0))) {
	    println("PureTreeSet Equality failed (pts0, B) on iteration " + i);
	    exit();
	}
	if (!pts0.equals(new PureTreeSet<MyInteger>(ts0, TestComparator.Instance))) {
	    println("PureTreeSet Equality failed (pts0, C) on iteration " + i);
	    println(pts0);
	    PureTreeSet<MyInteger> npts0 = new PureTreeSet<MyInteger>(ts0, TestComparator.Instance);
	    println(npts0);
	    println(npts0.dump());
	    exit();
	}
	if (!pts0.equals(new PureTreeSet<MyInteger>(new ArrayList<MyInteger>(ts0)))) {
	    println("PureTreeSet construction from ArrayList failed (pts0) on iteration "
		    + i);
	    exit();
	}
/*
	if (!pts0.equals(new PureTreeSet<MyInteger>(ts0.toArray()))) {
	    println("PureTreeSet construction from array failed (pts0) on iteration "
		    + i);
	    exit();
	}
*/
	if (!pts1.equals(ts1)) {
	    println("PureTreeSet Equality failed (pts1, A) on iteration " + i);
	    exit();
	}
	// Next line also tests constructor from `Object[]'
/*
	if (!pts1.equals(new PureTreeSet<MyInteger>(ts1.toArray()))) {
	    println("PureTreeSet Equality failed (pts1, B) on iteration " + i);
	    exit();
	}
	// Next line also tests constructor from `Object[]'
	if (!pts1.equals(new PureTreeSet(ts1.toArray(), TestComparator.Instance))) {
	    println("PureTreeSet Equality failed (pts1, C) on iteration " + i);
	    exit();
	}
*/
	if (pts0.first().intValue() / 2 != ts0.first().intValue() / 2) {
	    println("PureTreeSet `first' failed (pts0) on iteration " + i);
	    exit();
	}
	if (pts1.first().intValue() / 2 != ts1.first().intValue() / 2) {
	    println("PureTreeSet `first' failed (pts1) on iteration " + i);
	    exit();
	}
	if (pts0.last().intValue() / 2 != ts0.last().intValue() / 2) {
	    println("PureTreeSet `last' failed (pts0) on iteration " + i);
	    exit();
	}
	if (pts1.last().intValue() / 2 != ts1.last().intValue() / 2) {
	    println("PureTreeSet `last' failed (pts1) on iteration " + i);
	    exit();
	}
	PureTreeSet<MyInteger> ptsu = pts0.union(pts1);
	TreeSet<MyInteger> tsu = (TreeSet<MyInteger>)ts0.clone();
	tsu.addAll(ts1);
	if (!((PureTreeSet<MyInteger>)ptsu).verify() || !ptsu.equals(tsu)) {
	    println("PureTreeSet Union failed on iteration " + i);
	    println(pts0);
	    println(pts1);
	    if (!ptsu.verify())
		println(ptsu.dump());
	    println(ptsu.size() + ", " + tsu.size());
	    println(ptsu);
	    println(tsu);
	    exit();
	}
	if (!ptsu.equals(new PureTreeSet<MyInteger>(tsu))) {
	    println("PureTreeSet Equality failed (ptsu) on iteration " + i);
	}
	PureTreeSet<MyInteger> ptsi = pts0.intersection(pts1);
	TreeSet<MyInteger> tsi = (TreeSet<MyInteger>)ts0.clone();
	tsi.retainAll(ts1);
	if (!ptsi.verify() || !ptsi.equals(tsi)) {
	    println("PureTreeSet Intersection failed on iteration " + i);
	    println(pts0);
	    println(pts1);
	    if (!ptsi.verify())
		println(ptsi.dump());
	    println(ptsi.size() + ", " + tsi.size());
	    println(ptsi);
	    println(tsi);
	    exit();
	}
	if (!ptsi.isSubset(pts0) || !ptsi.isSubset(pts1)) {
	    println("PureTreeSet isSubset failed on iteration " + i);
	    exit();
	}
	if (!ptsi.equals(new PureTreeSet<MyInteger>(tsi))) {
	    println("PureTreeSet Equality failed (ptsi) on iteration " + i);
	}
	PureTreeSet<MyInteger> ptsd = pts0.difference(pts1);
	TreeSet<MyInteger> tsd = (TreeSet<MyInteger>)ts0.clone();
	tsd.removeAll(ts1);
	if (!ptsd.verify() || !ptsd.equals(tsd)) {
	    println("PureTreeSet Difference failed on iteration " + i);
	    println(pts0);
	    println(pts0.dump());
	    println(pts1);
	    println(pts1.dump());
	    //if (!((PureTreeSet)ptsd).verify())
	    println(ptsd.size() + ", " + tsd.size());
	    println(ptsd);
	    println(ptsd.dump());
	    println(tsd);
	    exit();
	}
	if (!ptsd.equals(new PureTreeSet<MyInteger>(tsd))) {
	    println("PureTreeSet Equality failed (ptsd) on iteration " + i);
	}
	PureTreeSet<MyInteger> npts0 = new PureTreeSet<MyInteger>(pts0, TestComparator.Instance);
	npts0 = npts0.less(pick(rand, npts0));
	PureTreeSet<MyInteger> pts0a = pts0.less(pick(rand, pts0));
	if (sgn(pts0a.compareTo(npts0)) != compare(pts0a, npts0)) {
	    println("PureTreeSet Compare failed (pts0) on iteration " + i);
	    println(pts0a.dump());
	    println(npts0.dump());
	    println(pts0a);
	    println(npts0);
	    println(pts0a.compareTo(npts0));
	    println(compare(pts0a, npts0));
	    exit();
	}
	if (pts0a.equals(npts0) != equals(pts0a, npts0)) {
	    println("PureTreeSet equality failed (pts0a) on iteration " + i);
	    exit();
	}
	PureTreeSet<MyInteger> npts1 = new PureTreeSet<MyInteger>(pts1, TestComparator.Instance);
	npts1 = npts1.less(pick(rand, npts1));
	PureTreeSet<MyInteger> pts1a = pts1.less(pick(rand, pts1));
	if (sgn(pts1a.compareTo(npts1)) != compare(pts1a, npts1)) {
	    println("PureTreeSet Compare failed (pts1) on iteration " + i);
	    println(pts1a.dump());
	    println(npts1.dump());
	    println(pts1a.compareTo(npts1));
	    println(compare(pts1a, npts1));
	    exit();
	}
	if (pts1a.equals(npts1) != equals(pts1a, npts1)) {
	    println("PureTreeSet equality failed (pts1a) on iteration " + i);
	    exit();
	}
	int lo = rand.nextInt(150) - 25;
	int hi = rand.nextInt(125 - lo) + lo;
	lo *= 2;	// they have to be even because of the comparator behavior
	hi *= 2;
	MyInteger Lo = new MyInteger(lo);
	MyInteger Hi = new MyInteger(hi);
	SortedSet<MyInteger> ptss = pts0.subSet(Lo, Hi);
	SortedSet<MyInteger> tss = ts0.subSet(Lo, Hi);
	if (!ptss.equals(tss)) {
	    println("PureTreeSet subSet failed on iteration " + i);
	    println("[" + lo + ", " + hi + ")");
	    println(ptss);
	    println(tss);
	    exit();
	}
	if (!pts0.headSet(Hi).equals(ts0.headSet(Hi))) {
	    println("PureTreeSet headSet failed on iteration " + i);
	    exit();
	}
	if (!pts0.tailSet(Lo).equals(ts0.tailSet(Lo))) {
	    println("PureTreeSet tailSet failed on iteration " + i);
	    exit();
	}
	while (!pts0.isEmpty()) {
	    MyInteger x = pts0.arb();
	    if (!pts0.contains(x) || !ts0.contains(x)) {
		println("PureTreeSet arb/contains failed on iteration " + i);
		exit();
	    }
	    pts0 = pts0.less(x);
	    ts0.remove(x);
	    if (ts0.isEmpty() != pts0.isEmpty()) {
		println("PureTreeSet less/isEmpty failed on iteration " + i);
		exit();
	    }
	}
	if (i % 50 == 0) {
	    // Check handling of null set
	    try {
		PureSet<MyInteger> ptsser = (i == 0 ? pts0 : pts1);
		FileOutputStream fos = new FileOutputStream("pts.tmp");
		ObjectOutputStream oos = new ObjectOutputStream(fos);
		oos.writeObject(ptsser);
		oos.close();
		FileInputStream fis = new FileInputStream("pts.tmp");
		ObjectInputStream ois = new ObjectInputStream(fis);
		PureSet<MyInteger> nptsser = (PureSet<MyInteger>)ois.readObject();
		ois.close();
		if (!ptsser.equals(nptsser)) {
		    println("PureTreeSet read/write failed on iteration " + i);
		    exit();
		}
	    } catch (IOException e) {
		println("PureTreeSet read/write: exception " + e);
		exit();
	    } catch (ClassNotFoundException e) {
		println("PureTreeSet read/write: exception " + e);
	    }
	}
	return pts1;
    }

    static PureHashSet<MyInteger> testPureHashSet(Random rand, int i) {
	PureHashSet<MyInteger> phs0 = new PureHashSet<MyInteger>();
	HashSet<MyInteger> hs0 = new HashSet<MyInteger>();
	for (int j = 0; j < 100; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = r == 57 ? null : new MyInteger(r);
	    PureHashSet<MyInteger> tmp = phs0.with(R);
	    hs0.add(R);
	    if (!tmp.verify()) {
		println("PureHashSet Verification failure on iteration " + i);
		println(phs0.dump());
		println("Adding " + (R == null ? "null" : "" + R));
		println(tmp.dump());
		exit();
	    }
	    if (tmp.hashCode() != hs0.hashCode()) {
		println("PureHashSet hashCode failed on phs0 on iteration " + i);
		println(tmp);
		println(hs0);
		println("Adding " + R + "; " + tmp.hashCode() + ", " + hs0.hashCode());
		exit();
	    }
	    if (!phs0.isSubset(tmp) || !tmp.isSuperset(phs0) ||
		(!phs0.contains(R) && (tmp.isSubset(phs0) || phs0.isSuperset(tmp)))) {
		println("PureHashSet is{Sub,Super}set failed (phs0) on iteration " + i);
		println(phs0.isSubset(tmp) + ", " + tmp.isSuperset(phs0) + ", " +
			phs0.contains(R) + ", " + tmp.isSubset(phs0) + ", " +
			phs0.isSuperset(tmp) + "; " + R);
		println(phs0);
		println(tmp);
		//PureHashSet.debug = true;
		//phs0.isSubset(tmp);
		exit();
	    }
	    phs0 = tmp;
	}
	PureHashSet<MyInteger> phs1 = new PureHashSet<MyInteger>();
	HashSet<MyInteger> hs1 = new HashSet<MyInteger>();
	for (int j = 0; j < 100; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = r == 57 ? null : new MyInteger(r);
	    PureHashSet<MyInteger> tmp = phs1.with(R);
	    hs1.add(R);
	    if (!tmp.verify()) {
		println("PureHashSet Verification failure on iteration " + i);
		println(phs1.dump());
		println("Adding " + R);
		println(tmp.dump());
		exit();
	    }
	    if (tmp.hashCode() != hs1.hashCode()) {
		println("PureHashSet hashCode failed on phs1 on iteration " + i);
		println(tmp);
		println(hs1);
		println("Adding " + R + "; " + tmp.hashCode() + ", " + hs1.hashCode());
		exit();
	    }
	    if (!phs1.isSubset(tmp) || !tmp.isSuperset(phs1) ||
		(!phs1.contains(R) && (tmp.isSubset(phs1) || phs1.isSuperset(tmp)))) {
		println("PureHashSet is{Sub,Super}set failed (phs1) on iteration " + i);
		println(phs1.isSubset(tmp) + ", " + tmp.isSuperset(phs1) + ", " +
			phs1.contains(R) + ", " + tmp.isSubset(phs1) + ", " +
			phs1.isSuperset(tmp) + "; " + R);
		println(phs1);
		println(tmp);
		exit();
	    }
	    phs1 = tmp;
	}
	for (int j = 0; j < 20; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = r == 57 ? null : new MyInteger(r);
	    PureHashSet<MyInteger> tmp = phs0.less(R);
	    hs0.remove(R);
	    if (!tmp.verify()) {
		println("PureHashSet Verification failure on iteration " + i);
		println(phs0.dump());
		println("Removing " + (R == null ? "null" : "" + R));
		println(tmp.dump());
		exit();
	    }
	    phs0 = tmp;
	}
	for (int j = 0; j < 20; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = r == 57 ? null : new MyInteger(r);
	    PureHashSet<MyInteger> tmp = phs1.less(R);
	    hs1.remove(R);
	    if (!tmp.verify()) {
		println("PureHashSet Verification failure on iteration " + i);
		println(phs1.dump());
		println("Removing " + (R == null ? "null" : "" + R));
		println(tmp.dump());
		exit();
	    }
	    if (tmp.hashCode() != hs1.hashCode()) {
		println("PureHashSet hashCode failed on phs1 on iteration " + i);
		println(tmp);
		println(hs1);
		println("Removing " + R + "; " + tmp.hashCode() + ", " + hs1.hashCode());
		exit();
	    }
	    if (!tmp.equals(hs1)) {
		println("PureHashSet equality failed on phs1 on iteration " + i);
		println(tmp);
		println(hs1);
		println(new PureHashSet<MyInteger>(hs1));
		println("Removing " + R + "; " + tmp.hashCode() + ", " + hs1.hashCode());
		exit();
	    }
	    phs1 = tmp;
	}
	if (phs0.hashCode() != hs0.hashCode()) {
	    println("PureHashSet hashCode failed on phs0 on iteration " + i);
	    println(phs0);
	    println(hs0);
	    exit();
	}
	if (phs1.hashCode() != hs1.hashCode()) {
	    println("PureHashSet hashCode failed on phs1 on iteration " + i);
	    exit();
	}
	if (!phs0.equals(hs0)) {
	    println("PureHashSet Equality failed (phs0, A) on iteration " + i);
	    println(phs0);
	    println(phs0.dump());
	    println(new TreeSet<MyInteger>(hs0));
	    exit();
	}
	if (!phs0.equals(new PureHashSet<MyInteger>(hs0))) {
	    println("PureHashSet Equality failed (phs0, B) on iteration " + i);
	    println(phs0);
	    println(phs0.dump());
	    PureHashSet<MyInteger> nphs0 = new PureHashSet<MyInteger>(hs0);
	    println(nphs0);
	    println(nphs0.dump());
	    exit();
	}
	if (!phs0.equals(new PureHashSet<MyInteger>(new ArrayList<MyInteger>(hs0)))) {
	    println("PureHashSet construction from ArrayList failed (phs0) on iteration " + i);
	    exit();
	}
	if (!phs0.equals(new PureHashSet<MyInteger>(hs0.toArray(new MyInteger[0])))) {
	    println("PureHashSet construction from array failed (phs0) on iteration " + i);
	    exit();
	}
	if (!phs1.equals(hs1)) {
	    println("PureHashSet Equality failed (phs1, A) on iteration " + i);
	    println(phs1);
	    println(hs1);
	    println(new PureHashSet<MyInteger>(hs1));
	    exit();
	}
	// Next line also tests constructor from `Object[]'
	if (!phs1.equals(new PureHashSet<MyInteger>(hs1.toArray(new MyInteger[0])))) {
	    println("PureHashSet Equality failed (phs1, B) on iteration " + i);
	    exit();
	}
	PureHashSet<MyInteger> phsu = phs0.union(phs1);
	HashSet<MyInteger> hsu = (HashSet<MyInteger>)hs0.clone();
	hsu.addAll(hs1);
	if (!phsu.verify() || !phsu.equals(hsu)) {
	    println("PureHashSet Union failed on iteration " + i);
	    println(phs0);
	    println(phs1);
	    if (!phsu.verify()) println(phsu.dump());
	    println(phsu.size() + ", " + hsu.size());
	    println(phsu);
	    println(hsu);
	    exit();
	}
	if (!phsu.equals(new PureHashSet<MyInteger>(hsu))) {
	    println("PureHashSet Equality failed (phsu) on iteration " + i);
	}
	PureHashSet<MyInteger> phsi = phs0.intersection(phs1);
	HashSet<MyInteger> hsi = (HashSet<MyInteger>)hs0.clone();
	hsi.retainAll(hs1);
	if (!phsi.verify() || !phsi.equals(hsi)) {
	    println("PureHashSet Intersection failed on iteration " + i);
	    println(phs0);
	    println(phs0.dump());
	    println(phs1);
	    println(phs1.dump());
	    if (!phsi.verify()) println(phsi.dump());
	    println(phsi.size() + ", " + hsi.size());
	    println(phsi);
	    println(new TreeSet<MyInteger>(hsi));
	    exit();
	}
	if (!phsi.isSubset(phs0) || !phsi.isSubset(phs1)) {
	    println("PureHashSet isSubset failed on iteration " + i);
	    exit();
	}
	if (!phsi.equals(new PureHashSet<MyInteger>(hsi))) {
	    println("PureHashSet Equality failed (phsi) on iteration " + i);
	}
	PureHashSet<MyInteger> phsd = phs0.difference(phs1);
	HashSet<MyInteger> hsd = (HashSet<MyInteger>)hs0.clone();
	hsd.removeAll(hs1);
	if (!phsd.verify() || !phsd.equals(hsd)) {
	    println("PureHashSet Difference failed on iteration " + i);
	    println(phs0);
	    println((phs0).dump());
	    println(phs1);
	    println(phs1.dump());
	    //if (!phsd.verify())
	    println(phsd.size() + ", " + hsd.size());
	    println(phsd);
	    println(phsd.dump());
	    println(hsd);
	    exit();
	}
	if (!phsd.equals(new PureHashSet<MyInteger>(hsd))) {
	    println("PureHashSet Equality failed (phsd) on iteration " + i);
	}
	PureHashSet<MyInteger> nphs0 = new PureHashSet<MyInteger>(phs0);
	nphs0 = nphs0.less(pick(rand, nphs0));
	PureHashSet<MyInteger> phs0a = phs0.less(pick(rand, phs0));
	if (sgn(phs0a.compareTo(nphs0)) != compare(phs0a, nphs0)) {
	    println("PureHashSet Compare failed (phs0) on iteration " + i);
	    println(phs0a.compareTo(nphs0) + ", " + compare(phs0a, nphs0));
	    println(phs0a);
	    println(phs0a.dump());
	    println(nphs0);
	    println(nphs0.dump());
	    exit();
	}
	if (phs0a.equals(nphs0) != equals(phs0a, nphs0)) {
	    println("PureHashSet equality failed (phs0a) on iteration " + i);
	    exit();
	}
	PureHashSet<MyInteger> nphs1 = new PureHashSet<MyInteger>(phs1);
	nphs1 = nphs1.less(pick(rand, nphs1));
	PureHashSet<MyInteger> phs1a = phs1.less(pick(rand, phs1));
	if (sgn(phs1a.compareTo(nphs1)) != compare(phs1a, nphs1)) {
	    println("PureHashSet Compare failed (phs1) on iteration " + i);
	    exit();
	}
	if (phs1a.equals(nphs1) != equals(phs1a, nphs1)) {
	    println("PureHashSet equality failed (phs1a) on iteration " + i);
	    exit();
	}
	while (!phs0.isEmpty()) {
	    MyInteger x = phs0.arb();
	    if (!phs0.contains(x) || !hs0.contains(x)) {
		println("PureHashSet arb/contains failed on iteration " + i);
		exit();
	    }
	    phs0 = phs0.less(x);
	    hs0.remove(x);
	    if (hs0.isEmpty() != phs0.isEmpty()) {
		println("PureHashSet less/isEmpty failed on iteration " + i);
		exit();
	    }
	}
	if (i % 50 == 0) {
	    try {
		// Check handling of null set
		PureSet<MyInteger> phsser = (i == 0 ? phs0 : phs1);
		FileOutputStream fos = new FileOutputStream("phs.tmp");
		ObjectOutputStream oos = new ObjectOutputStream(fos);
		oos.writeObject(phsser);
		oos.close();
		FileInputStream fis = new FileInputStream("phs.tmp");
		ObjectInputStream ois = new ObjectInputStream(fis);
		PureSet<MyInteger> nphsser = (PureSet<MyInteger>)ois.readObject();
		ois.close();
		if (!phsser.equals(nphsser)) {
		    println("PureHashSet read/write failed on iteration " + i);
		    exit();
		}
	    } catch (IOException e) {
		println("PureHashSet read/write: exception " + e);
		exit();
	    } catch (ClassNotFoundException e) {
		println("PureHashSet read/write: exception " + e);
	    }
	}
	return phs1;
    }

/********
    static PureSet testPureCachedHashSet(Random rand, int i) {
	PureSet phs0 = new PureCachedHashSet();
	HashSet hs0 = new HashSet();
	for (int j = 0; j < 100; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = r == 57 ? null : new MyInteger(r);
	    PureSet tmp = phs0.with(R);
	    hs0.add(R);
	    if (!((PureCachedHashSet)tmp).verify()) {
		println("PureCachedHashSet Verification failure on iteration " + i);
		println(((PureCachedHashSet)phs0).dump());
		println("Adding " + (R == null ? "null" : "" + R));
		println(((PureCachedHashSet)tmp).dump());
		exit();
	    }
	    if (tmp.hashCode() != hs0.hashCode()) {
		println("PureCachedHashSet hashCode failed on phs0 on iteration " + i);
		println(tmp);
		println(hs0);
		println("Adding " + R + "; " + tmp.hashCode() + ", " + hs0.hashCode());
		exit();
	    }
	    if (!phs0.isSubset(tmp) || !tmp.isSuperset(phs0) ||
		(!phs0.contains(R) && (tmp.isSubset(phs0) || phs0.isSuperset(tmp)))) {
		println("PureCachedHashSet is{Sub,Super}set failed (phs0) on iteration " + i);
		println(phs0.isSubset(tmp) + ", " + tmp.isSuperset(phs0) + ", " +
			phs0.contains(R) + ", " + tmp.isSubset(phs0) + ", " +
			phs0.isSuperset(tmp) + "; " + R);
		println(phs0);
		println(tmp);
		//PureCachedHashSet.debug = true;
		//phs0.isSubset(tmp);
		exit();
	    }
	    phs0 = tmp;
	}
	PureSet phs1 = new PureCachedHashSet();
	HashSet hs1 = new HashSet();
	for (int j = 0; j < 100; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = r == 57 ? null : new MyInteger(r);
	    PureSet tmp = phs1.with(R);
	    hs1.add(R);
	    if (!((PureCachedHashSet)tmp).verify()) {
		println("PureCachedHashSet Verification failure on iteration " + i);
		println(((PureCachedHashSet)phs1).dump());
		println("Adding " + R);
		println(((PureCachedHashSet)tmp).dump());
		exit();
	    }
	    if (tmp.hashCode() != hs1.hashCode()) {
		println("PureCachedHashSet hashCode failed on phs1 on iteration " + i);
		println(tmp);
		println(hs1);
		println("Adding " + R + "; " + tmp.hashCode() + ", " + hs1.hashCode());
		exit();
	    }
	    if (!phs1.isSubset(tmp) || !tmp.isSuperset(phs1) ||
		(!phs1.contains(R) && (tmp.isSubset(phs1) || phs1.isSuperset(tmp)))) {
		println("PureCachedHashSet is{Sub,Super}set failed (phs1) on iteration " + i);
		println(phs1.isSubset(tmp) + ", " + tmp.isSuperset(phs1) + ", " +
			phs1.contains(R) + ", " + tmp.isSubset(phs1) + ", " +
			phs1.isSuperset(tmp) + "; " + R);
		println(phs1);
		println(tmp);
		exit();
	    }
	    phs1 = tmp;
	}
	for (int j = 0; j < 20; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = r == 57 ? null : new MyInteger(r);
	    PureSet tmp = phs0.less(R);
	    hs0.remove(R);
	    if (!((PureCachedHashSet)tmp).verify()) {
		println("PureCachedHashSet Verification failure on iteration " + i);
		println(((PureCachedHashSet)phs0).dump());
		println("Removing " + (R == null ? "null" : "" + R));
		println(((PureCachedHashSet)tmp).dump());
		exit();
	    }
	    phs0 = tmp;
	}
	for (int j = 0; j < 20; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = r == 57 ? null : new MyInteger(r);
	    PureSet tmp = phs1.less(R);
	    hs1.remove(R);
	    if (!((PureCachedHashSet)tmp).verify()) {
		println("PureCachedHashSet Verification failure on iteration " + i);
		println(((PureCachedHashSet)phs1).dump());
		println("Removing " + (R == null ? "null" : "" + R));
		println(((PureCachedHashSet)tmp).dump());
		exit();
	    }
	    if (tmp.hashCode() != hs1.hashCode()) {
		println("PureCachedHashSet hashCode failed on phs1 on iteration " + i);
		println(tmp);
		println(hs1);
		println("Removing " + R + "; " + tmp.hashCode() + ", " + hs1.hashCode());
		exit();
	    }
	    if (!tmp.equals(hs1)) {
		println("PureCachedHashSet equality failed on phs1 on iteration " + i);
		println(tmp);
		println(hs1);
		println(new PureCachedHashSet(hs1));
		println("Removing " + R + "; " + tmp.hashCode() + ", " + hs1.hashCode());
		exit();
	    }
	    phs1 = tmp;
	}
	if (phs0.hashCode() != hs0.hashCode()) {
	    println("PureCachedHashSet hashCode failed on phs0 on iteration " + i);
	    println(phs0);
	    println(hs0);
	    exit();
	}
	if (phs1.hashCode() != hs1.hashCode()) {
	    println("PureCachedHashSet hashCode failed on phs1 on iteration " + i);
	    exit();
	}
	if (!phs0.equals(hs0)) {
	    println("PureCachedHashSet Equality failed (phs0, A) on iteration " + i);
	    println(phs0);
	    println(((PureCachedHashSet)phs0).dump());
	    println(new TreeSet(hs0));
	    exit();
	}
	if (!phs0.equals(new PureCachedHashSet(hs0))) {
	    println("PureCachedHashSet Equality failed (phs0, B) on iteration " + i);
	    println(phs0);
	    println(((PureCachedHashSet)phs0).dump());
	    PureCachedHashSet nphs0 = new PureCachedHashSet(hs0);
	    println(nphs0);
	    println(nphs0.dump());
	    exit();
	}
	if (!phs0.equals(new PureCachedHashSet(new ArrayList(hs0)))) {
	    println("PureCachedHashSet construction from ArrayList failed (phs0) on iteration "
		    + i);
	    exit();
	}
	if (!phs0.equals(new PureCachedHashSet(hs0.toArray()))) {
	    println("PureCachedHashSet construction from array failed (phs0) on iteration "
		    + i);
	    exit();
	}
	if (!phs1.equals(hs1)) {
	    println("PureCachedHashSet Equality failed (phs1, A) on iteration " + i);
	    println(phs1);
	    println(hs1);
	    println(new PureCachedHashSet(hs1));
	    exit();
	}
	// Next line also tests constructor from `Object[]'
	if (!phs1.equals(new PureCachedHashSet(hs1.toArray()))) {
	    println("PureCachedHashSet Equality failed (phs1, B) on iteration " + i);
	    exit();
	}
	PureSet phsu = phs0.union(phs1);
	HashSet hsu = (HashSet)hs0.clone();
	hsu.addAll(hs1);
	if (!((PureCachedHashSet)phsu).verify() || !phsu.equals(hsu)) {
	    println("PureCachedHashSet Union failed on iteration " + i);
	    println(phs0);
	    println(phs1);
	    if (!((PureCachedHashSet)phsu).verify())
		println(((PureCachedHashSet)phsu).dump());
	    println(phsu.size() + ", " + hsu.size());
	    println(phsu);
	    println(hsu);
	    exit();
	}
	if (!phsu.equals(new PureCachedHashSet(hsu))) {
	    println("PureCachedHashSet Equality failed (phsu) on iteration " + i);
	}
	PureSet phsi = phs0.intersection(phs1);
	HashSet hsi = (HashSet)hs0.clone();
	hsi.retainAll(hs1);
	if (!((PureCachedHashSet)phsi).verify() || !phsi.equals(hsi)) {
	    println("PureCachedHashSet Intersection failed on iteration " + i);
	    println(phs0);
	    println(((PureCachedHashSet)phs0).dump());
	    println(phs1);
	    println(((PureCachedHashSet)phs1).dump());
	    if (!((PureCachedHashSet)phsi).verify())
		println(((PureCachedHashSet)phsi).dump());
	    println(phsi.size() + ", " + hsi.size());
	    println(phsi);
	    println(new TreeSet(hsi));
	    exit();
	}
	if (!phsi.isSubset(phs0) || !phsi.isSubset(phs1)) {
	    println("PureCachedHashSet isSubset failed on iteration " + i);
	    exit();
	}
	if (!phsi.equals(new PureCachedHashSet(hsi))) {
	    println("PureCachedHashSet Equality failed (phsi) on iteration " + i);
	}
	PureSet phsd = phs0.difference(phs1);
	HashSet hsd = (HashSet)hs0.clone();
	hsd.removeAll(hs1);
	if (!((PureCachedHashSet)phsd).verify() || !phsd.equals(hsd)) {
	    println("PureCachedHashSet Difference failed on iteration " + i);
	    println(phs0);
	    println(((PureCachedHashSet)phs0).dump());
	    println(phs1);
	    println(((PureCachedHashSet)phs1).dump());
	    //if (!((PureCachedHashSet)phsd).verify())
	    println(phsd.size() + ", " + hsd.size());
	    println(phsd);
	    println(((PureCachedHashSet)phsd).dump());
	    println(hsd);
	    exit();
	}
	if (!phsd.equals(new PureCachedHashSet(hsd))) {
	    println("PureCachedHashSet Equality failed (phsd) on iteration " + i);
	}
	PureSet nphs0 = new PureCachedHashSet(phs0);
	nphs0 = nphs0.less(pick(rand, nphs0));
	PureSet phs0a = phs0.less(pick(rand, phs0));
	if (sgn(((PureCachedHashSet)phs0a).compareTo(nphs0)) !=
	      compare(phs0a, nphs0)) {
	    println("PureCachedHashSet Compare failed (phs0) on iteration " + i);
	    println(((PureCachedHashSet)phs0a).compareTo(nphs0) + ", " +
		    compare(phs0a, nphs0));
	    println(phs0a);
	    println(((PureCachedHashSet)phs0a).dump());
	    println(nphs0);
	    println(((PureCachedHashSet)nphs0).dump());
	    exit();
	}
	if (phs0a.equals(nphs0) != equals(phs0a, nphs0)) {
	    println("PureCachedHashSet equality failed (phs0a) on iteration " + i);
	    exit();
	}
	PureSet nphs1 = new PureCachedHashSet(phs1);
	nphs1 = nphs1.less(pick(rand, nphs1));
	PureSet phs1a = phs1.less(pick(rand, phs1));
	if (sgn(((PureCachedHashSet)phs1a).compareTo(nphs1)) !=
	      compare(phs1a, nphs1)) {
	    println("PureCachedHashSet Compare failed (phs1) on iteration " + i);
	    exit();
	}
	if (phs1a.equals(nphs1) != equals(phs1a, nphs1)) {
	    println("PureCachedHashSet equality failed (phs1a) on iteration " + i);
	    exit();
	}
	while (!phs0.isEmpty()) {
	    MyInteger x = phs0.arb();
	    if (!phs0.contains(x) || !hs0.contains(x)) {
		println("PureCachedHashSet arb/contains failed on iteration " + i);
		exit();
	    }
	    phs0 = phs0.less(x);
	    hs0.remove(x);
	    if (hs0.isEmpty() != phs0.isEmpty()) {
		println("PureCachedHashSet less/isEmpty failed on iteration " + i);
		exit();
	    }
	}
	if (i % 50 == 0) {
	    try {
		// Check handling of null set
		PureSet phsser = (i == 0 ? phs0 : phs1);
		FileOutputStream fos = new FileOutputStream("phs.tmp");
		ObjectOutputStream oos = new ObjectOutputStream(fos);
		oos.writeObject(phsser);
		oos.close();
		FileInputStream fis = new FileInputStream("phs.tmp");
		ObjectInputStream ois = new ObjectInputStream(fis);
		PureSet nphsser = (PureSet)ois.readObject();
		ois.close();
		if (!phsser.equals(nphsser)) {
		    println("PureCachedHashSet read/write failed on iteration " + i);
		    exit();
		}
	    } catch (IOException e) {
		println("PureCachedHashSet read/write: exception " + e);
		exit();
	    } catch (ClassNotFoundException e) {
		println("PureCachedHashSet read/write: exception " + e);
	    }
	}
	return phs1;
    }
*/

    static void testPureTreeMap(Random rand, int i, PureTreeSet<MyInteger> set) {
	PureTreeMap<MyInteger, MyInteger> ptm0 =
	    new PureTreeMap<MyInteger, MyInteger>(TestComparator.Instance);
	TreeMap<MyInteger, MyInteger> tm0 = new TreeMap<MyInteger, MyInteger>();
	for (int j = 0; j < 100; ++j) {
	    int r = rand.nextInt(200), v = rand.nextInt(3);
	    MyInteger R = new MyInteger(r), V = new MyInteger(v);
	    PureTreeMap<MyInteger, MyInteger> tmp = ptm0.with(R, V);
	    tm0.put(R, V);
	    if (!tmp.verify()) {
		println("PureTreeMap Verification failure on iteration " + i);
		println(ptm0.dump());
		println("Adding " + R + ", " + V);
		println(tmp.dump());
		exit();
	    }
	    if (tmp.hashCode() != tm0.hashCode()) {
		println("PureTreeMap hashCode failed on ptm0 on iteration " + i);
		println(ptm0);
		println(ptm0.dump());
		println("Adding " + r + " -> " + v);
		println(tmp.dump());
		println(tm0);
		exit();
	    }
	    ptm0 = tmp;
	}
	PureTreeMap<MyInteger, MyInteger> ptm1 = new PureTreeMap<MyInteger, MyInteger>(TestComparator.Instance);
	TreeMap<MyInteger, MyInteger> tm1 = new TreeMap<MyInteger, MyInteger>();
	for (int j = 0; j < 100; ++j) {
	    int r = rand.nextInt(200), v = rand.nextInt(3);
	    MyInteger R = new MyInteger(r), V = new MyInteger(v);
	    PureTreeMap<MyInteger, MyInteger> tmp = ptm1.with(R, V);
	    tm1.put(R, V);
	    if (!tmp.verify()) {
		println("PureTreeMap Verification failure on iteration " + i);
		println(ptm1.dump());
		println("Adding " + R + ", " + V);
		println(tmp.dump());
		exit();
	    }
	    if (tmp.hashCode() != tm1.hashCode()) {
		println("PureTreeMap hashCode failed on ptm1 on iteration " + i);
		println(ptm1);
		println(ptm1.dump());
		println("Adding " + r + " -> " + v);
		println(tmp.dump());
		println(tm1);
		exit();
	    }
	    ptm1 = tmp;
	}
	for (int j = 0; j < 20; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = new MyInteger(r);
	    if (!equals(ptm0.get(R), tm0.get(R))) {
		println("PureTreeMap get (ptm0) failed on iteration " + i);
		exit();
	    }
	    PureTreeMap<MyInteger, MyInteger> tmp = ptm0.less(R);
	    tm0.remove(R);
	    if (!tmp.verify()) {
		println("PureTreeMap Verification failure on iteration " + i);
		println(ptm0.dump());
		println("Removing " + R);
		println(tmp.dump());
		exit();
	    }
	    if (tmp.hashCode() != tm0.hashCode()) {
		println("PureTreeMap hashCode failed on ptm0 on iteration " + i);
		println(ptm0);
		println(ptm0.dump());
		println("Removing " + r);
		println(tmp.dump());
		println(tm0);
		exit();
	    }
	    ptm0 = tmp;
	}
	for (int j = 0; j < 20; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = new MyInteger(r);
	    if (!equals(ptm1.get(R), tm1.get(R))) {
		println("PureTreeMap get failed (ptm1) on iteration " + i);
		exit();
	    }
	    PureTreeMap<MyInteger, MyInteger> tmp = ptm1.less(R);
	    tm1.remove(R);
	    if (!tmp.verify()) {
		println("PureTreeMap Verification failure on iteration " + i);
		println(ptm1.dump());
		println("Removing " + R);
		println(tmp.dump());
		exit();
	    }
	    if (tmp.hashCode() != tm1.hashCode()) {
		println("PureTreeMap hashCode failed on ptm1 on iteration " + i);
		println(ptm1);
		println(ptm1.dump());
		println("Removing " + r);
		println(tmp.dump());
		println(tm1);
		exit();
	    }
	    ptm1 = tmp;
	}
	if (i == 0) {
	    PureTreeMap<MyInteger, MyInteger> tmp = ptm0.with(null, null);
	    if (!tmp.verify() || !tmp.containsKey(null) || tmp.firstKey() != null) {
		println("PureTreeMap Verification failure on iteration " + i);
		println(ptm0.dump());
		println("Adding null");
		println(tmp.dump());
		exit();
	    }
	    tmp = tmp.less(null);
	    if (!tmp.verify() || tmp.containsKey(null)) {
		println("PureTreeMap Verification failure on iteration " + i);
		println(ptm0.dump());
		println("Removing null");
		println(tmp.dump());
		exit();
	    }
	}		
	if (ptm0.hashCode() != tm0.hashCode()) {
	    println("PureTreeMap hashCode failed on ptm0 on iteration " + i);
	    println(ptm0);
	    println(tm0);
	    exit();
	}
	if (ptm1.hashCode() != tm1.hashCode()) {
	    println("PureTreeMap hashCode failed on ptm1 on iteration " + i);
	    exit();
	}
	if (!ptm0.equals(tm0)) {
	    println("PureTreeMap Equality failed (ptm0, A) on iteration " + i);
	    println(ptm0.dump());
	    println(tm0);
	    exit();
	}
	if (!ptm0.equals(new PureTreeMap<MyInteger, MyInteger>(tm0))) {
	    println("PureTreeMap Equality failed (ptm0, B) on iteration " + i);
	    println(ptm0.dump());
	    println(tm0);
	    exit();
	}
	if (!ptm0.equals(new PureTreeMap<MyInteger, MyInteger>(tm0, TestComparator.Instance))) {
	    println("PureTreeMap Equality failed (ptm0, C) on iteration " + i);
	    println(ptm0.dump());
	    println(tm0);
	    exit();
	}
	if (!ptm1.equals(tm1)) {
	    println("PureTreeMap Equality failed (ptm1, A) on iteration " + i);
	    println(ptm1.dump());
	    println(tm1);
	    exit();
	}
	if (!ptm1.equals(new PureTreeMap<MyInteger, MyInteger>(tm1))) {
	    println("PureTreeMap Equality failed (ptm1, B) on iteration " + i);
	    println(ptm1.dump());
	    exit();
	}
	if (!ptm1.equals(new PureTreeMap<MyInteger, MyInteger>(tm1, TestComparator.Instance))) {
	    println("PureTreeMap Equality failed (ptm1, C) on iteration " + i);
	    println(ptm1.dump());
	    exit();
	}
	if (ptm0.firstKey().intValue() / 2 != tm0.firstKey().intValue() / 2) {
	    println("PureTreeMap `firstKey' failed (ptm0) on iteration " + i);
	    exit();
	}
	if (ptm1.firstKey().intValue() / 2 != tm1.firstKey().intValue() / 2) {
	    println("PureTreeMap `firstKey' failed (ptm1) on iteration " + i);
	    exit();
	}
	if (ptm0.lastKey().intValue() / 2 != tm0.lastKey().intValue() / 2) {
	    println("PureTreeMap `lastKey' failed (ptm0) on iteration " + i);
	    exit();
	}
	if (ptm1.lastKey().intValue() / 2 != tm1.lastKey().intValue() / 2) {
	    println("PureTreeMap `lastKey' failed (ptm1) on iteration " + i);
	    exit();
	}
	PureTreeMap<MyInteger, MyInteger> ptmm = ptm0.union(ptm1);
	TreeMap<MyInteger, MyInteger> tmm = (TreeMap<MyInteger, MyInteger>)tm0.clone();
	tmm.putAll(tm1);
	if (!ptmm.verify() || !ptmm.equals(tmm)) {
	    println("PureTreeMap Union failed on iteration " + i);
	    println(ptm0);
	    println(ptm0.dump());
	    println(ptm1);
	    println(ptm1.dump());
	    //if (!((PureTreeMap)ptmm).verify())
	    println(ptmm.size() + ", " + tmm.size());
	    println(ptmm);
	    println(ptmm.dump());
	    println(tmm);
	    exit();
	}
	if (!ptmm.equals(new PureTreeMap<MyInteger, MyInteger>(tmm))) {
	    println("PureTreeMap Equality failed (ptmm) on iteration " + i);
	    exit();
	}
	PureTreeMap<MyInteger, MyInteger> ptmr = ptm0.restrictedTo(set);
	TreeMap<MyInteger, MyInteger> tmr = (TreeMap<MyInteger, MyInteger>)tm0.clone();
	for (Iterator it = tmr.keySet().iterator(); it.hasNext(); ) {
	    Object k = it.next();
	    if (!set.contains(k)) it.remove();
	}
	if (!ptmr.verify() || !ptmr.equals(tmr)) {
	    println("PureTreeMap restrict failed on iteration " + i);
	    exit();
	}
	ptmr = ptm0.restrictedFrom(set);
	tmr = (TreeMap<MyInteger, MyInteger>)tm0.clone();
	for (Iterator it = tmr.keySet().iterator(); it.hasNext(); ) {
	    Object k = it.next();
	    if (set.contains(k)) it.remove();
	}
	if (!ptmr.verify() || !ptmr.equals(tmr)) {
	    println("PureTreeMap restrictNot failed on iteration " + i);
	    exit();
	}
	ptm0 = ptm0.less(null);		// for benefit of `compare' below
	PureSet<MyInteger> ptm0_dom = ptm0.domain();
	PureTreeMap<MyInteger, MyInteger> ptm0a =
	    ptm0.less(pick(rand, ptm0_dom)).with(pick(rand, ptm0_dom),
						 new MyInteger(rand.nextInt(3)));
	PureTreeMap<MyInteger, MyInteger> ptm0b =
	    ptm0.less(pick(rand, ptm0_dom)).with(pick(rand, ptm0_dom),
						 new MyInteger(rand.nextInt(3)));
	if (sgn(ptm0a.compareTo(ptm0b)) != compare(ptm0a, ptm0b)) {
	    println("PureTreeMap Compare failed (ptm0) on iteration " + i);
	    println(ptm0a.dump());
	    println(ptm0b.dump());
	    println(ptm0a);
	    println(ptm0b);
	    println(ptm0a.compareTo(ptm0b));
	    println(compare(ptm0a, ptm0b));
	    exit();
	}
	ptm1 = ptm1.less(null);
	PureSet<MyInteger> ptm1_dom = ptm1.domain();
	PureTreeMap<MyInteger, MyInteger> ptm1a =
	    ptm1.less(pick(rand, ptm1_dom)).with(pick(rand, ptm1_dom),
						 new MyInteger(rand.nextInt(3)));
	PureTreeMap<MyInteger, MyInteger> ptm1b =
	    ptm1.less(pick(rand, ptm1_dom)).with(pick(rand, ptm1_dom),
						 new MyInteger(rand.nextInt(3)));
	if (sgn(ptm1a.compareTo(ptm1b)) != compare(ptm1a, ptm1b)) {
	    println("PureTreeMap Compare failed (ptm1) on iteration " + i);
	    println(ptm1a.dump());
	    println(ptm1b.dump());
	    println(ptm1a);
	    println(ptm1b);
	    println(ptm1a.compareTo(ptm1b));
	    println(compare(ptm1a, ptm1b));
	    exit();
	}
	int lo = rand.nextInt(150) - 25;
	int hi = rand.nextInt(125 - lo) + lo;
	lo *= 2;	// they have to be even because of the comparator behavior
	hi *= 2;
	MyInteger Lo = new MyInteger(lo);
	MyInteger Hi = new MyInteger(hi);
	SortedMap<MyInteger, MyInteger> ptsm = ptm0.subMap(Lo, Hi);
	SortedMap<MyInteger, MyInteger> tsm = tm0.subMap(Lo, Hi);
	if (!ptsm.equals(tsm)) {
	    println("PureTreeMap subMap failed on iteration " + i);
	    println("[" + lo + ", " + hi + ")");
	    println(ptsm);
	    println(tsm);
	    exit();
	}
	if (!ptm0.headMap(Hi).equals(tm0.headMap(Hi))) {
	    println("PureTreeMap headMap failed on iteration " + i);
	    exit();
	}
	if (!ptm0.tailMap(Lo).equals(tm0.tailMap(Lo))) {
	    println("PureTreeMap tailMap failed on iteration " + i);
	    exit();
	}
	if (i % 50 == 0) {
	    try {
		// Check handling of null map
		PureMap<MyInteger, MyInteger> ptmser =
		    (i == 0 ? new PureTreeMap<MyInteger, MyInteger>(TestComparator.Instance)
		     : ptm0);
		FileOutputStream fos = new FileOutputStream("ptm.tmp");
		ObjectOutputStream oos = new ObjectOutputStream(fos);
		oos.writeObject(ptmser);
		oos.close();
		FileInputStream fis = new FileInputStream("ptm.tmp");
		ObjectInputStream ois = new ObjectInputStream(fis);
		PureMap<MyInteger, MyInteger> nptmser =
		    (PureMap<MyInteger, MyInteger>)ois.readObject();
		ois.close();
		if (!ptmser.equals(nptmser)) {
		    println("PureTreeMap read/write failed on iteration " + i);
		    exit();
		}
	    } catch (IOException e) {
		println("PureTreeMap read/write: exception " + e);
		exit();
	    } catch (ClassNotFoundException e) {
		println("PureTreeMap read/write: exception " + e);
	    }
	}
    }

    static void testPureHashMap(Random rand, int i, PureHashSet<MyInteger> set) {
	PureHashMap<MyInteger, MyInteger> phm0 = new PureHashMap<MyInteger, MyInteger>();
	HashMap<MyInteger, MyInteger> hm0 = new HashMap<MyInteger, MyInteger>();
	for (int j = 0; j < 100; ++j) {
	    int r = rand.nextInt(200), v = rand.nextInt(3);
	    MyInteger R = r == 57 ? null : new MyInteger(r), V = new MyInteger(v);
	    PureHashMap<MyInteger, MyInteger> tmp = phm0.with(R, V);
	    hm0.put(R, V);
	    if (!tmp.verify()) {
		println("PureHashMap Verification failure on iteration " + i);
		println(phm0.dump());
		println("Adding " + R + ", " + V);
		println(tmp.dump());
		exit();
	    }
	    phm0 = tmp;
	}
	PureHashMap<MyInteger, MyInteger> phm1 = new PureHashMap<MyInteger, MyInteger>();
	HashMap<MyInteger, MyInteger> hm1 = new HashMap<MyInteger, MyInteger>();
	for (int j = 0; j < 100; ++j) {
	    int r = rand.nextInt(200), v = rand.nextInt(3);
	    MyInteger R = r == 57 ? null : new MyInteger(r), V = new MyInteger(v);
	    PureHashMap<MyInteger, MyInteger> tmp = phm1.with(R, V);
	    hm1.put(R, V);
	    if (!tmp.verify()) {
		println("PureHashMap Verification failure on iteration " + i);
		println(phm1.dump());
		println("Adding " + R + ", " + V);
		println(tmp.dump());
		exit();
	    }
	    phm1 = tmp;
	}
	for (int j = 0; j < 20; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = r == 57 ? null : new MyInteger(r);
	    PureHashMap<MyInteger, MyInteger> tmp = phm0.less(R);
	    hm0.remove(R);
	    if (!tmp.verify()) {
		println("PureHashMap Verification failure on iteration " + i);
		println(phm0.dump());
		println("Removing " + R);
		println(tmp.dump());
		exit();
	    }
	    phm0 = tmp;
	}
	for (int j = 0; j < 20; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = r == 57 ? null : new MyInteger(r);
	    PureHashMap<MyInteger, MyInteger> tmp = phm1.less(R);
	    hm1.remove(R);
	    if (!tmp.verify()) {
		println("PureHashMap Verification failure on iteration " + i);
		println(phm1.dump());
		println("Removing " + R);
		println(tmp.dump());
		exit();
	    }
	    phm1 = tmp;
	}
	if (!phm0.equals(hm0)) {
	    println("PureHashMap Equality failed (phm0, A) on iteration " + i);
	    println(phm0.dump());
	    println(phm0);
	    println(hm0);
	    exit();
	}
	if (!phm0.equals(new PureHashMap<MyInteger, MyInteger>(hm0))) {
	    println("PureHashMap Equality failed (phm0, B) on iteration " + i);
	    println(phm0.dump());
	    println(hm0);
	    exit();
	}
	if (!phm1.equals(hm1)) {
	    println("PureHashMap Equality failed (phm1, A) on iteration " + i);
	    println(phm1.dump());
	    println(phm1);
	    println(hm1);
	    exit();
	}
	if (!phm1.equals(new PureHashMap<MyInteger, MyInteger>(hm1))) {
	    println("PureHashMap Equality failed (phm1, B) on iteration " + i);
	    println(phm1.dump());
	    exit();
	}
	PureHashMap<MyInteger, MyInteger> phmm = phm0.union(phm1);
	HashMap<MyInteger, MyInteger> hmm = (HashMap<MyInteger, MyInteger>)hm0.clone();
	hmm.putAll(hm1);
	if (!phmm.verify() || !phmm.equals(hmm)) {
	    println("PureHashMap Union failed on iteration " + i);
	    println(phm0);
	    println(phm0.dump());
	    println(phm1);
	    println(phm1.dump());
	    //if (!phmm.verify())
	    println(phmm.size() + ", " + hmm.size());
	    println(phmm);
	    println(phmm.dump());
	    println(hmm);
	    exit();
	}
	if (!phmm.equals(new PureHashMap<MyInteger, MyInteger>(hmm))) {
	    println("PureHashMap Equality failed (phmm) on iteration " + i);
	}
	PureHashMap<MyInteger, MyInteger> phmr = phm0.restrictedTo(set);
	HashMap<MyInteger, MyInteger> hmr = (HashMap<MyInteger, MyInteger>)hm0.clone();
	for (Iterator it = hmr.keySet().iterator(); it.hasNext(); ) {
	    Object k = it.next();
	    if (!set.contains(k)) it.remove();
	}
	if (!phmr.verify() || !phmr.equals(hmr)) {
	    println("PureHashMap restrictedTo failed on iteration " + i);
	    println(phmr);
	    println(hmr);
	    exit();
	}
	phmr = phm0.restrictedFrom(set);
	hmr = (HashMap<MyInteger, MyInteger>)hm0.clone();
	for (Iterator it = hmr.keySet().iterator(); it.hasNext(); ) {
	    Object k = it.next();
	    if (set.contains(k)) it.remove();
	}
	if (!phmr.verify() || !phmr.equals(hmr)) {
	    println("PureHashMap restrictedFrom failed on iteration " + i);
	    println(phmr);
	    println(hmr);
	    exit();
	}
	phm0 = phm0.less(null);		// for benefit of `compare' below
	PureSet<MyInteger> phm0_dom = phm0.domain();
	PureHashMap<MyInteger, MyInteger> phm0a =
	    phm0.less(pick(rand, phm0_dom)).with(pick(rand, phm0_dom),
						 new MyInteger(rand.nextInt(3)));
	PureHashMap<MyInteger, MyInteger> phm0b =
	    phm0.less(pick(rand, phm0_dom)).with(pick(rand, phm0_dom),
						 new MyInteger(rand.nextInt(3)));
	if (sgn(phm0a.compareTo(phm0b)) != compare(phm0a, phm0b)) {
	    println("PureHashMap Compare failed (phm0) on iteration " + i);
	    println(phm0a.dump());
	    println(phm0b.dump());
	    println(phm0a);
	    println(phm0b);
	    println(phm0a.compareTo(phm0b));
	    println(compare(phm0a, phm0b));
	    exit();
	}
	phm1 = phm1.less(null);
	PureSet<MyInteger> phm1_dom = phm1.domain();
	PureHashMap<MyInteger, MyInteger> phm1a =
	    phm1.less(pick(rand, phm1_dom)).with(pick(rand, phm1_dom),
						 new MyInteger(rand.nextInt(3)));
	PureHashMap<MyInteger, MyInteger> phm1b =
	    phm1.less(pick(rand, phm1_dom)).with(pick(rand, phm1_dom),
						 new MyInteger(rand.nextInt(3)));
	if (sgn(phm1a.compareTo(phm1b)) != compare(phm1a, phm1b)) {
	    println("PureHashMap Compare failed (phm1) on iteration " + i);
	    println(phm1a.dump());
	    println(phm1b.dump());
	    println(phm1a);
	    println(phm1b);
	    println(phm1a.compareTo(phm1b));
	    println(compare(phm1a, phm1b));
	    exit();
	}
	if (i % 50 == 0) {
	    try {
		// Check handling of null map
		PureMap<MyInteger, MyInteger> phmser =
		    (i == 0 ? new PureHashMap<MyInteger, MyInteger>() : phm0);
		FileOutputStream fos = new FileOutputStream("phm.tmp");
		ObjectOutputStream oos = new ObjectOutputStream(fos);
		oos.writeObject(phmser);
		oos.close();
		FileInputStream fis = new FileInputStream("phm.tmp");
		ObjectInputStream ois = new ObjectInputStream(fis);
		PureMap<MyInteger, MyInteger> nphmser = (PureMap<MyInteger, MyInteger>)ois.readObject();
		ois.close();
		if (!phmser.equals(nphmser)) {
		    println("PureHashMap read/write failed on iteration " + i);
		    exit();
		}
	    } catch (IOException e) {
		println("PureHashMap read/write: exception " + e);
		exit();
	    } catch (ClassNotFoundException e) {
		println("PureHashMap read/write: exception " + e);
	    }
	}
    }

/********
    static void testPureCachedHashMap(Random rand, int i, PureSet set) {
	PureMap phm0 = new PureCachedHashMap();
	HashMap hm0 = new HashMap();
	for (int j = 0; j < 100; ++j) {
	    int r = rand.nextInt(200), v = rand.nextInt(3);
	    MyInteger R = r == 57 ? null : new MyInteger(r), V = new MyInteger(v);
	    PureMap tmp = phm0.with(R, V);
	    hm0.put(R, V);
	    if (!((PureCachedHashMap)tmp).verify()) {
		println("PureCachedHashMap Verification failure on iteration " + i);
		println(((PureCachedHashMap)phm0).dump());
		println("Adding " + R + ", " + V);
		println(((PureCachedHashMap)tmp).dump());
		exit();
	    }
	    phm0 = tmp;
	}
	PureMap phm1 = new PureCachedHashMap();
	HashMap hm1 = new HashMap();
	for (int j = 0; j < 100; ++j) {
	    int r = rand.nextInt(200), v = rand.nextInt(3);
	    MyInteger R = r == 57 ? null : new MyInteger(r), V = new MyInteger(v);
	    PureMap tmp = phm1.with(R, V);
	    hm1.put(R, V);
	    if (!((PureCachedHashMap)tmp).verify()) {
		println("PureCachedHashMap Verification failure on iteration " + i);
		println(((PureCachedHashMap)phm1).dump());
		println("Adding " + R + ", " + V);
		println(((PureCachedHashMap)tmp).dump());
		exit();
	    }
	    phm1 = tmp;
	}
	for (int j = 0; j < 20; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = r == 57 ? null : new MyInteger(r);
	    PureMap tmp = phm0.less(R);
	    hm0.remove(R);
	    if (!((PureCachedHashMap)tmp).verify()) {
		println("PureCachedHashMap Verification failure on iteration " + i);
		println(((PureCachedHashMap)phm0).dump());
		println("Removing " + R);
		println(((PureCachedHashMap)tmp).dump());
		exit();
	    }
	    phm0 = tmp;
	}
	for (int j = 0; j < 20; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = r == 57 ? null : new MyInteger(r);
	    PureMap tmp = phm1.less(R);
	    hm1.remove(R);
	    if (!((PureCachedHashMap)tmp).verify()) {
		println("PureCachedHashMap Verification failure on iteration " + i);
		println(((PureCachedHashMap)phm1).dump());
		println("Removing " + R);
		println(((PureCachedHashMap)tmp).dump());
		exit();
	    }
	    phm1 = tmp;
	}
	if (!phm0.equals(hm0)) {
	    println("PureCachedHashMap Equality failed (phm0, A) on iteration " + i);
	    println(((PureCachedHashMap)phm0).dump());
	    println(phm0);
	    println(hm0);
	    exit();
	}
	if (!phm0.equals(new PureCachedHashMap(hm0))) {
	    println("PureCachedHashMap Equality failed (phm0, B) on iteration " + i);
	    println(((PureCachedHashMap)phm0).dump());
	    println(hm0);
	    exit();
	}
	if (!phm1.equals(hm1)) {
	    println("PureCachedHashMap Equality failed (phm1, A) on iteration " + i);
	    println(((PureCachedHashMap)phm1).dump());
	    println(hm1);
	    exit();
	}
	if (!phm1.equals(new PureCachedHashMap(hm1))) {
	    println("PureCachedHashMap Equality failed (phm1, B) on iteration " + i);
	    println(((PureCachedHashMap)phm1).dump());
	    exit();
	}
	PureMap phmm = phm0.union(phm1);
	HashMap hmm = (HashMap)hm0.clone();
	hmm.putAll(hm1);
	if (!((PureCachedHashMap)phmm).verify() || !phmm.equals(hmm)) {
	    println("PureCachedHashMap Union failed on iteration " + i);
	    println(phm0);
	    println(((PureCachedHashMap)phm0).dump());
	    println(phm1);
	    println(((PureCachedHashMap)phm1).dump());
	    //if (!((PureCachedHashMap)phmm).verify())
	    println(phmm.size() + ", " + hmm.size());
	    println(phmm);
	    println(((PureCachedHashMap)phmm).dump());
	    println(hmm);
	    exit();
	}
	if (!phmm.equals(new PureCachedHashMap(hmm))) {
	    println("PureCachedHashMap Equality failed (phmm) on iteration " + i);
	}
	PureMap phmr = phm0.restrict(set);
	HashMap hmr = (HashMap)hm0.clone();
	for (Iterator it = hmr.keySet().iterator(); it.hasNext(); ) {
	    Object k = it.next();
	    if (!set.contains(k)) it.remove();
	}
	if (!((PureCachedHashMap)phmr).verify() || !phmr.equals(hmr)) {
	    println("PureCachedHashMap restrict failed on iteration " + i);
	    println(phmr);
	    println(hmr);
	    exit();
	}
	phmr = phm0.restrictNot(set);
	hmr = (HashMap)hm0.clone();
	for (Iterator it = hmr.keySet().iterator(); it.hasNext(); ) {
	    Object k = it.next();
	    if (set.contains(k)) it.remove();
	}
	if (!((PureCachedHashMap)phmr).verify() || !phmr.equals(hmr)) {
	    println("PureCachedHashMap restrictNot failed on iteration " + i);
	    println(phmr);
	    println(hmr);
	    exit();
	}
	phm0 = phm0.less(null);		// for benefit of `compare' below
	PureSet phm0_dom = phm0.domain();
	PureMap phm0a = phm0.less(pick(rand, phm0_dom))
			.with(pick(rand, phm0_dom), new MyInteger(rand.nextInt(3)));
	PureMap phm0b = phm0.less(pick(rand, phm0_dom))
			.with(pick(rand, phm0_dom), new MyInteger(rand.nextInt(3)));
	if (sgn(((PureCachedHashMap)phm0a).compareTo(phm0b)) !=
	      compare(phm0a, phm0b)) {
	    println("PureHashMap Compare failed (phm0) on iteration " + i);
	    println(((PureHashMap)phm0a).dump());
	    println(((PureHashMap)phm0b).dump());
	    println(phm0a);
	    println(phm0b);
	    println(((PureHashMap)phm0a).compareTo(phm0b));
	    println(compare(phm0a, phm0b));
	    exit();
	}
	phm1 = phm1.less(null);
	PureSet phm1_dom = phm1.domain();
	PureMap phm1a = phm1.less(pick(rand, phm1_dom))
			.with(pick(rand, phm1_dom), new MyInteger(rand.nextInt(3)));
	PureMap phm1b = phm1.less(pick(rand, phm1_dom))
			.with(pick(rand, phm1_dom), new MyInteger(rand.nextInt(3)));
	if (sgn(((PureCachedHashMap)phm1a).compareTo(phm1b)) !=
	      compare(phm1a, phm1b)) {
	    println("PureHashMap Compare failed (phm1) on iteration " + i);
	    println(((PureHashMap)phm1a).dump());
	    println(((PureHashMap)phm1b).dump());
	    println(phm1a);
	    println(phm1b);
	    println(((PureHashMap)phm1a).compareTo(phm1b));
	    println(compare(phm1a, phm1b));
	    exit();
	}
	if (i % 50 == 0) {
	    try {
		// Check handling of null map
		PureMap phmser = (i == 0 ? new PureCachedHashMap() : phm0);
		FileOutputStream fos = new FileOutputStream("phm.tmp");
		ObjectOutputStream oos = new ObjectOutputStream(fos);
		oos.writeObject(phmser);
		oos.close();
		FileInputStream fis = new FileInputStream("phm.tmp");
		ObjectInputStream ois = new ObjectInputStream(fis);
		PureMap nphmser = (PureMap)ois.readObject();
		ois.close();
		if (!phmser.equals(nphmser)) {
		    println("PureCachedHashMap read/write failed on iteration " + i);
		    exit();
		}
	    } catch (IOException e) {
		println("PureCachedHashMap read/write: exception " + e);
		exit();
	    } catch (ClassNotFoundException e) {
		println("PureCachedHashMap read/write: exception " + e);
	    }
	}
    }
*/

    static void testPureTreeList(Random rand, int i) {
	PureTreeList<MyInteger> ptl0 = new PureTreeList<MyInteger>();
	// No `java.util' List class has both log-time-or-better indexing and
	// log-time-or-better insertion.  So we're going to have a quadraticism
	// somewhere.
	ArrayList<MyInteger> al0 = new ArrayList<MyInteger>();
	for (int j = 0; j < 100; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = r == 57 ? null : new MyInteger(r);
	    int pos = al0.isEmpty() ? 0 : rand.nextInt(al0.size());
	    int which = rand.nextInt(6);
	    PureTreeList<MyInteger> tmp;
	    if (ptl0.indexOf(R) != al0.indexOf(R)) {
		println("PureTreeList indexOf failed (ptl0) on iteration " + i);
		exit();
	    }
	    if (which == 0 && !al0.isEmpty()) {
		if (!equals(ptl0.get(pos), al0.get(pos))) {
		    println("PureTreeList get failed (ptl0) on iteration " + i);
		    exit();
		}
		tmp = ptl0.with(pos, R);
		al0.set(pos, R);
	    } else if (which == 1 && !al0.isEmpty()) {
		tmp = ptl0.less(pos);
		al0.remove(pos);
	    } else {
		tmp = ptl0.withInserted(pos, R);
		al0.add(pos, R);
	    }
	    if (!tmp.verify()) {
		println("PureTreeList Verification failed on iteration " + i);
		println(ptl0.dump());
		if (which == 1)
		    println("Deleting at " + pos);
		else
		    println((which == 0 ? "Writing " : "Inserting ") + R + " at " + pos);
		println(tmp.dump());
		exit();
	    }
	    if (tmp.hashCode() != al0.hashCode()) {
		println("PureTreeList hashCode failed on iteration " + i);
		println(ptl0);
		println(ptl0.dump());
		if (which == 1)
		    println("Deleting at " + pos);
		else
		    println((which == 0 ? "Writing " : "Inserting ") + R + " at " + pos);
		println(tmp);
		println(tmp.dump());
		println(al0);
		println(tmp.hashCode() + ", " + al0.hashCode());
		exit();
	    }
	    ptl0 = tmp;
	}
	PureTreeList<MyInteger> ptl1 = new PureTreeList<MyInteger>();
	ArrayList<MyInteger> al1 = new ArrayList<MyInteger>();
	for (int j = 0; j < 100; ++j) {
	    int r = rand.nextInt(200);
	    MyInteger R = r == 57 ? null : new MyInteger(r);
	    int pos = al1.isEmpty() ? 0 : rand.nextInt(al1.size());
	    int which = rand.nextInt(5);
	    PureTreeList<MyInteger> tmp;
	    if (ptl1.lastIndexOf(R) != al1.lastIndexOf(R)) {
		println("PureTreeList lastIndexOf failed (ptl1) on iteration " + i);
		exit();
	    }
	    if (which == 0 && !al1.isEmpty()) {
		if (!equals(ptl1.get(pos), al1.get(pos))) {
		    println("PureTreeList get failed (ptl1) on iteration " + i);
		    exit();
		}
		tmp = ptl1.with(pos, R);
		al1.set(pos, R);
	    } else if (which == 1 && !al1.isEmpty()) {
		tmp = ptl1.less(pos);
		al1.remove(pos);
	    } else {
		tmp = ptl1.withInserted(pos, R);
		al1.add(pos, R);
	    }
	    if (!tmp.verify()) {
		println("PureTreeList Verification failed on iteration " + i);
		println(ptl1.dump());
		if (which == 1)
		    println("Deleting at " + pos);
		else
		    println((which == 0 ? "Writing " : "Inserting ") + R + " at " + pos);
		println(tmp.dump());
		exit();
	    }
	    if (tmp.hashCode() != al1.hashCode()) {
		println("PureTreeList hashCode failed on iteration " + i);
		println(tmp);
		println(tmp.hashCode() + ", " + al1.hashCode());
		exit();
	    }
	    ptl1 = tmp;
	}
	if (!ptl0.equals(al0)) {
	    println("PureTreeList Equality failed (ptl0, A) on iteration " + i);
	    println(ptl0.dump());
	    println(ptl0);
	    println(al0);
	    exit();
	}
	if (!ptl0.equals(new PureTreeList<MyInteger>(al0))) {
	    println("PureTreeList Equality failed (ptl0, B) on iteration " + i);
	    println(ptl0.dump());
	    println(al0);
	    exit();
	}
	if (!ptl1.equals(al1)) {
	    println("PureTreeList Equality failed (ptl1, A) on iteration " + i);
	    println(ptl1.dump());
	    println(al1);
	    exit();
	}
	if (!ptl1.equals(new PureTreeList<MyInteger>(al1))) {
	    println("PureTreeList Equality failed (ptl1, B) on iteration " + i);
	    println(ptl1.dump());
	    exit();
	}
	PureTreeList<MyInteger> ptlc = ptl0.concat(ptl1);
	ArrayList<MyInteger> alc = (ArrayList<MyInteger>)al0.clone();
	alc.addAll(alc.size(), al1);
	if (!ptlc.equals(alc)) {
	    println("PureTreeList concat failed on iteration " + i);
	    exit();
	}
	int lo = rand.nextInt(al0.size()), hi = rand.nextInt(al0.size() - lo) + lo;
	PureTreeList<MyInteger> sptl0 = ptl0.subseq(lo, hi);
	List<MyInteger> sal0 = al0.subList(lo, hi);
	if (!sptl0.equals(sal0)) {
	    println("PureTreeList subseq failed on iteration " + i);
	    println(ptl0);
	    println(ptl0.dump());
	    println(lo + ", " + hi);
	    println(sptl0);
	    println(sal0);
	    exit();
	}
	int delpos = rand.nextInt(ptl0.size());
	PureTreeList<MyInteger> ptl0a = ptl0.less(delpos);
	List<MyInteger> al0a = (List<MyInteger>)al0.clone();
	al0a.remove(delpos);
	if (!ptl0a.equals(al0a)) {
	    println("PureTreeList less failed on iteration " + i);
	    println(ptl0);
	    println(ptl0.dump());
	    println("Removing at " + delpos);
	    println(ptl0a);
	    println(ptl0a.dump());
	    exit();
	}
	PureTreeList<MyInteger> ptl0b = ptl0.less(rand.nextInt(ptl0.size()));
	if (sgn(ptl0a.compareTo(ptl0b)) !=
	    compare(ptl0a, ptl0b)) {
	    println("PureTreeList compareTo failed on iteration " + i);
	    println(ptl0a);
	    println(ptl0b);
	    println(ptl0a.compareTo(ptl0b));
	    println(compare(ptl0a, ptl0b));
	    exit();
	}
	PureTreeList<MyInteger> ptl0s = ptl0.sort(TestComparator.Instance);
	ArrayList<MyInteger> al0s = (ArrayList<MyInteger>)al0.clone();
	Collections.sort(al0s, TestComparator.Instance);
	if (!ptl0s.equals(al0s)) {
	    println("PureTreeList sort failed on iteration " + i);
	    println(ptl0s);
	    println(al0s);
	    exit();
	}
	ListIterator<MyInteger> ptli = ptl0.listIterator();
	ListIterator<MyInteger> ali = al0.listIterator();
	//PureTreeList.debug = true;
	for (int j = 0; j < 400; ++j) {
	    int which = rand.nextInt(2);
	    if (ptli.nextIndex() != ali.nextIndex()) {
		println("PureTreeList nextIndex failed on iteration "+ i + "." + j);
		exit();
	    }
	    if (which == 0 && ali.hasPrevious()) {
		//println("Rev");
		if (!ptli.hasPrevious()) {
		    println("PureTreeList hasPrevious failed false on iteration " +
			    i + "." + j);
		    exit();
		}
		MyInteger x = ptli.previous();
		MyInteger y = ali.previous();
		if (x == null ? y != null : !x.equals(y)) {
		    println("PureTreeList previous failed on iteration " + i + "." + j);
		    exit();
		}
	    } else if (ali.hasNext()) {
		//println("Fwd");
		if (!ali.hasPrevious() && ptli.hasPrevious()) {
		    println("PureTreeList hasPrevious failed true on iteration " +
			    i + "." + j);
		    exit();
		}
		if (!ptli.hasNext()) {
		    println("PureTreeList hasNext failed false on iteration " +
			    i + "." + j);
		    exit();
		}
		MyInteger x = ptli.next();
		MyInteger y = ali.next();
		if (x == null ? y != null : !x.equals(y)) {
		    println("PureTreeList next failed on iteration " + i + "." + j);
		    exit();
		}
	    } else if (ptli.hasNext()) {
		println("PureTreeList hasNext failed true on iteration " + i + "." + j);
		exit();
	    }
	}
	if (i % 50 == 0) {
	    try {
		// Check handling of null list
		PureList<MyInteger> ptlser = (i == 0 ? new PureTreeList<MyInteger>() : ptl0);
		FileOutputStream fos = new FileOutputStream("ptl.tmp");
		ObjectOutputStream oos = new ObjectOutputStream(fos);
		oos.writeObject(ptlser);
		oos.close();
		FileInputStream fis = new FileInputStream("ptl.tmp");
		ObjectInputStream ois = new ObjectInputStream(fis);
		PureList<MyInteger> nptlser = (PureList<MyInteger>)ois.readObject();
		ois.close();
		if (!ptlser.equals(nptlser)) {
		    println("PureTreeList read/write failed on iteration " + i);
		    exit();
		}
	    } catch (IOException e) {
		println("PureTreeList read/write: exception " + e);
		exit();
	    } catch (ClassNotFoundException e) {
		println("PureTreeList read/write: exception " + e);
	    }
	}
    }

    static class MyInteger implements Comparable<MyInteger>, Serializable {
	MyInteger(int val) { value = val; }
	private int value;
	public int intValue() { return value; }
	public boolean equals(Object x) {
	    if (x == this) return true;
	    else if (!(x instanceof MyInteger)) return false;
	    else return value == ((MyInteger)x).value;
	}
	public int hashCode() { return value >> 1; }
	public int compareTo(MyInteger x) {
	    return value < x.value ? -1 : value > x.value ? 1 : 0;
	}
	public String toString() { return "" + value; }
    }

    // For convenience of testing, we use integers; to make it easy to test
    // handling of equivalent values, we use a comparator that divides by 2
    // before comparing.
    static class TestComparator implements Comparator<MyInteger>, Serializable {
	private TestComparator() { }
	public static final TestComparator Instance = new TestComparator();
	public int compare(MyInteger a, MyInteger b) {
	    int ia = a == null ? 0 : a.intValue();
	    int ib = b == null ? 0 : b.intValue();
	    return (ia / 2) - (ib / 2);
	}
    }

    static MyInteger[] conv(int[] ary) {
	MyInteger[] res = new MyInteger[ary.length];
	for (int i = 0; i < ary.length; ++i) res[i] = new MyInteger(ary[i]);
	return res;
    }

    static MyInteger pick(Random rand, PureSet<MyInteger> s) {
	if (s.isEmpty()) throw new IllegalStateException();
	while (true) {
	    int r = rand.nextInt(200);
	    MyInteger R = new MyInteger(r);
	    if (s.contains(R)) return R;
	}
    }

    static int compare(PureSet<MyInteger> a, PureSet<MyInteger> b) {
	if (a.size() < b.size()) return -1;
	else if (a.size() > b.size()) return 1;
	else {
	    Iterator<MyInteger> bi = b.iterator();
	    for (Iterator<MyInteger> ai = a.iterator(); ai.hasNext(); ) {
		if (!bi.hasNext()) {
		    println("Set iterator disagreement");
		    exit();
		}
		MyInteger a_elt = ai.next();
		MyInteger b_elt = bi.next();
		// 0 is the right value to use for null in `PureHashSet', but for
		// `PureTreeSet' the right value would be -1.  However, we don't
		// leave null in `PureTreeSet's very long, because `TreeSet' doesn't
		// accept it.
		int a_val = a_elt == null ? 0 : a_elt.intValue();
		int b_val = b_elt == null ? 0 : b_elt.intValue();
		int a_tmp = a_val / 2;
		int b_tmp = b_val / 2;
		if (a_tmp < b_tmp) return -1;
		else if (a_tmp > b_tmp) return 1;
	    }
	    if (bi.hasNext()) {
		println("Set iterator disagreement");
		exit();
	    }
	    return 0;
	}
    }

    static boolean equals(PureSet<MyInteger> a, PureSet<MyInteger> b) {
	if (a.size() != b.size()) return false;
	else {
	    for (Iterator ai = a.iterator(); ai.hasNext(); )
		if (!b.contains(ai.next())) return false;
	    return true;
	}
    }

    static int compare(PureList<MyInteger> a, PureList<MyInteger> b) {
	if (a.size() < b.size()) return -1;
	else if (a.size() > b.size()) return 1;
	else {
	    Iterator<MyInteger> bi = b.iterator();
	    for (Iterator<MyInteger> ai = a.iterator(); ai.hasNext(); ) {
		MyInteger a_elt = ai.next();
		MyInteger b_elt = bi.next();
		int a_val = a_elt == null ? -1 : a_elt.intValue();
		int b_val = b_elt == null ? -1 : b_elt.intValue();
		if (a_val < b_val) return -1;
		else if (a_val > b_val) return 1;
	    }
	    return 0;
	}
    }

    static int compare(PureMap<MyInteger, MyInteger> a, PureMap<MyInteger, MyInteger> b) {
	if (a.size() < b.size()) return -1;
	else if (a.size() > b.size()) return 1;
	else {
	    Iterator<Map.Entry<MyInteger, MyInteger>> ai = a.iterator();
	    Iterator<Map.Entry<MyInteger, MyInteger>> bi = b.iterator();
	    Map.Entry<MyInteger, MyInteger> a_prev = null, b_prev = null;
	    while (ai.hasNext() || a_prev != null) {
		Map.Entry<MyInteger, MyInteger> a_ent =
		    a_prev != null ? a_prev : (Map.Entry<MyInteger, MyInteger>)ai.next();
		Map.Entry<MyInteger, MyInteger> b_ent =
		    b_prev != null ? b_prev : (Map.Entry<MyInteger, MyInteger>)bi.next();
		a_prev = b_prev = null;
		int aki = a_ent.getKey().intValue();
		int bki = b_ent.getKey().intValue();
		Map.Entry<MyInteger, MyInteger> a_next = null, b_next = null;
		if ((aki >> 1) < (bki >> 1)) return -1;
		else if ((aki >> 1) > (bki >> 1)) return 1;
		else {
		    if (ai.hasNext()) a_next = (Map.Entry)ai.next();
		    if (bi.hasNext()) b_next = (Map.Entry)bi.next();
		    if (a_next != null &&
			(((MyInteger)a_next.getKey()).intValue() >> 1) > (aki >> 1)) {
			a_prev = a_next;
			a_next = null;
		    }
		    if (b_next != null &&
			(((MyInteger)b_next.getKey()).intValue() >> 1) > (bki >> 1)) {
			b_prev = b_next;
			b_next = null;
		    }
		    if (a_next != null && b_next == null) return -1;
		    else if (a_next == null && b_next != null) return 1;
		    else if (a_next == null && b_next == null) {
			int av = a_ent.getValue().intValue();
			int bv = b_ent.getValue().intValue();
			if (av < bv) return -1;
			else if (av > bv) return 1;
		    } else if (a instanceof PureTreeMap) {
			// We have to use the same kind of set as 'a' and 'b' (which are
			// assumed to be the same as each other), because they use different
			// value comparators.
			PureTreeSet<MyInteger> avs = new PureTreeSet<MyInteger>();
			avs = avs.with(a_ent.getValue()).with(a_next.getValue());
			PureTreeSet<MyInteger> bvs = new PureTreeSet<MyInteger>();
			bvs = bvs.with(b_ent.getValue()).with(b_next.getValue());
			int comp_res = avs.compareTo(bvs);
			if (comp_res != 0) return comp_res;
		    } else {
			PureHashSet<MyInteger> avs = new PureHashSet<MyInteger>();
			avs = avs.with(a_ent.getValue()).with(a_next.getValue());
			PureHashSet<MyInteger> bvs = new PureHashSet<MyInteger>();
			bvs = bvs.with(b_ent.getValue()).with(b_next.getValue());
			int comp_res = avs.compareTo(bvs);
			if (comp_res != 0) return comp_res;
		    }
		}
	    }
	    return 0;
	}
    }

    static boolean equals(Object x, Object y) {
	return x == null ? y == null : x.equals(y);
    }

    static int sgn(int n) { return n < 0 ? -1 : n > 0 ? 1 : 0; }

    static void println(String str) { System.out.println(str); }
    static void println(Object obj) { System.out.println(obj); }
    static void println(int i) { System.out.println(i); }
    static void println(boolean b) { System.out.println(b); }

    static void exit() { System.exit(1); }

}
