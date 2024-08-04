/*
   Copyright 2024 Branden J Brown

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

module hmap

import std/num/int64

inline fun replace(v: vector<a>, i: int, a: a): vector<a>
  // TODO(zeph): I learned after writing this that v[i] := a is a thing that
  // exists, but it has tricky effects.
  vector-init-total(v.length) fn(k)
    if k == i then a else match v.at(k)
      Just(x) -> x
      Nothing -> a // unreachable, but we don't have another a in general

alias bucket<k, v> = list<(k, v)>

fun extract(bu: bucket<k, v>, key: k, ?eq: (k, k) -> e bool): e (maybe<(k, v)>, bucket<k, v>)
  fun help(l)
    match l
      Nil -> (Nothing, Nil)
      Cons(h, t) ->
        if eq(h.tuple2/fst, key)
        then (Just(h), t)
        else
          val (r, x) = help(t)
          (r, Cons(h, x))
  help(bu)

// Partition elements zipped with their hash positions
// fun partition-buckets(l: list<(u, int64)>)

// TODO(zeph): figure out optimal load factor
val load-factor = 6

// Hashmap, otherwise known as a map, dict, or associative array.
pub value struct hmap<k, v>
  buckets: vector<bucket<k, v>>
  len: int

fun grow(hm: hmap<k, v>, ?hash: k -> int64): hmap<k, v>
  val n = int64(hm.buckets.length * 2)
  val l = hm.buckets.list.flatmap fn(bu)
    val m = bu.map(o(hash, fst)).map(fn(x) x % n)
    bu.zip(m)
  val v = vector-init-total(n.int) fn(ii: int)
    val i = ii.int64
    l.filter-map fn((u, h))
      if h == i then Just(u) else Nothing
  Hmap(v, hm.len)

inline fun should-grow(bucket-count: int, entries: int): bool
  entries >= bucket-count*load-factor

inline fun bui-hash(hm: hmap<k, v>, x: int64): int
  int(x % hm.buckets.length.int64)

inline fun bui(hm: hmap<k, v>, key: k, ?hash: k -> int64): int
  hm.bui-hash(hash(key))

// Replace an element or add it if it's missing.
pub fun update(hm: hmap<k, v>, key: k, el: v, ?hash: k -> int64, ?eq: (k, k) -> e bool): e hmap<k, v>
  val h = if should-grow(hm.buckets.length, hm.len) then hm.grow else hm
  val i = h.bui(key)
  val r = h.buckets.at(i).default([])
  val (e, l) = r.extract(key)
  val n = if e.is-just then h.len else h.len + 1
  val u = Cons((key, el), l)
  val x = h.buckets.replace(i, u)
  Hmap(x, n)

// Add an element if it's missing.
pub fun add(hm: hmap<k, v>, key: k, el: v, ?hash: k -> int64, ?eq: (k, k) -> e bool): e hmap<k, v>
  val h = if should-grow(hm.buckets.length, hm.len) then hm.grow else hm
  val i = h.bui(key)
  val r = h.buckets.at(i).default([])
  val (e, l) = r.extract(key)
  if e.is-just then return h
  val n = if e.is-just then h.len else h.len + 1
  val u = Cons((key, el), l)
  val x = h.buckets.replace(i, u)
  Hmap(x, n)

// Get the value associated with a key if it's in the map.
pub fun get(hm: hmap<k, v>, key: k, ?hash: k -> int64, ?eq: (k, k) -> e bool): e maybe<v>
  val i = hm.bui(key)
  val r = hm.buckets.at(i).default([])
  lookup(r) fn(u) eq(u, key)

// Iterate over all entries in the map in unspecified order.
pub fun foreach(hm: hmap<k, v>, action: (k, v) -> e ()): e ()
  with bu <- hm.buckets.foreach
  with keyel <- bu.foreach
  action(keyel.fst, keyel.snd)

// Create an empty hashmap with sufficient space to add at least n items before growth.
pub fun unit/hmap(n: int = load-factor): hmap<k, v>
  val b = max((n + load-factor - 1)/load-factor, 0)
  Hmap(vector(b, []), 0)

// Convert a list of pairs to a hashmap.
// Repeated keys use the last value.
pub fun list/hmap(xs: list<(k, v)>, ?hash: k -> int64, ?eq: (k, k) -> e bool): e hmap<k, v>
  val n = xs.length
  val b = (n + load-factor - 1) / load-factor
  val l = xs.map fn(s) (s, hash(s.fst) % b.int64)
  // TODO(zeph): remove duplicates
  val bu = vector-init-total(b) fn(ii: int)
    val i = ii.int64
    l.filter-map fn((u, h))
      if h == i then Just(u) else Nothing
  Hmap(bu, n)
