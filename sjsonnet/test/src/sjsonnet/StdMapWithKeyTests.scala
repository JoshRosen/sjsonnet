package sjsonnet

import utest._
import TestUtils.eval

object StdMapWithKeyTests extends TestSuite {

  // These tests verify that std.mapWithKey matches the behavior of google/go-jsonnet.
  // Per the official jsonnet stdlib, mapWithKey is defined as:
  //   mapWithKey(func, obj):: { [k]: func(k, obj[k]) for k in std.objectFields(obj) }
  // This means it should only iterate over visible fields (std.objectFields excludes hidden fields).

  def tests: Tests = Tests {

    test("basic functionality") {
      // Basic test: mapWithKey should transform values
      eval("std.mapWithKey(function(k, v) v * 10, {a: 1, b: 2})") ==>
        ujson.Obj("a" -> 10, "b" -> 20)

      // Key is passed to the function
      eval("""std.mapWithKey(function(k, v) k + "=" + v, {x: 1, y: 2})""") ==>
        ujson.Obj("x" -> "x=1", "y" -> "y=2")
    }

    test("hidden fields are excluded") {
      // Hidden fields should not be processed by mapWithKey
      // This matches the behavior of std.objectFields which excludes hidden fields
      eval("std.objectFields(std.mapWithKey(function(k, v) v, {visible: 1, hidden:: 2}))") ==>
        ujson.Arr("visible")

      eval("std.objectFieldsAll(std.mapWithKey(function(k, v) v, {visible: 1, hidden:: 2}))") ==>
        ujson.Arr("visible")

      // The hidden field should not appear in the result at all
      eval("std.mapWithKey(function(k, v) v * 10, {visible: 1, hidden:: 2})") ==>
        ujson.Obj("visible" -> 10)
    }

    test("multiple hidden and visible fields") {
      eval("std.objectFields(std.mapWithKey(function(k, v) v, {a: 1, b: 2, c:: 3, d:: 4}))") ==>
        ujson.Arr("a", "b")

      eval("std.mapWithKey(function(k, v) v, {a: 1, b: 2, c:: 3, d:: 4})") ==>
        ujson.Obj("a" -> 1, "b" -> 2)
    }

    test("only hidden fields results in empty object") {
      // If the input object has only hidden fields, the result should be an empty object
      eval("std.mapWithKey(function(k, v) v, {h1:: 1, h2:: 2})") ==> ujson.Obj()

      eval("std.objectFields(std.mapWithKey(function(k, v) v, {h1:: 1, h2:: 2}))") ==>
        ujson.Arr()

      eval("std.objectFieldsAll(std.mapWithKey(function(k, v) v, {h1:: 1, h2:: 2}))") ==>
        ujson.Arr()
    }

    test("force-visible fields are preserved") {
      // Fields declared with ::: (force visible) should be included
      eval("std.objectFields(std.mapWithKey(function(k, v) v, {normal: 1, hidden:: 2, forced::: 3}))") ==>
        ujson.Arr("forced", "normal")

      eval("std.mapWithKey(function(k, v) v * 10, {normal: 1, hidden:: 2, forced::: 3})") ==>
        ujson.Obj("forced" -> 30, "normal" -> 10)
    }

    test("nested objects preserve their hidden fields") {
      // mapWithKey should not affect visibility of fields in nested objects
      // The nested object is just a value being transformed
      eval("std.objectFieldsAll(std.mapWithKey(function(k, v) v, {outer: {inner: 1, innerHidden:: 2}}).outer)") ==>
        ujson.Arr("inner", "innerHidden")

      eval("std.objectFields(std.mapWithKey(function(k, v) v, {outer: {inner: 1, innerHidden:: 2}}).outer)") ==>
        ujson.Arr("inner")
    }

    test("empty object") {
      eval("std.mapWithKey(function(k, v) v, {})") ==> ujson.Obj()
    }
  }
}
