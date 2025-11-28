package sjsonnet

import utest._
import TestUtils.{eval, evalErr}

/**
 * Tests for proper boolean validation in conditions.
 *
 * These test cases verify that sjsonnet properly rejects non-boolean values in contexts that
 * require booleans, matching the behavior of official Jsonnet.
 *
 * See: https://github.com/google/jsonnet/blob/master/doc/ref/spec.html
 */
object BooleanConditionTests extends TestSuite {

  def tests: Tests = Tests {

    test("std.filter rejects non-boolean predicate") {
      // Official Jsonnet: "filter function must return boolean, got: number"
      // Previously sjsonnet silently returned [] for non-boolean predicates
      assert(
        evalErr("std.filter(function(x) x, [1, 2, 3])")
          .contains("must return boolean") ||
        evalErr("std.filter(function(x) x, [1, 2, 3])")
          .contains("must be boolean")
      )

      assert(
        evalErr("""std.filter(function(x) "yes", [1, 2, 3])""")
          .contains("must return boolean") ||
        evalErr("""std.filter(function(x) "yes", [1, 2, 3])""")
          .contains("must be boolean")
      )

      assert(
        evalErr("std.filter(function(x) null, [1, 2, 3])")
          .contains("must return boolean") ||
        evalErr("std.filter(function(x) null, [1, 2, 3])")
          .contains("must be boolean")
      )
    }

    test("std.filterMap rejects non-boolean filter function") {
      // Official Jsonnet: "filter function must return boolean, got: number"
      // Previously sjsonnet silently returned [] for non-boolean filter results
      assert(
        evalErr("std.filterMap(function(x) x, function(x) x * 2, [1, 2, 3])")
          .contains("must return boolean") ||
        evalErr("std.filterMap(function(x) x, function(x) x * 2, [1, 2, 3])")
          .contains("must be boolean")
      )
    }

    test("assert rejects non-boolean condition") {
      // Official Jsonnet: "condition must be boolean, got number"
      // Previously sjsonnet reported "Assertion failed" instead of type error
      val err = evalErr("assert 1; true")
      assert(
        err.contains("must be boolean") ||
        err.contains("condition") && err.contains("boolean")
      )
      // Should NOT just say "Assertion failed" for non-boolean
      assert(!err.contains("Assertion failed") || err.contains("boolean"))
    }

    test("assert in object rejects non-boolean condition") {
      // Same issue in object assertions
      val err = evalErr("{ assert 1, x: 1 }.x")
      assert(
        err.contains("must be boolean") ||
        err.contains("condition") && err.contains("boolean")
      )
    }

    test("valid boolean conditions still work") {
      // Make sure we don't break valid cases
      eval("std.filter(function(x) x > 1, [1, 2, 3])") ==> ujson.Arr(2, 3)
      eval("std.filter(function(x) true, [1, 2, 3])") ==> ujson.Arr(1, 2, 3)
      eval("std.filter(function(x) false, [1, 2, 3])") ==> ujson.Arr()
      eval("std.filterMap(function(x) x > 1, function(x) x * 2, [1, 2, 3])") ==> ujson.Arr(4, 6)
      eval("assert true; 42") ==> ujson.Num(42)
      eval("{ assert true, x: 1 }.x") ==> ujson.Num(1)
    }
  }
}
