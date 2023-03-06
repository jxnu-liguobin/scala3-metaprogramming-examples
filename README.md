# bitlapx

![CI][Badge-CI]  [![codecov][Badge-Codecov]][Link-Codecov]   [![Nexus (Snapshots)][Badge-Snapshots]][Link-Snapshots] 

[Badge-CI]: https://github.com/bitlap/bitlapx/actions/workflows/ScalaCI.yml/badge.svg
[Badge-Codecov]: https://codecov.io/gh/bitlap/bitlapx/branch/master/graph/badge.svg?token=IA596YRTOT
[Badge-Snapshots]: https://img.shields.io/nexus/s/org.bitlap/bitlapx-json_3?server=https%3A%2F%2Fs01.oss.sonatype.org

[Link-Codecov]: https://codecov.io/gh/bitlap/smt
[Link-Snapshots]: https://s01.oss.sonatype.org/content/repositories/snapshots/org/bitlap/bitlapx

----

Toy box for learning scala3 metaprogramming


# bitlapx-json

simple json tool.

```scala
"org.bitlap" %% "bitlapx-json" % version
```

## Usage 1: derives JsonCodec
- [x] scala types to json asts
- [x] json asts to scala types
- [x] json asts to string
- [x] json asts to pretty string
- [x] Support product types
- [x] Support `@jsonField` annotation
- [x] Support `@jsonExclude` annotation
- [x] Support sum types

Usage 2
```scala
    final case class Test1(d: Double, s: String, b: Boolean, l: Set[Int]) derives JsonCodec

    val obj  = Test1(1, "s", true, Set(1, 2, 3))
    val json = JsonCodec[Test1].toJson(obj)
    val back = JsonCodec[Test1].fromJson(json)
```

## Usage 2: DeriveJsonCodec.gen
- [x] scala types to json asts
- [x] json asts to scala types
- [x] json asts to string
- [x] json asts to pretty string
- [x] Support product types
- [x] Support `@jsonField` annotation
- [x] Support `@jsonExclude` annotation
- [x] Support sum types

```scala
    final case class Test1(d: Double, s: String, b: Boolean, l: Set[Test2])
    final case class Test2(abc: String)

    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

    val obj1 = Test1(1, "s", true, Set(Test2("abc")))
    val json = JsonCodec[Test1].toJson(obj1)
    val back = JsonCodec[Test1].fromJson(json)
```