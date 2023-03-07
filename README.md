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
"org.bitlap" %% "bitlapx-json" % <version>
```

## Usage 1: by magnolia1
- [x] Scala types to Json asts
- [x] Json asts to Scala types
- [x] Json asts to string
- [x] Json asts to pretty string
- [x] Support product types
- [x] Support `@jsonField` annotation
- [x] Support `@jsonExclude` annotation
- [x] Support sum types (sealed)

```scala
    // derives JsonCodec -> AutoDerivation
    final case class Test1(d: Double, s: String, b: Boolean, l: Set[Int]) derives JsonCodec

    val obj  = Test1(1, "s", true, Set(1, 2, 3))
    val json = JsonCodec[Test1].toJson(obj)
    val back = JsonCodec[Test1].fromJson(json)

    // DeriveJsonCodec
    final case class Test1(d: Double, s: String, b: Boolean, l: Set[Test2])
    final case class Test2(abc: String)

    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

    val obj1 = Test1(1, "s", true, Set(Test2("abc")))
    val json = JsonCodec[Test1].toJson(obj1)
    val back = JsonCodec[Test1].fromJson(json)
```

## Usage 2: by pure Scala3
- [x] Scala types to Json asts `bitlapx.json.original.JsonEncoder.derived[A]`
- [x] Json asts to Scala types `bitlapx.json.original.JsonDecoder.derived[A]`
- [x] Json asts to string
- [x] Json asts to pretty string
- [x] Support product types
- [x] Support `@jsonField` annotation
- [x] Support `@jsonExclude` annotation
- [x] Support sum types (sealed)

```scala
    sealed trait Test0
    final case class Test1(d: Double, s: String, b: Boolean, l: List[Int]) extends Test0
    val encoder = JsonEncoder.derived[Test0]
    val decoder = JsonDecoder.derived[Test0]

    val obj1  = Test1(1, "s", true, List(1, 2, 3))
    val json1 = encoder.encode(obj1)
    val back1 = decoder.decode(json1)

    println(json1.asJsonString)
    json1.asJsonString shouldEqual "{\"Test1\": {\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": [1, 2, 3]}}"
```