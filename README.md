# Scala3 Metaprogramming Examples

![CI][Badge-CI]  [![codecov][Badge-Codecov]][Link-Codecov] 

[Badge-CI]: https://github.com/jxnu-liguobin/scala3-metaprogramming-examples/actions/workflows/ScalaCI.yml/badge.svg
[Badge-Codecov]: https://codecov.io/gh/jxnu-liguobin/scala3-metaprogramming-examples/branch/master/graph/badge.svg?token=IA596YRTOT

[Link-Codecov]: https://codecov.io/gh/jxnu-liguobin/scala3-metaprogramming-examples

----

Toy box for learning scala3 metaprogramming


# bitlapx-json

simple json tool.

## Usage 1: by magnolia1
- [x] Scala types to Json asts
- [x] Json asts to Scala types
- [x] Json asts to string
- [x] Json asts to pretty string
- [x] Support product types
- [x] Support `@jsonField` annotation
- [x] Support `@jsonExclude` annotation
- [x] Support sum types (sealed trait)
- [x] Support sum types (enum)

```scala
    sealed trait Test0
    final case class Test1(d: Double, s: String, b: Boolean, l: List[Test2]) extends Test0
    // Nested classes are not automatically derived
    final case class Test2(d: Double, s: String, b: Option[Boolean]) derives JsonEncoder, JsonDecoder
    
    given JsonCodec[Test0] = DeriveJsonCodec.gen[Test0]
    
    val obj1  = Test1(1, "s", true, List(Test2(0.1, "212", None)))
    val json1 = JsonCodec[Test0].toJson(obj1)
    val back1 = JsonCodec[Test0].fromJson(json1)
```

## Usage 2: by pure Scala3
- [x] Scala types to Json asts
- [x] Json asts to Scala types
- [x] Json asts to string
- [x] Json asts to pretty string
- [x] Support product types
- [x] Support `@jsonField` annotation
- [x] Support `@jsonExclude` annotation
- [x] Support sum types (sealed trait)

```scala
    // Nested classes are not automatically derived 
    sealed trait Test0 
    final case class Test1(d: Double, s: String, b: Boolean, l: List[Test2]) extends Test0
    final case class Test2(d: Double, s: String, b: Option[Boolean])
    
    given JsonCodec[Test0] = JsonCodec(
      JsonEncoder.derived[Test0],
      JsonDecoder.derived[Test0],
    )
    // To actively invoke the second implementation, `derives` cannot be used.
    given JsonEncoder[Test1] = original.JsonEncoder.derived[Test1]
    given JsonDecoder[Test1] = original.JsonDecoder.derived[Test1]
    given JsonEncoder[Test2] = original.JsonEncoder.derived[Test2]
    given JsonDecoder[Test2] = original.JsonDecoder.derived[Test2]
    
    val obj1 = Test1(1, "s", true, List(Test2(0.1, "212", None)))
    val json1 = JsonCodec[Test0].toJson(obj1)
    val back1 = JsonCodec[Test0].fromJson(json1)
```