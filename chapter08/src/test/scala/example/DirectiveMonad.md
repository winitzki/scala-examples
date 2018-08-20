Support the Scala `for`/`yield` syntax in route directives, simplify route code.

Note: [PR was made to Akka-HTTP](https://github.com/akka/akka-http/pull/1925) but was rejected, with a suggestion to publish this code as a separate library.

# Writing routes in `for`/`yield` syntax

The default Akka-HTTP style is to use nested functions for defining routes, for example:

```scala
val myRoute: Route = get {
  path("test" / Segment) { userId =>
    val user = userById(userId)
    extractLog { log =>
      extractUri { uri =>
        parameter('x, 'y, 'z) { (x, y, z) =>
          log.info(s"Got URI $uri with parameters $x, $y, $z") // log this before parsing JSON
          entity(as[MyCaseClass]) { input =>
            // `computeAsFuture` is our business logic function returning a `Future`
            onSuccess(computeAsFuture(input, user, x, y, z)) { result =>
              complete(MyResultCaseClass(result))
            }
          }
        }
      }
    }
  }
}
```

Each of the functions `get`, `path`, `extractLog`, `entity` etc. are defined by the Akka-HTTP library as values of type `Directive[L]`.
A `Directive[L]` has a `tapply()` method that takes as an argument a function of type `L => Route` and returns a `Route`.
Therefore, each directive in a route requires you to write one more nested function of type `L => Route`.

It also follows that a `Directive[L]` is equivalent to a value of type `(L => Route) => Route`, which is known in functional programming as the "continuation monad".
Being a monad, this type can be used in `for`/`yield` syntax, which is idiomatic in Scala, and avoids having to write several nested functions.
The only missing part is the methods `map`, `flatMap`, and `withFilter` as required by the `for`/`yield` syntax.

The class `Directive[L]` already has methods `tmap`, `tflatMap`, and `tfilter` but their type signatures are not suitable because of the type class constraint on `L` and because of using magnets.

This PR adds syntax helpers that enable the `for`/`yield` syntax for routes, so that the route above can be rewritten like this:
 
```scala
import DirectiveMonad._

val myRoute: Route = for {
  _         <- get
  userId    <- path("test" / Segment)
  user      =  userById(userId)
  log       <- extractLog
  uri       <- extractUri
  (x, y, z) <- parameter('x, 'y, 'z)
  _         =  log.info(s"Got URI $uri with parameters $x, $y, $z") // log this before parsing JSON
  input     <- entity(as[MyCaseClass])
  result    <- onSuccess(computeAsFuture(input, user, x, y, z))
} yield MyResultCaseClass(result) // no need for `complete()`
```

[All Akka-HTTP directives](https://doc.akka.io/docs/akka-http/current/routing-dsl/directives/alphabetically.html) can be used without any changes in this syntax.
Directive operations such as `&` can be used as well, in the right-hand side of `<-`.

The new syntax is added using a zero-allocation wrapper, `WrapDirective[L]`, which can be created out of a `Directive[L]` or a `Directive1[L]` automatically via an implicit conversion.
There are also implicit conversions from `WrapDirective[L]` back to `Directive[L]` and `Directive1[L]` for convenience.

## Advantages of this syntax

- the `for`/`yield` syntax is a standard, idiomatic Scala syntax for sequential computations
- code is easier to read and refactor because it is not deeply nested
- directives become composable in the usual way, as monadic values
- further enrichment can be done using monad transformers

Example of refactoring several routes with a common prefix:

```scala
val commonDirectives = for {
  _ <- get
  _ <- pathPrefix("my_app")
} yield ()

val route1: Route = ???
val route2: Route = ???

val route: Route = for {
  _ <- commonDirectives
} yield route1 ~ route2
```

## Remarks

- Directives that appear to have no arguments (such as `get`) need to be used with the syntax `_ <- get` (these directives are of type `Directive0` and have an argument of type `Unit`).
- Ordinary (non-directive) computations within the `for` block are written as `a = b` instead of `val a = b`.
- Computations returning `Unit`, such as `log.info(...)`, are written as `_ = log.info(...)`.
- Filtering conditions such as `if x.length > 0` are permitted within the `for` block as usual; the route will be rejected if the condition fails.
- To return a `complete(xyz)`, just write `yield xyz`, where `xyz` must be a marshallable value (a string, a case class, or an http response.)
- For convenience, `yield` can be also used on a `Route`, so that `yield complete(xyz)`, `yield reject(...)`, etc. are also supported.
- Directive methods, such as `recover()`, `&` etc., can be used on a `WrapDirective` (i.e. on the result of `for/yield`) as well.

## Disadvantages of this syntax

- Java compatibility may suffer: it may be harder to call this route code from Java, I have not tried
- Route code that is significantly non-linear, with many branchings, may be harder to represent, because the monad
syntax is best for linearly arranged sequences of computations where each next computation uses results of previous ones
