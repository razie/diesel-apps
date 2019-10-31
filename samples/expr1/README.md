A simple example of using the diesel expressions.

Just cd to here and run:

```
sbt run sample.SampleExprParser
```

And you should see something like this:

```
> run sample.SampleExprParser
[info] Running sample.SampleExprParser sample.SampleExprParser
Some(:Number="3")
[success] Total time: 0 s, completed 30-Oct-2019 8:43:44 PM
```

The build file is `build.sbt` and the main file running a simple expression is
`src/main/scala/SampleExprParser.scala`, which looks like:

```scala
import razie.diesel.expr._
import razie.diesel.dom.RDOM._

object SampleExprParser {
  def main(args: Array[String]): Unit = {
   val input = "a + 2"
   val parser = new SimpleExprParser()
   // create and initialize context with "a"
   implicit val ctx: ECtx = new SimpleECtx().withP(P.fromTypedValue("a", 1))

   // parse and execute any expression
   val result = parser.parseExpr(input).map(_.applyTyped(""))

   println(result)
   }
}
```
