# Expressions

Expressions: parser, evaluation, contexts etc.

Many forms of expressions are permitted, including arithmetic, logical etc, like:

```js
[1,2] + [3] filter (x=> x > 1) map (x=> x + 1) 
```

See complete detailed examples in [expr_story](http://specs.razie.com/wiki/Story:expr_story) - here are some 
quick examples (right side of the `=` sign):

```js
$send ctx.set (
  constants = 321, 
  addition = last+first,
  jsonBlock={
    "accountNumber": "1729534",
    "start_num": 123
	},
  qualified = jsonBlock.accountNumber,
  interpolation="is${1+2}",
  url="${HOST}/search?q=dieselapps",
  builtInFunctions1 = sizeOf(x=cart.items),
  builtInFunctions2 = typeOf(x=cart.items),
  
  anArray =[1,"a",3],

  // json operations
  cart=cart || {id:customer, items:[] } ,
  cart=cart+{items:{sku : sku, quantity : quantity} } ,
  
  // embedded Javascript expressions
  simpleJs = js:wix.diesel.env ,
  res78=js:cart.items[0].sku ,
  
  res40=js:email.replace(/(\w+)@(\w+).com/, "$1") ,
  
  later=js:{var d = new Date(); d.setSeconds(d.getSeconds() + 10); d.toISOString();} ,

  a284 = [1,2] + [3] filter (x=> x > 1) map (x=> x + 1) 
)

```

The samples at [expr_story](http://specs.razie.com/wiki/Story:expr_story) are also unit tests and the reference for expressions.

## Using

Easy to use if your project requires expressions. Parse a DSL expression with the ExprParser:

```scala
import razie.diesel.expr._
import razie.diesel.dom.RDOM._

val input = "a + 2"
val parser = new SimpleExprParser()
// create and initialize context with "a"
val ctx: ECtx = new SimpleECtx().withP(P.fromTypedValue("a", 1))

// parse and execute any expression
val result = parser.parseExpr(input).map(_.applyTyped("")(new SimpleECtx()))

// parse and execute a specific expr (say identifiers)
val result2 = parser.parseIdent(input).map(_.applyTyped("")(new SimpleECtx()))

// parse and execute a specific expr (say conditions)
val result3 = parser.parseCond(input).map(_.bapply("")(new SimpleECtx()))
```

# Contexts

An important part of evaluating expressions is the context they evaluate in (like a closure). This
is represented by the ECtx and related contexts:
- SimpleECtx - a siple context
- ScopeECtx - does not propagate udpates
- StaticECtx - delegates all updates to parent

Contexts are hierarchical (each has one parent/base) and tie into [tconf](tconf).

Note that contexts have a weak tie to a DieselEngine (via `root` and auth etc), but, if you just use them for 
expressions, you don't need an engine.

