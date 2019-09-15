package tests

import org.scalatest.{FlatSpec, MustMatchers, OptionValues, WordSpecLike}
import razie.diesel.dom.{CExpr, WTypes}
import razie.diesel.expr.{AExprIdent, CExpr}
import razie.wiki.parser.ExprParser

/** A simple parser for expressions */
class SimpleExprParser extends ExprParser {
  def apply(input: String) = {
    parseAll(expr, input) match {
      case Success(value, _) => value
      // don't change the format of this message
      case NoSuccess(msg, next) => throw new IllegalArgumentException("CANNOT PARSE: "+input)
    }
  }
}

/**
  * run like: sbt 'testOnly *TestExprParser'
  */
class TestExprParser extends WordSpecLike with MustMatchers with OptionValues {

  def p(s:String) = (new SimpleExprParser).apply(s)

  "CExpr parser" should {

    "parse numbers" in {
      assert(p( "3" ).isInstanceOf[CExpr[_]])
      assert(p( "3" ).getType == WTypes.NUMBER)
    }

    "parse strings" in {
      assert(p( "\"a string\"" ).isInstanceOf[CExpr[_]])
      assert(p( "\"a string\"" ).getType == WTypes.STRING)
    }
  }

  "id parser" should {

    "parse id" in {
      assert(p( "anid" ).isInstanceOf[AExprIdent])
      assert(p( "'an id'" ).isInstanceOf[AExprIdent])
//      assert(p( "anid[23]" ).isInstanceOf[AExprIdent])
//      assert(p( "anid[\"field1\"]" ).isInstanceOf[AExprIdent])
//      assert(p( "anid[\"field1\"][4]" ).isInstanceOf[AExprIdent])
//      assert(p( "anid[\"field1\"][4].jake.gg[4\[\"asfd\"]" ).isInstanceOf[AExprIdent])
    }
  }
}

