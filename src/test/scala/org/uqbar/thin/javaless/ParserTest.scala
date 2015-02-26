package org.uqbar.thin.javaless

import scala.language.implicitConversions
import scala.util.parsing.combinator.Parsers

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

class JavalessParserTest extends ParserTest with Parser {

	implicit def StringToIdentifier(s: String) = Identifier(s)

	"Javaless parse of" - {
		"identifiers" - {
			implicit val parser = identifier

			"should succeed" - {
				"with all letters" in { "aaab" should beParsedTo[Identifier]("aaab") }
				"with a single letter" in { "x" should beParsedTo[Identifier]("x") }
				"with a letter and digits" in { "bd32" should beParsedTo[Identifier]("bd32") }
			}

			"should fail with invalid identifiers" in { "1fds" should notBeParsed }
		}

		"methods" - {
			implicit val parser = method

			"private Integer x(){ }" should beParsedTo (Method("x", "Integer"))
		}

		"fields" - {
			implicit val parser = field

			"private Integer x;" should beParsedTo (Field("x", "Integer"))
		}

		"classes" - {
			implicit val parser = clazz

			"should succeed" - {
				"with an empty class" in {
					"class MyClass { }" should beParsedTo (Class("MyClass", Nil))
					"public class MyClass { }" should beParsedTo (Class("MyClass", Nil))
				}

				"with a fields" in {
					"class MyClass { private Integer x; }" should beParsedTo (Class("MyClass", Field("x", "Integer") :: Nil))
					"class MyClass { public String x; }" should beParsedTo (Class("MyClass", Field("x", "String") :: Nil))
					"class MyClass { MyClass x; public Float y; }" should beParsedTo (Class("MyClass", Field("x", "MyClass") :: Field("y", "Float") :: Nil))
				}

				"with a methods" in {
					"class MyClass { private Integer x(){ } }" should beParsedTo (Class("MyClass", Method("x", "Integer") :: Nil))
					"class MyClass { private Integer x(){ } public Float y(){ }}" should beParsedTo (Class("MyClass", Method("x", "Integer") :: Method("y", "Float") :: Nil))
				}

				"with fields and methods" in {
					"class MyClass { private Integer x; public Float y(){ } }" should beParsedTo (Class("MyClass", Field("x", "Integer") :: Method("y", "Float") :: Nil))
					"class MyClass { private Integer x(){ } public Float y; }" should beParsedTo (Class("MyClass", Method("x", "Integer") :: Field("y", "Float") :: Nil))
				}
			}
		}
	}
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// PARSER TEST
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

abstract class ParserTest extends FreeSpec with Matchers with Parsers {

	def parseAll[T](p: Parser[T], in: CharSequence): ParseResult[T]

	case class beParsedTo[T](expected: T)(implicit parser: Parser[T]) extends Matcher[String] {
		def apply(target: String) = {
			val result = parseAll(parser, target)
			MatchResult(
				result.successful && result.get == expected,
				if (result.successful) s"Parsed ${result.get} did not equal $expected" else s"Parse failed! $result",
				if (result.successful) s"Parsed ${result.get} was equal to $expected" else s"Parse didn't fail! $result"
			)
		}
	}

	case class beParsed[T](implicit parser: Parser[T]) extends Matcher[String] {
		def apply(target: String) = {
			val result = parseAll(parser, target)
			MatchResult(
				result.successful,
				s"Parse failed: $result",
				s"Parse didn't fail! $result"
			)
		}
	}

	def notBeParsed[T](implicit parser: Parser[T]) = not(beParsed()(parser))

}