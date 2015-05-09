package org.uqbar.thin.javaless

import scala.language.implicitConversions
import scala.reflect.runtime.universe
import scala.util.Success
import org.uqbar.thin.encoding.combinator._
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import scala.util.Failure

class JavalessEncoderTest extends FreeSpec with EncoderTest with JavalessEncoderDefinition {

	val terminals = DefaultTerminals
	implicit val preferences = DefaultPreferences

	"Javaless encoding" - {

		"should succeed" - {
			"for classes" - {
				implicit val encoder = classDefinition

				"with no members" in {
					val emptyClass = Class("MyClass", Nil)

					emptyClass should beEncodedTo("class MyClass {}")(emptyClass -> 0.until(16))
				}

				"with a method definition" in {
					val emptyMethod = Method("calculate", Nil, Nil)
					val nonEmptyClass = Class("MyClass", List(emptyMethod))

					nonEmptyClass should beEncodedTo("""
						class MyClass {
							public calculate() {}
						}
					""")(nonEmptyClass -> 0.until(40), emptyMethod -> 16.until(38)) 
				}

				"for methods" - {
					implicit val encoder = methodDefinition

					"with no arguments or body" in {
						val argumentlessEmptyMethod = Method("calculate", Nil, Nil)

						argumentlessEmptyMethod should beEncodedTo("public calculate() {}")(argumentlessEmptyMethod -> 0.until(21))
					}

					"with one argument but no body" in {
						val arg = Argument("void", "arg")
						val argumentedEmptyMethod = Method("calculate", arg :: Nil, Nil)

						argumentedEmptyMethod should beEncodedTo("public calculate(void arg) {}")(argumentedEmptyMethod -> 0.until(29), arg -> 17.until(25))
					}

					"with arguments but no body" in {
						val arg1 = Argument("void", "arg1")
						val arg2 = Argument("void", "arg2")
						val argumentedEmptyMethod = Method("calculate", arg1 :: arg2 :: Nil, Nil)

						argumentedEmptyMethod should beEncodedTo("public calculate(void arg1, void arg2) {}")(argumentedEmptyMethod -> 0.until(41), arg1 -> 17.until(26), arg2 -> 28.until(37))
					}

				}

			}
		}

	}

}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// ENCODER TEST
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

trait EncoderTest extends Matchers {

	case class beEncodedTo[T](tabulatedExpectedText: String)(expectedReferencesSeq: (Any, Range)*)(implicit encoder: Encoder[T], preferences: EncoderPreferences) extends Matcher[T] {

		val unwantedTabulation = """^\n?([\t| ]*)[^\$]*""".r.unapplySeq(tabulatedExpectedText).fold("")(_.head)
		val expectedTextParts = tabulatedExpectedText.
			split("\n").
			map(s => if (s.startsWith(unwantedTabulation)) s.replaceFirst(unwantedTabulation, "") else s).
			toList

		val expectedText = expectedTextParts.filterNot{ part =>
			(part == expectedTextParts.head && part.isEmpty) || (part != expectedTextParts.head && part == expectedTextParts.last && part.filterNot{ c => c == ' ' || c == '\t' }.isEmpty)
		}.mkString("\n") 

		val expectedReferences = expectedReferencesSeq.toMap

		def apply(target: T) = {
			val result = encoder(preferences)(if (target.isInstanceOf[List[_]]) EncoderResult(target.asInstanceOf[List[_]]: _*) else EncoderResult(target))
			val (success, message) = result match {
				case Success((text, _, _)) if text != expectedText => (false, s"""Encoded text: "$text" did not match expected text: "$expectedText"""")
				case Success((_, references, _)) =>
					val unexpectedReferences = references.filterNot(expectedReferences isDefinedAt _._1)
					val missedReferences = expectedReferences.filterNot(references isDefinedAt _._1)
					val wrongReferences = references.
						map{ case (key, value) => (key, (value.start, value.end), (expectedReferences(key).start, expectedReferences(key).end)) }.
						filter{ case (key, value, expected) => value != expected }

					if (unexpectedReferences.nonEmpty) (false, s"""Encoding yielded references to unexpected objects: ${unexpectedReferences.keys.mkString("[", ", ", "]")}""")
					else if (missedReferences.nonEmpty) (false, s"""Encoding didn't yield references to expected objects: ${missedReferences.keys.mkString("[", ", ", "]")}""")
					else if (wrongReferences.nonEmpty) (false, s"""Encoding yielded wrong references: ${wrongReferences.map{ case (k, (vs, ve), (es, ee)) => s"$k: $vs to $ve != $es to $ee" }.mkString("[", ", ", "]")}""")
					else (true, "Encoded was as expected")
				case Failure(e) => (false, "Encoding failed because of $e")
			}

			MatchResult(success, message, message)
		}
	}

	case class beEncoded(implicit encoder: Encoder[_], preferences: EncoderPreferences) extends Matcher[T] {
		def apply(target: T) = {
			val result = encoder(preferences)(EncoderResult(target))

			MatchResult(
				result.isSuccess,
				s"Encode was not a success: $result",
				s"Encode was a success: $result"
			)
		}
	}
}