package org.uqbar.thin.javaless

import java.io.PrintWriter
import java.io.StringWriter

import scala.annotation.migration
import scala.language.implicitConversions
import scala.util.Failure
import scala.util.Success

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import org.uqbar.thin.encoding.combinator.Encoder
import org.uqbar.thin.encoding.combinator.EncoderPreferences
import org.uqbar.thin.encoding.combinator.T

class JavalessEncoderTest extends FreeSpec with EncoderTest with JavalessEncoderDefinition {

	val terminals = DefaultTerminals
	implicit val preferences = DefaultPreferences

	"Javaless encoding" - {

		"should succeed" - {
			
			"for classes" - {
				implicit val encoder = classDefinition

				"with no members" in {
					val emptyClass = Class("MyClass", Nil)

					emptyClass should beEncodedTo("class MyClass {}")(emptyClass.name -> 6.until(13), emptyClass.body -> 15.until(15))
				}

				"with a method definition" in {
					val emptyMethod = Method("calculate", Nil, Nil)
					val nonEmptyClass = Class("MyClass", List(emptyMethod))

					nonEmptyClass should beEncodedTo("""
						class MyClass {
							public calculate() {}
						}
					""")(nonEmptyClass.name -> 6.until(13), nonEmptyClass.body -> 16.until(38), emptyMethod -> 16.until(38), emptyMethod.name -> 24.until(33), Nil-> 33.until(35)) 
				}

				"with two method definitions" in {
					val emptyMethod1 = Method("calculate", Nil, Nil)
					val emptyMethod2 = Method("recalculate", Nil, Nil)
					val nonEmptyClass = Class("MyClass", List(emptyMethod1, emptyMethod2))
							
					nonEmptyClass should beEncodedTo("""
						class MyClass {
							public calculate() {}

							public recalculate() {}
						}
					""")(nonEmptyClass -> 0.until(65), emptyMethod1 -> 16.until(38), emptyMethod1 -> 40.until(64)) 
				}

				"for methods" - {
					implicit val encoder = methodDefinition

					"with no arguments or body" in {
						val argumentlessEmptyMethod = Method("calculate", Nil, Nil)

						argumentlessEmptyMethod should beEncodedTo("public calculate() {}")(argumentlessEmptyMethod.name -> 7.until(16), argumentlessEmptyMethod.arguments -> 16.until(18))
					}

					"with one argument but no body" in {
						val arg = Argument("void", "arg")
						val argumentedEmptyMethod = Method("calculate", arg :: Nil, Nil)

						argumentedEmptyMethod should beEncodedTo("public calculate(void arg) {}")(argumentedEmptyMethod.name -> 7.until(16), argumentedEmptyMethod.arguments -> 16.until(26), arg -> 17.until(25), arg.name -> 22.until(25), arg.atype -> 17.until(21))
					}

					"with arguments but no body" in {
						val arg1 = Argument("void", "arg1")
						val arg2 = Argument("void", "arg2")
						val argumentedEmptyMethod = Method("calculate", arg1 :: arg2 :: Nil, Nil)

						argumentedEmptyMethod should beEncodedTo("public calculate(void arg1, void arg2) {}")(argumentedEmptyMethod.name -> 7.until(16), argumentedEmptyMethod.arguments -> 16.until(38), arg1 -> 17.until(26), arg1.name -> 22.until(26), arg1.atype -> 17.until(21), arg2 -> 28.until(37), arg2.name -> 33.until(37), arg2.atype -> 17.until(21))
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

			
		val expectedText = expectedTextParts.zipWithIndex.filterNot{
			case ("", 0) => true
			case (part, n) => n == expectedTextParts.size - 1 && part.forall{ " \t" contains _ }
		}.map(_._1).mkString("\n")
		
		val expectedReferences = expectedReferencesSeq.toMap

		def apply(target: T) = {
			val result = encoder.encode(target)(preferences)
			val (success, message) = result match {
				case Success((text, _)) if text != expectedText => (false, s"""Encoded text: "$text" did not match expected text: "$expectedText"""")
				case Success((_, references)) =>
					val unexpectedReferences = references.filterNot(expectedReferences isDefinedAt _._1)
					val missedReferences = expectedReferences.filterNot(references isDefinedAt _._1)
					val wrongReferences = references.
						filterNot{ case (key, _) => unexpectedReferences.isDefinedAt(key) || missedReferences.isDefinedAt(key) }.
						map{ case (key, value) => (key, (value.start, value.end), (expectedReferences(key).start, expectedReferences(key).end)) }.
						filter{ case (key, value, expected) => value != expected }

					if (unexpectedReferences.nonEmpty) (false, s"""Encoding yielded references to unexpected objects: ${unexpectedReferences.keys.mkString("[", ", ", "]")}""")
					else if (missedReferences.nonEmpty) (false, s"""Encoding didn't yield references to expected objects: ${missedReferences.keys.mkString("[", ", ", "]")}""")
					else if (wrongReferences.nonEmpty) (false, s"""Encoding yielded wrong references: ${wrongReferences.map{ case (k, (vs, ve), (es, ee)) => s"$k: $vs to $ve wasn't $es to $ee" }.mkString("[", ", ", "]")}""")
					else (true, "Encoded was as expected")
				case Failure(e) =>
					val stack = new StringWriter
					e.printStackTrace(new PrintWriter(stack))					
					(false, s"Encoding failed because of $e: ${stack.toString}}")
			}

			MatchResult(success, message, message)
		}
	}

	case class beEncoded(implicit encoder: Encoder[T], preferences: EncoderPreferences) extends Matcher[T] {
		def apply(target: T) = {
			val result = encoder.encode(target)(preferences)

			MatchResult(
				result.isSuccess,
				s"Encode was not a success: $result",
				s"Encode was a success: $result"
			)
		}
	}
}