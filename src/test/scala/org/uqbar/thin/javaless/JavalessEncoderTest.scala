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

	implicit val terminals = DefaultTerminals
	implicit val preferences = DefaultPreferences

	"Javaless encoding" - {

		"should succeed" - {
			
			"for classes" - {
				implicit val encoder = classDefinition

				"with no members" in {
					val emptyClass = Class("MyClass", Nil)

					emptyClass should beEncodedTo("class MyClass {}")(emptyClass -> 0.to(15), emptyClass.name -> 6.to(12), emptyClass.body -> 15.until(15))
				}

				"with a method definition" in {
					val emptyMethod = Method("calculate", Nil, Nil)
					val nonEmptyClass = Class("MyClass", List(emptyMethod))

					nonEmptyClass should beEncodedTo("""
						class MyClass {
							public calculate() {}
						}
					""")(nonEmptyClass -> 0.to(39), nonEmptyClass.name -> 6.to(12), nonEmptyClass.body -> 16.to(37), emptyMethod -> 17.to(37), emptyMethod.name -> 24.to(32), Nil-> 33.to(34)) 
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
					""")(nonEmptyClass -> 0.to(65), nonEmptyClass.name -> 6.to(12), nonEmptyClass.body -> 16.to(63), emptyMethod1 -> 17.to(37), emptyMethod1.name -> 24.to(32), emptyMethod2 -> 41.to(63), emptyMethod2.name -> 48.to(58), Nil-> 33.to(34)) 
				}

				"for methods" - {
					implicit val encoder = methodDefinition

					"with no arguments or body" in {
						val argumentlessEmptyMethod = Method("calculate", Nil, Nil)

						argumentlessEmptyMethod should beEncodedTo("public calculate() {}")(argumentlessEmptyMethod -> 0.to(20),argumentlessEmptyMethod.name -> 7.to(15), argumentlessEmptyMethod.arguments -> 16.to(17))
					}

					"with one argument but no body" in {
						val arg = Argument("void", "arg")
						val argumentedEmptyMethod = Method("calculate", arg :: Nil, Nil)

						argumentedEmptyMethod should beEncodedTo("public calculate(void arg) {}")(argumentedEmptyMethod -> 0.to(28), argumentedEmptyMethod.name -> 7.to(15), argumentedEmptyMethod.arguments -> 16.to(25), arg -> 17.to(24), arg.name -> 22.to(24), arg.atype -> 17.to(20))
					}

					"with arguments but no body" in {
						val arg1 = Argument("void", "arg1")
						val arg2 = Argument("void", "arg2")
						val argumentedEmptyMethod = Method("calculate", arg1 :: arg2 :: Nil, Nil)

						argumentedEmptyMethod should beEncodedTo("public calculate(void arg1, void arg2) {}")(argumentedEmptyMethod -> 0.to(40), argumentedEmptyMethod.name -> 7.to(15), argumentedEmptyMethod.arguments -> 16.to(37), arg1 -> 17.to(25), arg1.name -> 22.to(25), arg1.atype -> 17.to(20), arg2 -> 28.until(37), arg2.name -> 33.to(36), arg2.atype -> 17.to(20))
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

	case class beEncodedTo[T](tabulatedExpectedText: String)(expectedReferencesSeq: (Any, Range)*)(implicit encoder: Encoder[T], preferences: EncoderPreferences, terminals: Map[Symbol,String]) extends Matcher[T] {

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
			val result = encoder.encode(target)(preferences, terminals)
			val (success, message) = result match {
				case Success((text, _)) if text != expectedText => (false, s"""Encoded text: "${text.replaceAll(" ", "·").replaceAll("\t","»").replaceAll("\n", "¶\n")}" did not match expected text: "${expectedText.replaceAll(" ", "·").replaceAll("\t","»").replaceAll("\n", "¶\n")}"""")
				case Success((_, references)) =>
					val unexpectedReferences = references.filterNot(expectedReferences isDefinedAt _._1)
					val missedReferences = expectedReferences.filterNot(references isDefinedAt _._1)
					val wrongReferences = references.filterNot{ case (key, value) =>
						unexpectedReferences.isDefinedAt(key) ||
						missedReferences.isDefinedAt(key) ||
						value == expectedReferences(key)
					}
					if (unexpectedReferences.nonEmpty) (false, s"""Encoding yielded references to unexpected objects: ${unexpectedReferences.map{case (key, value) => s"$key: ${if(value.isEmpty) value.start else value.head} to ${if(value.isEmpty) value.end else value.last}"}.mkString("[", ", ", "]")}""")
					else if (missedReferences.nonEmpty) (false, s"""Encoding didn't yield references to expected objects: ${missedReferences.map{case (key, value) => s"$key: ${if(value.isEmpty) value.start else value.head} to ${if(value.isEmpty) value.end else value.last}"}.mkString("[", ", ", "]")}""")
					else if (wrongReferences.nonEmpty) (false, s"""Encoding yielded wrong references: ${wrongReferences.map{ case (key, value) => s"$key: ${if(value.isEmpty) value.start else value.head} to ${if(value.isEmpty) value.end else value.last} wasn't ${if(expectedReferences(key).isEmpty) expectedReferences(key).start else expectedReferences(key).head} to ${if(expectedReferences(key).isEmpty) expectedReferences(key).end else expectedReferences(key).last}" }.mkString("[", ", ", "]")}""")
					else (true, "Encoded was as expected")
				case Failure(e) =>
					val stack = new StringWriter
					e.printStackTrace(new PrintWriter(stack))					
					(false, s"Encoding failed because of $e: ${stack.toString}}")
			}

			MatchResult(success, message, message)
		}
	}

	case class beEncoded(implicit encoder: Encoder[T], preferences: EncoderPreferences, terminals: Map[Symbol,String]) extends Matcher[T] {
		def apply(target: T) = {
			val result = encoder.encode(target)(preferences, terminals)

			MatchResult(
				result.isSuccess,
				s"Encode was not a success: $result",
				s"Encode was a success: $result"
			)
		}
	}
}