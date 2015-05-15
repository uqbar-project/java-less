package org.uqbar.thin.encoding.combinator

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

trait EncoderExample extends Encoders {
	val terminals = Map[Symbol, String]()
	implicit val preferences = new EncoderPreferences(
		spacing = Map().withDefaultValue(false)
	)
}

trait T
case class X(s: String) extends T
case class Y(s: String, n: Int) extends T

class EncodersTest extends FreeSpec with Matchers with EncoderExample {

	"Encoder" - {

		"Access" - {
			"output text should be the encoded object toString" in {
				__ encodingOf (1) should resultIn("1")()
				__ encodingOf ("Foo") should resultIn("Foo")()
				__ encodingOf (Some('c')) should resultIn("Some(c)")()
			}
		}

		"Constant" - {
			"output text should be the given constant" in {
				Constant("Foo") encodingOf (null) should resultIn("Foo")()
			}

			"should be implicitly created from a String" in {
				("Foo": Encoder[_]) should be (Constant("Foo"))
			}
		}

		"Append" - { 
			"output text should be the result of appending the output of the given encoders" in {
				"Foo" ~ "Bar" encodingOf (null) should resultIn("FooBar")()
				"Foo" ~ "Bar" ~ "Meh" encodingOf (null) should resultIn("FooBarMeh")()
				"Foo" ~ __ ~ "Bar" encodingOf (5) should resultIn("Foo5Bar")()
			}
			
			"should have syntactic sugar to be crated from target encoders" in {
				"Foo" ~ __ ~ "Bar" should be (Append(Append(Constant("Foo"), __), Constant("Bar")))
			}
		}

		"Transform" - {
			"given a transform function, the output should be the output of the given encoder, applied to the target transformed by that function" in {
				val target = Some(58)

				__{ e: Option[Any] => e.get } encodingOf (target) should resultIn("58")(58 -> 0.until(2))
				("X:" ~ __ : Encoder[Any]){ e: Option[Any] => e.get } encodingOf (target) should resultIn("X:58")(58 -> 0.until(4))
			}

			"should have syntactic sugar to be crated from target encoders" in {
				__{ x: Any => x.toString } should be (Tx(__){ x: Any => x.toString })
			}
		}

		"Or" - {  
			"output should be the output of the left encoder or, if it fails, the output of the right one" in {
				val x: Encoder[X] = "X:" ~ __{ x: X => x.s }
				val y: Encoder[Y] = "Y:" ~ __{y: Y => y.s} ~ ":" ~ __{y: Y => y.n}

				val targetX = X("foo") 
				val targetY = Y("bar", 5)
  
				(x | y : Encoder[T]) encodingOf (targetX) should resultIn("X:foo")("foo" -> 2.until(5))
				y | x encodingOf (targetX) should resultIn("X:foo")("foo" -> 2.until(5))
				x | y encodingOf (targetY) should resultIn("Y:bar:5")("bar" -> 2.until(5), 5 -> 6.until(7))
				(y | x : Encoder[T]) encodingOf (targetY) should resultIn("Y:bar:5")("bar" -> 2.until(5), 5 -> 6.until(7))
			}

			"should have syntactic sugar to be crated from target encoders" in {
				"Foo" | "Bar" | "Meh" should be (Or(Or(Constant("Foo"), Constant("Bar")), Constant("Meh")))
			}
		}

		"RepSep" - {
			val encoder = __.*~("|")

			"if target list is empty, output should be the empty" in {
				encoder encodingOf (Nil) should resultIn("")()
			}

			"if target list has only one element, output should be the output of target encoder, applied to that element, with no separator" in {
				encoder encodingOf (List(1)) should resultIn("1")()
			}

			"if target list has more than one element, output should be the output of target encoder, applied to each element, separated by the output of the separator encoder" in {
				encoder encodingOf (List(1, 2, 3)) should resultIn("1|2|3")()
			}

			"should have syntactic sugar to be crated from target encoders" in {
				encoder should be (RepSep(__, Constant("|")))
			}

			"should have syntactic sugar for no separator repetition, to be crated from target encoders" in {
				__.* should be (RepSep(__, Constant("")))
			}
		}

		"Subcontext" - {
			"output should be the target encoder output, with one more tabulation level" in {
				"{" ~~ ("Foo") ~ "}" encodingOf (null) should resultIn("{\n\tFoo\n}")()
			}

			"should be nestable" in {
				"class C {" ~~ ("method M {" ~~ (__.*) ~ "}") ~ "}" encodingOf (List("Line 1", "Line 2", "Line 3")) should resultIn("class C {\n\tmethod M {\n\t\tLine 1\n\t\tLine 2\n\t\tLine 3\n\t}\n}")()
			}

			"should have syntactic sugar  to be crated from target encoders" in {
				"Foo" ~~ (__) ~ "Bar" should be (Append(Append(Constant("Foo"), Subcontext(__)), Constant("Bar")))
			}
		}
	}

	//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
	// MATCHERS
	//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

	protected case class resultIn(expectedText: String, expectedPending: List[Any] = Nil)(expectedReferencesSeq: (Any, Range)*) extends Matcher[EncoderResult] {
		val expectedReferences = expectedReferencesSeq.toMap

		def apply(target: EncoderResult) = {
			val (success, message) = target match {
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

	protected implicit class TestedEncoder[T](encoder: Encoder[T]) {
		def encodingOf(target: T)(implicit preferences: EncoderPreferences) = encoder(preferences)(target)
	}
}