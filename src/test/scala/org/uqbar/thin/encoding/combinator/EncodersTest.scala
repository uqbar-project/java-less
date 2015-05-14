package org.uqbar.thin.encoding.combinator

import scala.language.implicitConversions
import scala.reflect.runtime.universe
import scala.util.Success
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import scala.util.Failure
import java.io.StringWriter
import java.io.PrintWriter

trait T
case class X(s: String) extends T
case class Y(s: String, n: Int) extends T
case class Q(x: X, y: Y)
case class Z(t: T)
case class W(ts: List[T])

trait EncoderExample extends Encoders {
	val terminals = Map[Symbol, String]()
	implicit val preferences = new EncoderPreferences(
		spacing = Map().withDefaultValue(false)
	)

	lazy val foo: Encoder[Any] = "Foo"
	lazy val xStripped: Encoder[X] = __
	lazy val x: Encoder[X] = "X:" ~ __
	lazy val y: Encoder[Y] = "Y:" ~ __ ~ ":" ~ __
	lazy val q: Encoder[Q] = "Q(" ~ x ~ y ~ ")"
	lazy val t = x | y
	lazy val z: Encoder[Z] = "Z(" ~ t ~ ")"
	lazy val w: Encoder[W] = "W(" ~ t.*~(",") ~ ")"
}

class EncodersTest extends FreeSpec with Matchers with EncoderExample {

	val anX = X("foo")
	val aY = Y("bar", 5)
	val aQ = Q(anX, aY)
	val aZ = Z(anX)
	val anotherZ = Z(aY)
	val aW = W(anX :: aY :: Nil)

	"Encoder" - {

		"Id" - {
			"output should be equal to input" in {
				Id encodingOf () should resultIn ("")()
			}
		}

		"Access" - {
			"output text should be the encoded object toString" in {
				__ encodingOf (1) should resultIn("1")()
				__ encodingOf ("Foo") should resultIn("Foo")()
				__ encodingOf (Some('c')) should resultIn("Some(c)")()
			}
		}

		"Constant" - {
			"output text should be the given constant" in {
				Constant("Foo") encodingOf () should resultIn("Foo")()
			}

			"should be implicitly created from a String" in {
				("Foo": Encoder[_]) should be (Constant("Foo"))
			}
		}

		"Append" - {
			"output text should be the result of appending the output of the given encoders" in {
				"Foo" ~ "Bar" encodingOf () should resultIn("FooBar")()
				"Foo" ~ "Bar" ~ "Meh" encodingOf () should resultIn("FooBarMeh")()
				"Foo" ~ __ ~ "Bar" encodingOf (5) should resultIn("Foo5Bar")()
			}

			"should have syntactic sugar to be crated from target encoders" in {
				"Foo" ~ __ ~ "Bar" should be (Append(Append(Constant("Foo"), __), Constant("Bar")))
			}
		}

		"Extract" - {
			"given a transform function, output text should be the output of the given encoder, applied to the target transformed by that function" in {
				val target = Some(58)

				__ ^^ { e: Option[Any] => e.get :: Nil } encodingOf (target) should resultIn("58")(target -> 0.until(2))
				"X:" ~ __ ^^ { e: Option[Any] => e.get :: Nil } encodingOf (target) should resultIn("X:58")(target -> 0.until(4))
				"X:" ~ __ ~ __ ^^ { e: Option[Any] => e.get :: Nil } encodingOf (target, 3) should resultIn("X:583")(target -> 0.until(5))
			}

			"given a type extended from Product, output text should be the output of the given encoder, applied to the target instance of that type's fields in declaration order" in {
				val target = ("Foo", "Bar")

				(__ ~ ": " ~ __).^^[Tuple2[String, String]] encodingOf (target) should resultIn("Foo: Bar")(target -> 0.until(8))
			}

			"should be implicitly created from a non matching type expectation" in {
				((__ ~ __): Encoder[Tuple2[String, String]]) should be ((__ ~ __).^^[Tuple2[String, String]])
			}
		}

		"Or" - {
			"output should be the output of the left encoder or, if it fails, the output of the right one" in {
				val x: Encoder[X] = "X:" ~ __
				val y: Encoder[Y] = "Y:" ~ __ ~ ":" ~ __

				val targetX: T = X("foo")
				val targetY: T = Y("bar",5)

				x | y encodingOf (targetX) should resultIn("X:foo")(targetX -> 0.until(5))
				y | x encodingOf (targetX) should resultIn("X:foo")(targetX -> 0.until(5))
				x | y encodingOf (targetY) should resultIn("Y:bar:5")(targetY -> 0.until(7))
				y | x encodingOf (targetY) should resultIn("Y:bar:5")(targetY -> 0.until(7))
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
				"{" ~~ ("Foo") ~ "}" encodingOf () should resultIn("{\n\tFoo\n}")()
			}

			"should be nestable" in {
				"class C {" ~~ ("method M {" ~~ (__.*) ~ "}") ~ "}" encodingOf (List("Line 1", "Line 2", "Line 3")) should resultIn("class C {\n\tmethod M {\n\t\tLine 1\n\t\tLine 2\n\t\tLine 3\n\t}\n}")()
			}

			"should have syntactic sugar  to be crated from target encoders" in {
				"Foo" ~~(__) ~ "Bar" should be (Append(Append(Constant("Foo"), Subcontext(__)),Constant("Bar")))
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
				case Success((text, _, _)) if text != expectedText => (false, s"""Encoded text: "$text" did not match expected text: "$expectedText"""")
				case Success((_, references, _)) =>
					val unexpectedReferences = references.filterNot(expectedReferences isDefinedAt _._1)
					val missedReferences = expectedReferences.filterNot(references isDefinedAt _._1)
					val wrongReferences = references.
						filterNot{ case (key, _) => unexpectedReferences.isDefinedAt(key) || missedReferences.isDefinedAt(key) }.
						map{ case (key, value) => (key, (value.start, value.end), (expectedReferences(key).start, expectedReferences(key).end)) }.
						filter{ case (key, value, expected) => value != expected }

					if (unexpectedReferences.nonEmpty) (false, s"""Encoding yielded references to unexpected objects: ${unexpectedReferences.keys.mkString("[", ", ", "]")}""")
					else if (missedReferences.nonEmpty) (false, s"""Encoding didn't yield references to expected objects: ${missedReferences.keys.mkString("[", ", ", "]")}""")
					else if (wrongReferences.nonEmpty) (false, s"""Encoding yielded wrong references: ${wrongReferences.map{ case (k, (vs, ve), (es, ee)) => s"$k: $vs to $ve != $es to $ee" }.mkString("[", ", ", "]")}""")
					else (true, "Encoded was as expected")
				case Failure(e) =>
					val stack = new StringWriter
					e.printStackTrace(new PrintWriter(stack))
					(false, s"Encoding failed because of $e: ${stack.toString}}")
			}

			MatchResult(success, message, message)
		}
	}

	protected implicit class TestedEncoder(target: Encoder[_]) {
		def encodingOf(stack: Any*)(implicit preferences: EncoderPreferences) = target(preferences)(EncoderResult(stack: _*))
	}
}