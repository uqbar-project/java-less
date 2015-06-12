package org.uqbar.thin.encoding.combinator

import java.io.PrintWriter
import java.io.StringWriter

import scala.annotation.migration
import scala.language.implicitConversions
import scala.util.Failure
import scala.util.Success

import org.scalatest.Finders
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

trait T
case class X(s: String) extends T
case class Y(s: String, n: Int) extends T

class EncodersTest extends FreeSpec with Matchers with Encoders {

	implicit val terminals = Map[Symbol, String](
		'Foo -> "Foo",
		'Bar -> "Bar",
		'Meh -> "Meh",
		'X -> "X:",
		'Y -> "Y:",
		': -> ":"
	)
	
	implicit val preferences = new EncoderPreferences(
		spacing = Set(),
		lineBreaks = Map().withDefaultValue(0),
		tabulationLevelIncrements = Map().withDefaultValue(0),
		tabulationSequence = "\t",
		tabulationSize = 1
	)

	"Encoder" - {

		"Reference" - { 
			"output text should be the encoded object toString" in {
				& encodingOf (1) should resultIn("1")(1 -> 0.to(0))
				& encodingOf ("Foo") should resultIn("Foo")("Foo" -> 0.to(2))
				
				val option = Some('c')
				& encodingOf (option) should resultIn("Some(c)")(option -> 0.to(6))
			}
		}

		"Constant" - {
			"output text should be the given constant" in {
				Constant('Foo) encodingOf (()) should resultIn("Foo")(() -> 0.to(2))
			}

			"should be implicitly created from a Symbol" in {
				('Foo: Encoder[_]) should be (Constant('Foo))
			}
		}

		"Append" - {
			"output text should be the result of appending the output of the given encoders" in {
				'Foo ~ 'Bar encodingOf (()) should resultIn("FooBar")(() -> 0.to(5))
				'Foo ~ 'Bar ~ 'Meh encodingOf (()) should resultIn("FooBarMeh")(() -> 0.to(8))
				'Foo ~ & ~ 'Bar encodingOf (5) should resultIn("Foo5Bar")(5 -> 0.to(6))
			}
			
			"should have syntactic sugar to be crated from target encoders" in {
				'Foo ~ & ~ 'Bar should be (Append(Append(Constant('Foo), &), Constant('Bar)))
			}
		}

		"Transform" - {
			"given a transform function, the output should be the output of the given encoder, applied to the target transformed by that function" in {
				val target = Some(58)

				&{ e: Option[Any] => e.get } encodingOf (target) should resultIn("58")(58 -> 0.until(2))
				('X ~ & : Encoder[Any]){ e: Option[Any] => e.get } encodingOf (target) should resultIn("X:58")(58 -> 0.until(4))
			}

			"should have syntactic sugar to be crated from target encoders" in {
				&{ x: Any => x.toString } should be (Transform(&){ x: Any => x.toString })
			}
		}

		"Or" - {  
			"output should be the output of the left encoder or, if it fails, the output of the right one" in {
				val x: Encoder[X] = 'X ~ &{ x: X => x.s }
				val y: Encoder[Y] = 'Y ~ &{y: Y => y.s} ~ ': ~ &{y: Y => y.n}

				val targetX = X("foo") 
				val targetY = Y("bar", 5)
  
				(x | y : Encoder[T]) encodingOf (targetX) should resultIn("X:foo")(targetX -> 0.to(4), "foo" -> 2.until(5))
				y | x encodingOf (targetX) should resultIn("X:foo")(targetX -> 0.to(4), "foo" -> 2.until(5))
				x | y encodingOf (targetY) should resultIn("Y:bar:5")(targetY -> 0.to(6), "bar" -> 2.until(5), 5 -> 6.until(7))
				(y | x : Encoder[T]) encodingOf (targetY) should resultIn("Y:bar:5")(targetY -> 0.to(6), "bar" -> 2.until(5), 5 -> 6.until(7))
			}

			"should have syntactic sugar to be crated from target encoders" in {
				'Foo | 'Bar | 'Meh should be (Or(Or(Constant('Foo), Constant('Bar)), Constant('Meh)))
			}
		}

		"RepSep" - {
			val encoder = &.*~(':)

			"if target list is empty, output should be the empty" in {
				encoder encodingOf (Nil) should resultIn("")(Nil -> 0.until(0))
			}

			"if target list has only one element, output should be the output of target encoder, applied to that element, with no separator" in {
				val target = List(1)
				encoder encodingOf (target) should resultIn("1")(target -> 0.to(0), 1 -> 0.to(0))
			}

			"if target list has more than one element, output should be the output of target encoder, applied to each element, separated by the output of the separator encoder" in {
				val target = List(1, 2, 3)
				encoder encodingOf (target) should resultIn("1:2:3")(target -> 0.to(4), 1 -> 0.to(0),2 -> 2.to(2),3 -> 4.to(4))
			}

			"should have syntactic sugar to be crated from target encoders" in {
				encoder should be (RepSep(&, Constant(':)))
			}

			"should have syntactic sugar for no separator repetition, to be crated from target encoders" in {
				&.* should be (RepSep(&, Empty))
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

	protected implicit class TestedEncoder[T](encoder: Encoder[T]) {
		def encodingOf(target: T)(implicit preferences: EncoderPreferences, terminals: Map[Symbol,String]) = encoder.encode(target)(preferences, terminals)
	}
}