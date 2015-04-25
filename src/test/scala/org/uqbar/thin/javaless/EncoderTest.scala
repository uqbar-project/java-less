package org.uqbar.thin.javaless

import scala.language.implicitConversions
import scala.reflect.runtime.universe
import scala.util.Success

import org.uqbar.thin.encoding.combinator._
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

class JavalessEncoderTest extends FreeSpec with EncoderTest[EncoderDefinition] with EncoderDefinition {

	val terminals = DefaultTerminals
	val preferences = DefaultPreferences

		"Javaless encode of" - {
	
			"classes" - {
				"should succeed" - {
					"for a class" in {
						implicit val encoder = classDefinition 
						
						val emptyClass = Class("MyClass", Nil)
						emptyClass should beEncodedTo("class MyClass {}")(emptyClass -> 0.until(16))
						
						val emptyMethod = Method("calculate", Nil, Nil)
						val nonEmptyClass = Class("MyClass", List(emptyMethod))
						nonEmptyClass should beEncodedTo("class MyClass {public calculate(){}}")(nonEmptyClass -> 0.until(36), emptyMethod -> 15.until(35))
					}
			
					"for a method" in {
						implicit val encoder = methodDefinition
						
						val argumentlessEmptyMethod = Method("calculate", Nil, Nil)
						argumentlessEmptyMethod should beEncodedTo("public calculate(){}")(argumentlessEmptyMethod -> 0.until(20))

						val arg1 = Argument("void", "arg1")
						val arg2 = Argument("void", "arg2")
						val argumentedEmptyMethod = Method("calculate", arg1 :: arg2 :: Nil, Nil)
						argumentedEmptyMethod should beEncodedTo("public calculate(void arg1, void arg2){}")(argumentedEmptyMethod -> 0.until(40), arg1 -> 17.until(26), arg2 -> 28.until(37))

					}

				}
			}
	
		}

}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// ENCODER TEST
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

trait EncoderTest[E <: Encoders] extends Matchers {

	case class beEncodedTo[T](expectedText: String)(expectedReferences: (Any, Range)*)(implicit encoder: E#Encoder[T]) extends Matcher[T] {
		def apply(target: T) = {
			val result = encoder(if(target.isInstanceOf[List[_]]) EncoderResult(target.asInstanceOf[List[_]] : _*) else EncoderResult(target))
			val success = result match {
				case Success((`expectedText`, references, _)) => references.size == expectedReferences.size && expectedReferences.forall{ case (key, value) => references.get(key) == Some(value) }
				case _ => false
			}

			MatchResult(
				success,
				s"Encoded $result did not match Success($expectedText, $expectedReferences, _)}",
				s"Encoded $result matched Success($expectedText, $expectedReferences, _)}"
			)
		}
	}

	case class beEncoded(implicit encoder: E#Encoder[_]) extends Matcher[T] {
		def apply(target: T) = {
			val result = encoder(EncoderResult(target))

			MatchResult(
				result.isSuccess,
				s"Encode was not a success: $result",
				s"Encode was a success: $result"
			)
		}
	}
}