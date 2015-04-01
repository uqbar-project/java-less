package org.uqbar.thin.javaless

import scala.language.implicitConversions
import scala.reflect.runtime.universe
import scala.util.Success

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

class JavalessEncoderTest extends FreeSpec with EncoderTest[EncoderDefinition] with EncoderDefinition {

	val terminals = DefaultTerminals

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

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// ENCODER FRAMEWORK TEST
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

trait T
case class X(s: String) extends T
case class Y(s: String, n: Int) extends T
case class Q(x: X, y: Y)
case class Z(t: T)
case class W(ts: List[T])

trait EncoderExample extends Encoders {
	lazy val foo: Encoder[Any] = "Foo"
	lazy val xStripped: Encoder[X] = __
	lazy val x: Encoder[X] = "X:" ~ __
	lazy val y: Encoder[Y] = "Y:" ~ __ ~ ":" ~ __
	lazy val q: Encoder[Q] = "Q(" ~ x ~ y ~ ")"
	lazy val t = x|y
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

	"Encoders" - {

		"Access encoders" in {

			foo(EncoderResult()) should resultIn("Foo")()
			(foo ~ foo : Encoder[_])(EncoderResult()) should resultIn("FooFoo")()
			(foo ~ foo ~ foo : Encoder[_])(EncoderResult()) should resultIn("FooFooFoo")()

			__(EncoderResult(1)) should resultIn("1")()
			 
			x(EncoderResult(anX)) should resultIn("X:foo")(anX -> 0.until(5))
			y(EncoderResult(aY)) should resultIn("Y:bar:5")(aY -> 0.until(7))

			
			(foo ~ x : Encoder[_])(EncoderResult(anX)) should resultIn("FooX:foo")(anX -> 3.until(8))

			(x ~ foo : Encoder[_])(EncoderResult(anX)) should resultIn("X:fooFoo")(anX -> 0.until(5))

			(x ~ y : Encoder[_])(EncoderResult(anX, aY)) should resultIn("X:fooY:bar:5")(anX -> 0.until(5), aY -> 5.until(12))

			q(EncoderResult(aQ)) should resultIn("Q(X:fooY:bar:5)")(aQ -> 0.until(15), anX -> 2.until(7), aY -> 7.until(14))

			(x | y : Encoder[_])(EncoderResult(anX)) should resultIn("X:foo")(anX -> 0.until(5))
			(y | x : Encoder[_])(EncoderResult(anX)) should resultIn("X:foo")(anX -> 0.until(5))
			(x | y : Encoder[_])(EncoderResult(aY)) should resultIn("Y:bar:5")(aY -> 0.until(7))
			(y | x : Encoder[_])(EncoderResult(aY)) should resultIn("Y:bar:5")(aY -> 0.until(7))

			z(EncoderResult(aZ)) should resultIn("Z(X:foo)")(aZ -> 0.until(8), anX -> 2.until(7))
			z(EncoderResult(anotherZ)) should resultIn("Z(Y:bar:5)")(anotherZ -> 0.until(10), aY -> 2.until(9))
			
			(t.*)(EncoderResult(List(anX,aY))) should resultIn("X:fooY:bar:5")(anX -> 0.until(5), aY -> 5.until(12))
			(t *~ "|")(EncoderResult(List(anX,aY))) should resultIn("X:foo|Y:bar:5")(anX -> 0.until(5), aY -> 6.until(13))
			
			("foo" ~ __.* : Encoder[_])(EncoderResult(Nil)) should resultIn("foo")()
			
			w(EncoderResult(aW)) should resultIn("W(X:foo,Y:bar:5)")(aW -> 0.until(16), anX -> 2.until(7), aY -> 8.until(15))
			
			
		}

		case class resultIn(expectedText: String, expectedPending: List[Any] = Nil)(expectedReferences: (Any, Range)*) extends Matcher[EncoderResult] {
			def apply(target: EncoderResult) = target match {
					case failure if failure.isFailure => MatchResult(false, s"Encoder failed: $failure", s"")
					case Success((text, _, _)) if text != expectedText => MatchResult(false, s"Encoded text: $text did not match expected: $expectedText", s"")
					case Success((_, references, _)) if references.size != expectedReferences.size || expectedReferences.exists{ case (key, value) => references.get(key) != Some(value) } => MatchResult(false, s"Encoded references: $references did not match expected: $expectedReferences", "")
					case Success((_, _, pending )) if pending != expectedPending => MatchResult(false, s"Pending: $pending was not expected: $expectedPending", s"")
					case _ => MatchResult(true, s"", s"Encoded should not have result in Success($expectedText,$expectedReferences), but did")
			}
		}

	}

}