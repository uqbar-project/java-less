package org.uqbar.thin.javaless

import scala.language.implicitConversions
import scala.collection.JavaConversions._
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import java.util.IdentityHashMap

class JavalessEncoderTest extends FreeSpec with Matchers with EncoderDefinition {

	val terminals = DefaultTerminals

	//	"Javaless encode of" - {
	//
	//		"classes" - {
	//			"should succeed" - {
	//				"for an empty class" in {
	//					val target = Class("MyClass", Nil)
	//					encode(classDefinition)(target) shouldBe a[Success]
	//					encode(classDefinition)(target).asInstanceOf[Success].text shouldBe ("class MyClass {}")
	//				}
	//
	//				"for a simple empty method" in {
	//					val method1 = Method("calculate", Nil, Nil)
	//					val target = Class("MyClass", List(method1))
	//
	//					encode(classDefinition)(target) shouldBe a[Success]
	//					encode(method)(method1).asInstanceOf[Success].text shouldBe ("public calculate(){}")
	//					encode(classDefinition)(target).asInstanceOf[Success].text shouldBe ("class MyClass {public calculate(){}}")
	//				}
	//
	//				"for a simple method with arguments" in {
	//					val arg1 = Argument("void", "arg1")
	//					val arg2 = Argument("void", "arg2")
	//					val method1 = Method("calculate", List(arg1, arg2), List())
	//					val target = Class("MyClass", List(method1))
	//
	//					encode(argument)(arg1).asInstanceOf[Success].text shouldBe ("void arg1")
	//					encode(argument)(arg2).asInstanceOf[Success].text shouldBe ("void arg2")
	//					encode(method)(method1).asInstanceOf[Success].text shouldBe ("public calculate(void arg1, void arg2){}")
	//					encode(classDefinition)(target).asInstanceOf[Success].text shouldBe ("class MyClass {public calculate(void arg1, void arg2){}}")
	//				}
	//
	//			}
	//		}
	//
	//	}

}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// ENCODER TEST
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

trait EncoderTest[E <: Encoders] extends Matchers {

	case class beEncodedTo[T](expectedText: String)(expectedReferences: (T, Range)*)(implicit encoder: E#Encoder[T]) extends Matcher[T] {
		def apply(target: T) = {
			val result = encoder(Success(pending = target :: Nil))
			val success = result match {
				case Success(`expectedText`, references, _) => references.size == expectedReferences.size && expectedReferences.forall{ case (key, value) => references.get(key) == value }
				case _ => false
			}

			MatchResult(
				success,
				s"Encoded $result did not match Success($expectedText, $expectedReferences, Nil)}",
				s"Encoded $result matched Success($expectedText, $expectedReferences, Nil)}"
			)
		}
	}

	case class beEncoded(implicit encoder: E#Encoder[_]) extends Matcher[T] {
		def apply(target: T) = {
			val result = encoder(Success(pending = target :: Nil))

			MatchResult(
				result.isInstanceOf[Success],
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
	lazy val w: Encoder[W] = "W(" ~ t.* ~ ")"
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

			foo(Success()) should resultIn("Foo")()
			(foo ~ foo : Encoder[_])(Success()) should resultIn("FooFoo")()
			(foo ~ foo ~ foo : Encoder[_])(Success()) should resultIn("FooFooFoo")()
			 
			x(Success(pending = anX :: Nil)) should resultIn("X:foo")(anX -> 0.until(5))
			y(Success(pending = aY :: Nil)) should resultIn("Y:bar:5")(aY -> 0.until(7))

			
			(foo ~ x : Encoder[_])(Success(pending = anX :: Nil)) should resultIn("FooX:foo")(anX -> 3.until(8))

			(x ~ foo : Encoder[_])(Success(pending = anX :: Nil)) should resultIn("X:fooFoo")(anX -> 0.until(5))

			(x ~ y : Encoder[_])(Success(pending = anX :: aY :: Nil)) should resultIn("X:fooY:bar:5")(anX -> 0.until(5), aY -> 5.until(12))

			q(Success(pending = aQ :: Nil)) should resultIn("Q(X:fooY:bar:5)")(aQ -> 0.until(15), anX -> 2.until(7), aY -> 7.until(14))

			(x | y : Encoder[_])(Success(pending = anX :: Nil)) should resultIn("X:foo")(anX -> 0.until(5))
			(y | x : Encoder[_])(Success(pending = anX :: Nil)) should resultIn("X:foo")(anX -> 0.until(5))
			(x | y : Encoder[_])(Success(pending = aY :: Nil)) should resultIn("Y:bar:5")(aY -> 0.until(7))
			(y | x : Encoder[_])(Success(pending = aY :: Nil)) should resultIn("Y:bar:5")(aY -> 0.until(7))

			z(Success(pending = aZ :: Nil)) should resultIn("Z(X:foo)")(aZ -> 0.until(8), anX -> 2.until(7))
			z(Success(pending = anotherZ :: Nil)) should resultIn("Z(Y:bar:5)")(anotherZ -> 0.until(10), aY -> 2.until(9))
			
			(t.*)(Success(pending = List(anX,aY) :: Nil)) should resultIn("X:fooY:bar:5")(anX -> 0.until(5), aY -> 5.until(12))
			(t *~ "|")(Success(pending = List(anX,aY) :: Nil)) should resultIn("X:foo|Y:bar:5")(anX -> 0.until(5), aY -> 6.until(13))
			
			w(Success(pending = aW :: Nil)) should resultIn("W(X:fooY:bar:5)")(aW -> 0.until(15), anX -> 2.until(7), aY -> 7.until(14))
			
			
		}

		case class resultIn(expectedText: String)(expectedReferences: (Any, Range)*) extends Matcher[EncoderResult] {
			def apply(target: EncoderResult) = target match {
					case failure: Failure => MatchResult(false, s"Encoder failed: $failure", s"")
					case Success(text, _, _) if text != expectedText => MatchResult(false, s"Encoded text: $text did not match expected: $expectedText", s"")
					case Success(_, references, _) if references.size != expectedReferences.size || expectedReferences.exists{ case (key, value) => references.get(key) != value } => MatchResult(false, s"Encoded references: $references did not match expected: $expectedReferences", "")
					case Success(_, _, _::_) => MatchResult(false, s"Non empty pending", s"")
					case _ => MatchResult(true, s"", s"Encoded should not have result in Success($expectedText,$expectedReferences), but did")
			}
		}

	}

}