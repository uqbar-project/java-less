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

	"Javaless encode of" - {

		"classes" - {
			"should succeed" - {
				"for an empty class" in {
					val target = Class("MyClass", Nil)
					encode(classDefinition)(target) shouldBe a [Success]
					encode(classDefinition)(target).asInstanceOf[Success].text shouldBe ("class MyClass {}")
				}
			}
		}

	}

}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// ENCODER TEST
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

//trait EncoderTest extends Matchers {
//
//	protected def encode(target: SyntaxElement): EncoderResult
//
//	case class beEncodedTo(expectedText: String, expectedReferences: (SyntaxElement, Range)*) extends Matcher[SyntaxElement] {
//		def apply(target: SyntaxElement) = {
//			val result = encode(target)
//			val success = result match {
//				//				case Success(`expectedText`, references) => references.size == expectedReferences.size && expectedReferences.forall{ case (key, value) => references.get(key) == value }
//				case _ => false
//			}
//
//			MatchResult(
//				success,
//				s"Encoded $result did not match Success($expectedText, $expectedReferences)}",
//				s"Encoded $result matched Success($expectedText, $expectedReferences)}"
//			)
//		}
//	}
//}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// ENCODER FRAMEWORK TEST
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

trait T
case class X(s: String) extends T
case class Y(s: String,n: Int) extends T
case class Q(x: X, y: Y)
case class Z(t: T)

class EncodersTest extends FreeSpec with Matchers with Encoders {

	"Encoders" - {
		"Constant encoders" in {
			val base = Success()

			val e: Encoder[Any] = "Foo"
			e(base) should be (Success("Foo"))

			val encoder: Encoder[_] = "Foo" ~ "Bar"
			encoder(base) should be (Success("FooBar"))
		}

		"Access encoders" in {
			lazy val baseX = Success(pending = X("foo") :: Nil )
			lazy val baseY = Success(pending = Y("bar",5) :: Nil )
			lazy val baseQ = Success(pending = Q(X("foo"),Y("bar",5))  :: Nil)

			lazy val x = "X:" ~ __ ^^ [X]()
			x(baseX) should be (Success("X:foo"))  
			y(baseX) shouldBe a [Failure]

			
			lazy val y = "Y[" ~ __ ~ "|" ~ __ ~ "]" ^^[Y]()
			y(baseY) should be (Success("Y[bar|5]"))
			x(baseY) shouldBe a [Failure]  
			
			lazy val q = "@{" ~ x ~ "," ~ y ~ "}" ^^[Q]()
			q(baseQ) should be (Success("@{X:foo,Y[bar|5]}"))
		}
		
		"Or encoders" in {
			
			lazy val baseX = Success(pending = Z(X("foo")) :: Nil )
			lazy val baseY = Success(pending = Z(Y("bar",5))  :: Nil)
			
			lazy val x = "X:" ~ __ ^^[X]()    
			lazy val y = "Y:" ~ __ ~ ":" ~ __ ^^[Y]()
			lazy val z = "Z[" ~ (x | y) ~ "]" ^^[Z]()
 
			z(baseX) should be (Success("Z[X:foo]"))
			z(baseY) should be (Success("Z[Y:bar:5]"))			
		}

	}

}