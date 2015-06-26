package org.uqbar.thin.javaless

import scala.language.implicitConversions

import org.scalatest.FreeSpec
import org.uqbar.thin.encoding.combinator.EncoderMatchers

class JavalessEncoderTest extends FreeSpec with JavalessEncoderDefinition with EncoderMatchers {

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
							calculate() {}
						}
					""")(nonEmptyClass -> 0.to(32), nonEmptyClass.name -> 6.to(12), nonEmptyClass.body -> 17.to(30), emptyMethod -> 17.to(30), emptyMethod.name -> 17.to(25), Nil -> 26.to(27))
        }

        "with two method definitions" in {
          val emptyMethod1 = Method("calculate", Nil, Nil)
          val emptyMethod2 = Method("recalculate", Nil, Nil)
          val nonEmptyClass = Class("MyClass", List(emptyMethod1, emptyMethod2))

          nonEmptyClass should beEncodedTo("""
						class MyClass {
							calculate() {}

							recalculate() {}
						}
					""")(nonEmptyClass -> 0.to(51), nonEmptyClass.name -> 6.to(12), nonEmptyClass.body -> 17.to(49), emptyMethod1 -> 17.to(30), emptyMethod1.name -> 17.to(25), emptyMethod2 -> 34.to(49), emptyMethod2.name -> 34.to(44), Nil -> 26.to(27))
        }

        "with a field definition" in {
          val field = Field("foo")
          val nonEmptyClass = Class("MyClass", List(field))

          nonEmptyClass should beEncodedTo("""
						class MyClass {
							foo
						}
					""")(nonEmptyClass -> 0.to(21), nonEmptyClass.name -> 6.to(12), nonEmptyClass.body -> 17.to(19), field -> 17.to(19), field.name -> 17.to(19))
        }

        "with three field definition" in {
          val field1 = Field("foo")
          val field2 = Field("bar")
          val field3 = Field("meh")
          val nonEmptyClass = Class("MyClass", List(field1, field2, field3))

          nonEmptyClass should beEncodedTo("""
						class MyClass {
							foo

							bar

							meh
						}
					""")(nonEmptyClass -> 0.to(33), nonEmptyClass.name -> 6.to(12), nonEmptyClass.body -> 17.to(31), field1 -> 17.to(19), field1.name -> 17.to(19), field2 -> 23.to(25), field2.name -> 23.to(25), field3 -> 29.to(31), field3.name -> 29.to(31))
        }

        "with fields and methods" in {
          val field1 = Field("foo")
          val field2 = Field("bar")
          val field3 = Field("meh")
          val emptyMethod1 = Method("calculate", Nil, Nil)
          val emptyMethod2 = Method("recalculate", Nil, Nil)
          val nonEmptyClass = Class("MyClass", List(field1, field2, field3, emptyMethod1, emptyMethod2))

          nonEmptyClass should beEncodedTo("""
						class MyClass {
							foo

							bar

							meh

							calculate() {}

							recalculate() {}
						}
					""")(nonEmptyClass -> 0.to(69), nonEmptyClass.name -> 6.to(12), nonEmptyClass.body -> 17.to(67), field1 -> 17.to(19), field1.name -> 17.to(19), field2 -> 23.to(25), field2.name -> 23.to(25), field3 -> 29.to(31), field3.name -> 29.to(31), emptyMethod1 -> 35.to(48), emptyMethod1.name -> 35.to(43), emptyMethod2 -> 52.to(67), emptyMethod2.name -> 52.to(62), Nil -> 44.to(45))
        }
      }

      "for methods" - {
        implicit val encoder = methodDefinition

        "with no arguments or body" in {
          val argumentlessEmptyMethod = Method("calculate", Nil, Nil)

          argumentlessEmptyMethod should beEncodedTo("calculate() {}")(argumentlessEmptyMethod -> 0.to(13), argumentlessEmptyMethod.name -> 0.to(8), argumentlessEmptyMethod.arguments -> 9.to(10))
        }

        "with one argument but no body" in {
          val argumentedEmptyMethod = Method("calculate", "arg" :: Nil, Nil)

          argumentedEmptyMethod should beEncodedTo("calculate(arg) {}")(argumentedEmptyMethod -> 0.to(16), argumentedEmptyMethod.name -> 0.to(8), argumentedEmptyMethod.arguments -> 9.to(13), "arg" -> 10.to(12), argumentedEmptyMethod.body -> 16.to(16))
        }

        "with arguments but no body" in {
          val argumentedEmptyMethod = Method("calculate", "arg1" :: "arg2" :: Nil, Nil)

          argumentedEmptyMethod should beEncodedTo("calculate(arg1, arg2) {}")(argumentedEmptyMethod -> 0.to(23), argumentedEmptyMethod.name -> 0.to(8), argumentedEmptyMethod.arguments -> 9.to(20), "arg1" -> 10.to(13), "arg2" -> 16.to(19), argumentedEmptyMethod.body -> 23.to(23))
        }

        "with string literals as body" in {
          val literalString1 = StringLiteral("\"\"\"This is a dobBlock\"\"\"")
          val argumentedLiteralStringMethod = Method("calculate", "xarg" :: Nil, List(literalString1))
          argumentedLiteralStringMethod should beEncodedTo("calculate(xarg) {\"\"\"This is a dobBlock\"\"\"}")(argumentedLiteralStringMethod -> 0.to(41), argumentedLiteralStringMethod.name -> 0.to(8), argumentedLiteralStringMethod.arguments -> 9.to(14), "xarg" -> 10.to(13), argumentedLiteralStringMethod.body -> 17.to(40), literalString1 -> 17.to(40), literalString1.value -> 17.to(40))
        }
      }

      "for sentences" - {

        "for expressions" - {

          "for literals" - {

            "for string literals" - {
              implicit val encoder = stringLiteral
              
              "for empty string" in {
               val literalString1 = StringLiteral("\"\"\"\"\"\"")
               literalString1 should beEncodedTo("\"\"\"\"\"\"")(literalString1 -> 0.to(5), literalString1.value-> 0.to(5))
              }

              "for non empty string" in {
                val literalString1 = StringLiteral("\"\"\"This is a docBlock\"\"\"")
                literalString1 should beEncodedTo("\"\"\"This is a docBlock\"\"\"")(literalString1 -> 0.to(23), literalString1.value-> 0.to(23))
              }
              "for non empty string containing the string delimiter literal" in {
                val literalString = StringLiteral("\"\"\"\'Hello World\'\"\"\"")
                literalString should beEncodedTo("\"\"\"\'Hello World\'\"\"\"")(literalString -> 0.to(18), literalString.value-> 0.to(18))
              }
              
              "for multiline string literal " in 
              {
                val literalString = StringLiteral("\"\"\"Hello\n World\n!\"\"\"")
                literalString should beEncodedTo("\"\"\"Hello\n World\n!\"\"\"")(literalString -> 0.to(19), literalString.value-> 0.to(19))
              }
            }

          }

        }

      }

      "for fields" - {
        implicit val encoder = fieldDefinition

        "with a valid name" in {
          val field = Field("foo")

          field should beEncodedTo("foo")(field -> 0.to(2), field.name -> 0.to(2))
        }

      }

    }

  }
}