package org.uqbar.thin.javaless

import scala.language.implicitConversions
import scala.language.reflectiveCalls
import org.scalatest.Finders
import org.scalatest.FreeSpec
import org.uqbar.thin.encoding.combinator.EncoderMatchers
import org.uqbar.thin.encoding.combinator.Order

class JavalessEncoderTest extends FreeSpec with JavalessEncoderDefinition with EncoderMatchers {

  implicit val terminals = DefaultTerminals
  implicit val preferences = DefaultPreferences

  "Javaless encoding" - {

    "should succeed" - {

      "for classes" - {
        implicit val encoder = classDefinition

        "with no members" in {
          val emptyClass = Class("MyClass", Nil)

          emptyClass should beEncodedTo"""
						${0}class ${1}MyClass${2} {${3}}${4}
					"""(emptyClass -> 0.until(4), emptyClass.name -> 1.until(2), emptyClass.body -> 3.until(3))
        }

        "with a method definition" in {
          val emptyMethod = Method("calculate", Nil, Nil)
          val nonEmptyClass = Class("MyClass", List(emptyMethod))

          nonEmptyClass should beEncodedTo"""
						${0}class ${1}MyClass${2} {
							${3}calculate${4}(${5}) {}${6}
						}${7}
					"""(nonEmptyClass -> 0.until(7), nonEmptyClass.name -> 1.until(2), nonEmptyClass.body -> 3.until(6), emptyMethod -> 3.until(6), emptyMethod.name -> 3.until(4), Nil -> 4.to(5))
        }

        "with two method definitions" in {
          val emptyMethod1 = Method("calculate", Nil, Nil)
          val emptyMethod2 = Method("recalculate", Nil, Nil)
          val nonEmptyClass = Class("MyClass", List(emptyMethod1, emptyMethod2))

          nonEmptyClass should beEncodedTo"""
						${0}class ${1}MyClass${2} {
							${3}calculate${4}(${5}) {}${6}

							${7}recalculate${8}(${9}) {}${10}
						}${11}
					"""(nonEmptyClass -> 0.until(11), nonEmptyClass.name -> 1.until(2), nonEmptyClass.body -> 3.until(10), emptyMethod1 -> 3.until(6), emptyMethod1.name -> 3.until(4), emptyMethod2 -> 7.until(10), emptyMethod2.name -> 7.until(8), Nil -> 4.to(5))
        }

        "with a field definition" in {
          val field = Field("foo")
          val nonEmptyClass = Class("MyClass", List(field))

          nonEmptyClass should beEncodedTo"""
						${0}class ${1}MyClass${2} {
							${3}foo${4}
						}${5}
					"""(nonEmptyClass -> 0.until(5), nonEmptyClass.name -> 1.until(2), nonEmptyClass.body -> 3.until(4), field -> 3.until(4), field.name -> 3.until(4))
        }

        "with three field definition" in {
          val field1 = Field("foo")
          val field2 = Field("bar")
          val field3 = Field("meh")
          val nonEmptyClass = Class("MyClass", List(field1, field2, field3))

          nonEmptyClass should beEncodedTo"""
						${0}class ${1}MyClass${2} {
							${3}foo${4}	${5}bar${6}	${7}meh${8}
						}${9}
					"""(nonEmptyClass -> 0.until(9), nonEmptyClass.name -> 1.until(2), nonEmptyClass.body -> 3.until(8), field1 -> 3.until(4), field1.name -> 3.until(4), field2 -> 5.until(6), field2.name -> 5.until(6), field3 -> 7.until(8), field3.name -> 7.until(8))
        }

        "with fields and methods" in {
          val field1 = Field("foo")
          val field2 = Field("bar")
          val field3 = Field("meh")
          val emptyMethod1 = Method("calculate", Nil, Nil)
          val emptyMethod2 = Method("recalculate", Nil, Nil)
          val nonEmptyClass = Class("MyClass", List(field1, field2, field3, emptyMethod1, emptyMethod2))

          nonEmptyClass should beEncodedTo"""
						${0}class ${1}MyClass${2} {
							${3}foo${4}	${5}bar${6}	${7}meh${8}

							${9}calculate${10}(${11}) {}${12}

							${13}recalculate${14}() {}${15}
						}${16}
					"""(nonEmptyClass -> 0.until(16), nonEmptyClass.name -> 1.until(2), nonEmptyClass.body -> 3.until(15), field1 -> 3.until(4), field1.name -> 3.until(4), field2 -> 5.until(6), field2.name -> 5.until(6), field3 -> 7.until(8), field3.name -> 7.until(8), emptyMethod1 -> 9.until(12), emptyMethod1.name -> 9.until(10), Nil -> 10.to(11), emptyMethod2 -> 13.until(15), emptyMethod2.name -> 13.until(14))
        }
      }

      "for methods" - {
        implicit val encoder = methodDefinition

        "with no arguments or body" in {
          val argumentlessEmptyMethod = Method("calculate", Nil, Nil)

          argumentlessEmptyMethod should beEncodedTo"""
						${0}calculate${1}()${2} {}${3}
					"""(argumentlessEmptyMethod -> 0.until(3), argumentlessEmptyMethod.name -> 0.until(1), argumentlessEmptyMethod.arguments -> 1.until(2))
        }

        "with one argument but no body" in {
          val argumentedEmptyMethod = Method("calculate", "arg" :: Nil, Nil)

          argumentedEmptyMethod should beEncodedTo"""
            ${0}calculate${1}(${2}arg${3}) {}${4}
          """(argumentedEmptyMethod -> 0.until(4), argumentedEmptyMethod.body -> 4.until(4), argumentedEmptyMethod.name -> 0.until(1), argumentedEmptyMethod.arguments -> 1.to(3), "arg" -> 2.until(3))
        }

        "with arguments but no body" in {
          val argumentedEmptyMethod = Method("calculate", "arg1" :: "arg2" :: Nil, Nil)

          argumentedEmptyMethod should beEncodedTo"""
            ${0}calculate${1}(${2}arg1${3}, ${4}arg2${5}) {}${6}
          """(argumentedEmptyMethod -> 0.until(6), argumentedEmptyMethod.body -> 6.until(6), argumentedEmptyMethod.name -> 0.until(1), argumentedEmptyMethod.arguments -> 1.to(5), "arg1" -> 2.until(3), "arg2" -> 4.until(5))
        }

        "with string literals as body" in {
          val literalString1 = StringLiteral("\"This is a dobBlock\"")
          val argumentedLiteralStringMethod = Method("calculate", "xarg" :: Nil, List(literalString1))
          argumentedLiteralStringMethod should beEncodedTo("calculate(xarg) {\"This is a dobBlock\"}")(argumentedLiteralStringMethod -> 0.to(37), argumentedLiteralStringMethod.name -> 0.to(8), argumentedLiteralStringMethod.arguments -> 9.to(14), "xarg" -> 10.to(13), argumentedLiteralStringMethod.body -> 17.to(36), literalString1 -> 17.to(36), literalString1.value -> 17.until(37))
        }

      }

      "for fields" - {
        implicit val encoder = fieldDefinition

        "with a valid name" in {
          val field = Field("foo")

          field should beEncodedTo"""
						${0}foo${1}
					"""(field -> 0.until(1), field.name -> 0.until(1))
        }

      }

    }

  }

  "for sentences" - {

    "for expressions" - {

      "for literals" - {

        "for string literals" - {
          implicit val encoder = stringLiteral

          "for empty string" in {
            val literalString1 = StringLiteral("\"\"")
            literalString1 should beEncodedTo("\"\"")(literalString1 -> 0.to(1), literalString1.value -> 0.to(1))
          }

          "for non empty string" in {
            val literalString1 = StringLiteral("\"This is a docBlock\"")
            literalString1 should beEncodedTo("\"This is a docBlock\"")(literalString1 -> 0.to(19), literalString1.value -> 0.to(19))
          }
          "for non empty string containing the string delimiter literal" in {
            val literalString = StringLiteral("\"\'Hello World\'\"")
            literalString should beEncodedTo("\"\'Hello World\'\"")(literalString -> 0.to(14), literalString.value -> 0.to(14))
          }

        }

      }

    }

  }

  "preferences" - {
    "fields should be groupable before methods" in {
      implicit val preferences = DefaultPreferences.copy(sortOrders = Set(Order(classMember.*) { (a, b) => a.isInstanceOf[Field] && b.isInstanceOf[Method] }))
      implicit val encoder = classDefinition

      val field1 = Field("foo")
      val field2 = Field("bar")
      val field3 = Field("meh")
      val emptyMethod1 = Method("calculate", Nil, Nil)
      val emptyMethod2 = Method("recalculate", Nil, Nil)
      val nonEmptyClass = Class("MyClass", List(field1, emptyMethod1, field2, emptyMethod2, field3))

      nonEmptyClass should beEncodedTo"""
				${0}class ${1}MyClass${2} {
					${3}foo${4}	${5}bar${6}	${7}meh${8}

					${9}calculate${10}(${11}) {}${12}

					${13}recalculate${14}() {}${15}
				}${16}
			"""(nonEmptyClass -> 0.until(16), nonEmptyClass.name -> 1.until(2), nonEmptyClass.body -> 3.until(15), field1 -> 3.until(4), field1.name -> 3.until(4), field2 -> 5.until(6), field2.name -> 5.until(6), field3 -> 7.until(8), field3.name -> 7.until(8), emptyMethod1 -> 9.until(12), emptyMethod1.name -> 9.until(10), Nil -> 10.to(11), emptyMethod2 -> 13.until(15), emptyMethod2.name -> 13.until(14))
    }
  }
}