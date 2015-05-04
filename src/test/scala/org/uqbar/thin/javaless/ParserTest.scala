package org.uqbar.thin.javaless

import org.scalatest.FreeSpec
import org.uqbar.testing.ParserTest

class JavalessParserTest extends FreeSpec with ParserTest[ParserDefinition] with ParserDefinition {

  val terminals = DefaultTerminals

  "Javaless parse of" - {

    "classes" - {

      implicit val parser = classDefinition

      "should succeed" - {
        "for an empty class" in {
          "class MyClass { }" should beParsedTo(Class("MyClass", Nil))
        }

        "for a simle method" in {
          "class MyClass { public calculate() {} }" should beParsedTo(Class("MyClass", List(Method("calculate", List(), List()))))
        }

        "for a simle method with arguments" in {
          "class MyClass { public calculate(void arg1, void arg2) {} }" should beParsedTo(Class("MyClass", List(Method("calculate", List(Argument("void", "arg1"), Argument("void", "arg2")), List()))))
        }
      }
    }

  }

}