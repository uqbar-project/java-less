package org.uqbar.thin.javaless

import org.scalatest.FreeSpec
import org.uqbar.testing.ParserTest

class JavalessParserTest extends FreeSpec with ParserTest[JavalessParserDefinition] with JavalessParserDefinition {

  val terminals = DefaultTerminals

  "Javaless parse of" - {

    "classes" - {

      implicit val parser = classDefinition

      "should succeed" - {
        "for an empty class" in {
          "class MyClass { }" should beParsedTo(Class("MyClass", Nil))
        }

        "for a simle method" in {
          "class MyClass { calculate() {} }" should beParsedTo(Class("MyClass", List(Method("calculate", Nil, Nil))))
        }

        "for a simle method with arguments" in {
          "class MyClass { calculate(arg1, arg2) {} }" should beParsedTo(Class("MyClass", List(Method("calculate", List("arg1", "arg2"), Nil))))
        }
      }
    }

  }

}