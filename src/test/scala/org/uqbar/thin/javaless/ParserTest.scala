package org.uqbar.thin.javaless

import org.scalatest.FreeSpec

class JavalessParserTest extends FreeSpec with ParserTest {
	
	val parserDefinition = new Parser().Definition
	
	"Javaless parse of" - {
		
		"classes" - { 
			
			implicit val parser = parserDefinition.classDefinition

			"should succeed" - {
				"for an empty class" in {
					"class MyClass { }" should beParsedTo (Class("MyClass", Nil))
				}
			}
			
		}
	
	}

}