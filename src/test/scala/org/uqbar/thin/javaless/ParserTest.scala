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
					"class MyClass { }" should beParsedTo (Class("MyClass", Nil))
				}
			}
			
		}
	
	}

}