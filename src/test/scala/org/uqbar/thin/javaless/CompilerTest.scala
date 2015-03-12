package org.uqbar.thin.javaless

import scala.language.implicitConversions
import scala.collection.JavaConversions._
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import java.util.IdentityHashMap
import org.uqbar.voodoo.BytecodeClassLoader
import java.io.File
import org.scalatest.BeforeAndAfterAll

class JavalessCompilerTest extends FreeSpec with Matchers  with BeforeAndAfterAll  with Compiler {

	val TEMP_PATH = new File(s"${getClass.getProtectionDomain.getCodeSource.getLocation.getFile}GENERATED")
	
	override def beforeAll {
		TEMP_PATH.mkdir
	}

	override def afterAll {
		TEMP_PATH.listFiles.foreach{ _.delete } 
		TEMP_PATH.delete
	}
	
	"Javaless compilation of" - {
		"classes" - {
			"should succeed" - {
				"for an empty class" in {
					val target = Class("MyClass", Nil)
					val targetFile = s"$TEMP_PATH${File.separator}${target.name.value}.class"
					val classLoader = new BytecodeClassLoader
					compile(Program(List(target)), targetFile)
					val importedClass = classLoader.importClass(targetFile)
					
					importedClass.getName should be(target.name.value)
				}
			}
		}

	}

}