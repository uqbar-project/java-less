package org.uqbar.thin.javaless

import scala.language.implicitConversions
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import java.util.IdentityHashMap
import org.uqbar.voodoo.BytecodeClassLoader
import java.io.File
import org.scalatest.BeforeAndAfterAll

class JavalessCompilerTest extends FreeSpec with Matchers with BeforeAndAfterAll with JavalessCompiler {

	val TEMP_PATH = new File(s"${getClass.getProtectionDomain.getCodeSource.getLocation.getFile}GENERATED")
	val CLASS_NAME = "MyClass"
	val TARGET_FILE = s"$TEMP_PATH${File.separator}$CLASS_NAME.class"

	override def beforeAll {
		TEMP_PATH.mkdir
	}

	override def afterAll {
		TEMP_PATH.listFiles.foreach{ _.delete }
		TEMP_PATH.delete
	}

	"Javaless compilation" - {

		"should succeed" - {

			"for an empty class" in {
				val target = Class("MyClass", Nil)
				
				val classLoader = new BytecodeClassLoader
				compile(Program(target :: Nil), TARGET_FILE)
				val importedClass = classLoader.importClass(TARGET_FILE)

				importedClass.getName should be(target.name)
				importedClass.getDeclaredMethods should have length 0
			}

			"for a class with empty methods with no parameters" in {
				val target = Class("MyClass", List(
					Method("foo", Nil, Nil),
					Method("bar", Nil, Nil)
				))
				
				val classLoader = new BytecodeClassLoader
				compile(Program(target :: Nil), TARGET_FILE)
				val importedClass = classLoader.importClass(TARGET_FILE)

				importedClass.getName should be(target.name)
				importedClass.getDeclaredMethods should have length 2
				importedClass.getDeclaredMethods()(0) should have ('name("foo"))
				importedClass.getDeclaredMethods()(1) should have ('name("bar"))
			}

			"for a class with empty methods with parameters" in {
				val target = Class("MyClass", List(
					Method("foo", "par1" :: Nil, Nil),
					Method("bar", "par1" :: "par2" :: Nil, Nil)
				))
				
				val classLoader = new BytecodeClassLoader
				compile(Program(target :: Nil), TARGET_FILE)
				val importedClass = classLoader.importClass(TARGET_FILE)

				importedClass.getName should be(target.name)

				importedClass.getDeclaredMethods should have length 2

				val importedFooMethod = importedClass.getDeclaredMethod("foo",classOf[Object])
				importedFooMethod should have ('name("foo"))
				importedFooMethod.getParameters should have length 1

				val importedBarMethod = importedClass.getDeclaredMethod("foo",classOf[Object])
				importedClass.getDeclaredMethods()(1) should have ('name("bar"))
				importedClass.getDeclaredMethods()(1).getParameters() should have length 2
			}

			"for a class with fields" in {
				val target = Class("MyClass", List(
					Field("foo"),
					Field("bar")
				))
				
				val classLoader = new BytecodeClassLoader
				compile(Program(target :: Nil), TARGET_FILE)
				val importedClass = classLoader.importClass(TARGET_FILE)

				importedClass.getName should be(target.name)
				importedClass.getDeclaredFields should have length 2
				importedClass.getDeclaredField("foo") should have ('name("foo"))
				importedClass.getDeclaredField("bar") should have ('name("bar"))
			}

		}
	}

}