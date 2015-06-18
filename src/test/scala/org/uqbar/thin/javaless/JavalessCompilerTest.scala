package org.uqbar.thin.javaless

import java.io.File

import scala.language.implicitConversions

import org.scalatest.BeforeAndAfterAll
import org.scalatest.Finders
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.uqbar.voodoo.BytecodeClassLoader

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
				importedClass.getDeclaredMethod("foo") should have ('name("foo"))
				importedClass.getDeclaredMethod("bar") should have ('name("bar"))
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

				val importedFooMethod = importedClass.getDeclaredMethod("foo", classOf[Object])
				importedFooMethod should have ('name("foo"))
				importedFooMethod.getParameters should have length 1

				val importedBarMethod = importedClass.getDeclaredMethod("bar", classOf[Object], classOf[Object])
				importedBarMethod should have ('name("bar"))
				importedBarMethod.getParameters should have length 2
			}
			
			"for a class with methods with int literals as bodies" in ???

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