package org.uqbar.thin.javaless

import org.uqbar.voodoo.writer.ClassWriter
import org.uqbar.voodoo.Globals.byteSize
import org.uqbar.voodoo.model._
import org.uqbar.voodoo.mutator._

object JavalessCompiler extends JavalessCompiler
trait JavalessCompiler {
	def compile(program: Program, targetPath: String) =
		for (classDefinition <- program.classes) {
			val javaClass = $(classDefinition.name) let { it =>
				classDefinition.body.foreach {
					case Method(name, arguments, _) =>
						it += (name :: MethodType($[Unit], arguments.map(_ => $[Object]): _*))(
							RETURN
						)
					case Field(name) =>
						it += name :: $[Object]
				}
			}

			ClassWriter.writeClass(javaClass, targetPath)
		}
}