package org.uqbar.thin.javaless

import org.uqbar.voodoo.writer.ClassWriter
import org.uqbar.voodoo.model._
import org.uqbar.voodoo.mutator._

object Compiler extends Compiler
trait Compiler {
	def compile(program: Program,targetPath: String) = for(classDefinition <- program.definitions)
		ClassWriter.writeClass($(classDefinition.name) let {it => }, targetPath) 
}