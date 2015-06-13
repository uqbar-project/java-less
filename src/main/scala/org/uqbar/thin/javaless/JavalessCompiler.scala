package org.uqbar.thin.javaless

import org.uqbar.voodoo.writer.ClassWriter
import org.uqbar.voodoo.model._
import org.uqbar.voodoo.mutator._

object JavalessCompiler extends JavalessCompiler
trait JavalessCompiler {
	def compile(program: Program,targetPath: String) = 
    for(classDefinition <- program.classes)
    	ClassWriter.writeClass($(classDefinition.name) let {it => }, targetPath) 
}