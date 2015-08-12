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
          case Method(name, arguments, sentences) =>
            val builder: MethodBuilder = sentences.foldLeft(new MethodBuilder(name :: MethodType($[Object], arguments.map(_ => $[Object]): _*), List())) { (previousBuilder, sentence) =>
              sentence match {
                case e: StringLiteral => previousBuilder ++ LDC(e.value)
              }
            }
            val buildedMethod = builder.build
            it += (buildedMethod.signature)(buildedMethod.instructions: _*)
          case Field(name) =>
            it += name :: $[Object]
        }
      }

      ClassWriter.writeClass(javaClass, targetPath)
    }
}

case class MethodBuilder(signature: Signature[org.uqbar.voodoo.model.Method], instructions: List[org.uqbar.voodoo.model.Instruction]) {

  def ++(instruccion: org.uqbar.voodoo.model.Instruction) =
    copy(instructions = this.instructions :+ instruccion)

  def build: MethodBuilder =
    this.instructions match {      
      case List()  => copy(instructions = this.instructions :+ ACONST_NULL :+ ARETURN)
      case List(_) => copy(instructions = this.instructions :+ ARETURN)
    }

}


