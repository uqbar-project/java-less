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
            it += (name :: MethodType($[Unit], arguments.map(_ => $[Object]): _*))(
              sentences match {
                case Nil => RETURN
                case elements => elements.map { e =>
                  e match {
                    case e: StringLiteral => LDC(e.value)
                    case _                => ARETURN
                  }
                }
              })
          case Field(name) =>
            it += name :: $[Object]
        }
      }

      ClassWriter.writeClass(javaClass, targetPath)
    }
}