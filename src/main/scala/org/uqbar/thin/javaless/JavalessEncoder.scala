package org.uqbar.thin.javaless

import scala.language.implicitConversions
import org.uqbar.thin.encoding.combinator._

class JavalessEncoder(_terminals: => Map[Symbol, String] = DefaultTerminals, _preferences: => EncoderPreferences = DefaultPreferences) extends JavalessEncoderDefinition {
	def terminals = _terminals
	def preferences = _preferences
	def apply(input: Program) = encode(program)(input)
}

trait JavalessEncoderDefinition extends Encoders { 
	lazy val program = $[Program] ~> classDefinition.*{_.definitions}
	lazy val classDefinition = $[Class] ~> 'class ~ &{_.name} ~ 'contextOpen ~~(classMember.*{_.body}) ~ 'contextClose
	lazy val classMember = methodDefinition | "" 
	lazy val methodDefinition = $[Method] ~> 'public ~ &{_.name} ~ arguments{_.arguments} ~ 'contextOpen ~ 'contextClose
	lazy val arguments = 'argumentOpen ~ (argument *~ 'argumentSeparator) ~ 'argumentClose
	lazy val argument = $[Argument] ~> &{_.atype} ~ 'typeApplication ~ &{_.name}
}