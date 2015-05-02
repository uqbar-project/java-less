package org.uqbar.thin.javaless

import scala.language.implicitConversions
import org.uqbar.thin.encoding.combinator._

class JavalessEncoder(_terminals: => Map[Symbol, String] = DefaultTerminals, _preferences: => EncoderPreferences = DefaultPreferences) extends JavalessEncoderDefinition {
	def terminals = _terminals
	def preferences = _preferences
	def apply(input: Program) = encode(program)(input)
}

trait JavalessEncoderDefinition extends Encoders {
	lazy val program: Encoder[Program] = classDefinition.*
	lazy val classDefinition: Encoder[Class] = 'class ~ __ ~ 'contextOpen ~~(classMember.*) ~ 'contextClose
	lazy val classMember = methodDefinition
	lazy val methodDefinition: Encoder[Method] = 'public ~ __ ~ arguments ~ 'contextOpen ~ 'contextClose
	lazy val arguments = 'argumentOpen ~ (argument *~ 'argumentSeparator) ~ 'argumentClose
	lazy val argument: Encoder[Argument] = __ ~ 'typeApplication ~ __
}