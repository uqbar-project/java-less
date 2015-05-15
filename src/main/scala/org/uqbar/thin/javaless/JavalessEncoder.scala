package org.uqbar.thin.javaless

import scala.language.implicitConversions
import org.uqbar.thin.encoding.combinator._

class JavalessEncoder(_terminals: => Map[Symbol, String] = DefaultTerminals, _preferences: => EncoderPreferences = DefaultPreferences) extends JavalessEncoderDefinition {
	def terminals = _terminals
	def preferences = _preferences
	def apply(input: Program) = encode(program)(input)
}

trait JavalessEncoderDefinition extends Encoders {
	lazy val program: Encoder[Program] = classDefinition.*{(_:Program).definitions}
	lazy val classDefinition: Encoder[Class] = 'class ~ __{(_:Class).name} ~ 'contextOpen ~~(classMember.*{(_:Class).body}) ~ 'contextClose
	lazy val classMember: Encoder[ClassMember] = methodDefinition | ""
	lazy val methodDefinition: Encoder[Method] = 'public ~ __{(_:Method).name} ~ arguments{(_:Method).arguments} ~ 'contextOpen ~ 'contextClose
	lazy val arguments = 'argumentOpen ~ (argument *~ 'argumentSeparator) ~ 'argumentClose
	lazy val argument: Encoder[Argument] = __{(_:Argument).atype} ~ 'typeApplication ~ __{(_:Argument).name}
}