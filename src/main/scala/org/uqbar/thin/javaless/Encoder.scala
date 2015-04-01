package org.uqbar.thin.javaless

import scala.language.implicitConversions

import org.uqbar.thin.encoding.combinator.Encoders

class Encoder(_terminals: => Map[Symbol, String] = DefaultTerminals) extends EncoderDefinition {
	val terminals = _terminals
	def apply(input: Program) = encode(program)(input)
}

trait EncoderDefinition extends Encoders {
	val terminals: Map[Symbol, String]

	implicit protected def SymbolToEncoder(s: Symbol): Encoder[Any] = terminals(s)

	lazy val program: Encoder[Program] = classDefinition.*
	lazy val classDefinition: Encoder[Class] = 'class ~ " " ~ __ ~ " " ~ 'contextOpen ~ classMember.* ~ 'contextClose
	lazy val classMember = methodDefinition
	lazy val methodDefinition: Encoder[Method] = 'public ~ " " ~ __ ~ 'argumentOpen ~ argument.*~('argumentSeparator ~ " ") ~ 'argumentClose ~ 'contextOpen ~ 'contextClose
	lazy val argument: Encoder[Argument] = __ ~ " " ~ __
}