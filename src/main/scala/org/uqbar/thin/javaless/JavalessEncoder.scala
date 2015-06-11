package org.uqbar.thin.javaless

import scala.language.implicitConversions
import org.uqbar.thin.encoding.combinator._

class JavalessEncoder(_terminals: => Map[Symbol, String] = DefaultTerminals, _preferences: => EncoderPreferences = null) extends JavalessEncoderDefinition {
	def terminals = _terminals
	def preferences = if (_preferences == null) DefaultPreferences else _preferences
	def apply(input: Program) = encode(program)(input)
}

trait JavalessEncoderDefinition extends Encoders {
	lazy val program = $[Program] ~> classDefinition.*{ _.definitions }
	lazy val classDefinition = $[Class] ~> 'class ~ &{ _.name } ~ 'contextOpen ~ classMember.*{ _.body } ~ 'contextClose
	lazy val classMember = methodDefinition | Empty
	lazy val methodDefinition = $[Method] ~> 'public ~ &{ _.name } ~ arguments{ _.arguments } ~ 'contextOpen ~ 'contextClose
	lazy val arguments = 'argumentOpen ~ (argument *~ 'argumentSeparator) ~ 'argumentClose
	lazy val argument = $[Argument] ~> &{ _.atype } ~ 'typeApplication ~ &{ _.name }

	//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────
	// PREFERENCES
	//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

	lazy val DefaultPreferences = new EncoderPreferences(
		spacing = Set(
			After('class),
			After('public),
			After('argumentSeparator),
			After('typeApplication),
			Before('contextOpen)
		),
		
		tabulationSequence = "\t",
		
		tabulationSize = 1,
		
		lineBreaks = Map[Location[_],Int](
			ConditionalLocation(Before(classMember.*)){_.nonEmpty} -> 1,
			ConditionalLocation(After(classMember.*)){_.nonEmpty} -> 1,
			InBetween(classMember) -> 2
		).withDefaultValue(0),
		
		tabulationLevelIncrements = Map[Location[_], Int](
			InBetween(classMember.*) -> 1
		).withDefaultValue(0)
	)
}