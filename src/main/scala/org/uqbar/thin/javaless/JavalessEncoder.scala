package org.uqbar.thin.javaless

import scala.language.implicitConversions

import org.uqbar.thin.encoding.combinator.{& => &}
import org.uqbar.thin.encoding.combinator.After
import org.uqbar.thin.encoding.combinator.Before
import org.uqbar.thin.encoding.combinator.ConditionalLocation
import org.uqbar.thin.encoding.combinator.Encoder
import org.uqbar.thin.encoding.combinator.EncoderPreferences
import org.uqbar.thin.encoding.combinator.Encoders
import org.uqbar.thin.encoding.combinator.InBetween
import org.uqbar.thin.encoding.combinator.Location
import org.uqbar.thin.encoding.combinator.Empty

class JavalessEncoder(_terminals: => Map[Symbol, String] = DefaultTerminals, _preferences: => EncoderPreferences = null) extends JavalessEncoderDefinition {
	def terminals = _terminals
	def preferences = if (_preferences == null) DefaultPreferences else _preferences
	def apply(input: Program) = encode(program)(input)
}

trait JavalessEncoderDefinition extends Encoders {

	lazy val program = $[Program] ~> classDefinition.*{ _.classes }
	lazy val classDefinition = $[Class] ~> 'class ~ &{ _.name } ~ 'contextOpen ~ classMember.*{ _.body } ~ 'contextClose
	lazy val classMember: Encoder[ClassMember] = methodDefinition | fieldDefinition
	lazy val methodDefinition = $[Method] ~> &{ _.name } ~ arguments{ _.arguments } ~ 'contextOpen ~ sentence.*{ _.body} ~ 'contextClose
	lazy val fieldDefinition = $[Field] ~> &{ _.name }
  lazy val sentence: Encoder[Sentence] = stringLiteral | Empty
  lazy val stringLiteral = $[StringLiteral] ~> &{ _.value}
	lazy val arguments = 'argumentOpen ~ (& *~ 'argumentSeparator) ~ 'argumentClose

	//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────
	// PREFERENCES
	//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

	lazy val DefaultPreferences = new EncoderPreferences(
		spacing = Set(
			After('class),
			After('argumentSeparator),
			Before('contextOpen)
		),

		tabulationSequence = "\t",

		tabulationSize = 1,

		lineBreaks = Map[Location, Int](
			ConditionalLocation(Before(classMember.*)){ case l: List[_] => l.nonEmpty } -> 1,
			ConditionalLocation(After(classMember.*)){ case l: List[_] => l.nonEmpty } -> 1,
			InBetween(classMember) -> 2
		).withDefaultValue(0),

		tabulationLevelIncrements = Map[Location, Int](
			InBetween(classMember.*) -> 1
		).withDefaultValue(0)
	)
}