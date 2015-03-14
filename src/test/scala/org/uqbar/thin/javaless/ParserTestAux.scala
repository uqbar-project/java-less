package org.uqbar.thin.javaless

import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader

import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

trait ParserTest extends Matchers {

	import scala.language.reflectiveCalls

	val parserDefinition: Parsers { type Elem = Char }

	case class beParsedTo[T](expected: T)(implicit parser: parserDefinition.Parser[T]) extends Matcher[String] {
		def apply(target: String) = {
			val result = parser.apply(new CharSequenceReader(target))

			MatchResult(
				result.successful && result.get == expected,
				if (result.successful) s"Parsed ${result.get} did not equal $expected" else s"Parse failed! $result",
				if (result.successful) s"Parsed ${result.get} was equal to $expected" else s"Parse didn't fail! $result"
			)
		}
	}

	case class beParsed[T](implicit parser: parserDefinition.Parser[T]) extends Matcher[String] {
		def apply(target: String) = {
			val result = parser(new CharSequenceReader(target))
			MatchResult(
				result.successful,
				s"Parse failed: $result",
				s"Parse didn't fail! $result"
			)
		}
	}

	def notBeParsed[T](implicit parser: parserDefinition.Parser[T]) = not(beParsed()(parser))

}