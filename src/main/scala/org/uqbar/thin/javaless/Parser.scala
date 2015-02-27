package org.uqbar.thin.javaless

import scala.language.implicitConversions
import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends Parser {
	def apply(input: String) = parseAll(program, input)
}

trait Parser extends JavaTokenParsers {
	protected val terminals: Map[Symbol, String] = DefaultTerminals

	implicit def SymbolToParser(s: Symbol): Parser[String] = terminals(s)

	protected def identifier = ident ^^ Identifier

	protected def program = classDef.* ^^ Program
	protected def classDef = 'class ~> identifier <~ 'contextOpen ~ 'contextClose ^^ { case name => Class(name, Nil) }
}