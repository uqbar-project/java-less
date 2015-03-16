package org.uqbar.thin.javaless

import scala.language.implicitConversions
import scala.util.parsing.combinator.JavaTokenParsers


class Parser(_terminals: => Map[Symbol, String] = DefaultTerminals) extends ParserDefinition {
	def terminals = _terminals
	
	def apply(input: String) = parseAll(program, input)
}

trait ParserDefinition extends JavaTokenParsers {
	def terminals: Map[Symbol, String]

	protected implicit def SymbolToParser(s: Symbol): Parser[String] = terminals(s)

	lazy val identifier = ident
	lazy val program = classDefinition.* ^^ Program
	lazy val classDefinition = 'class ~> identifier <~ 'contextOpen ~ 'contextClose ^^ { case name => Class(name, Nil) }
}