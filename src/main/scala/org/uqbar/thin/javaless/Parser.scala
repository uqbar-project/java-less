package org.uqbar.thin.javaless

import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends Parser {
	def apply(input: String) = parseAll(program, input)
}

trait Parser extends JavaTokenParsers {
	protected def identifier = ident ^^ Identifier

	protected def program = classDef.* ^^ Program
	protected def classDef = "class" ~> identifier <~ "{" ~ "}" ^^ { case name => Class(name, Nil) }
}