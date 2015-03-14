package org.uqbar.thin.javaless

import scala.language.implicitConversions
import scala.util.parsing.combinator.JavaTokenParsers

class Parser(terminals: => Map[Symbol, String] = DefaultTerminals){
	
	def apply(input:String) = Definition.parseAll(Definition.program,input)

	object Definition extends JavaTokenParsers {
		protected implicit def SymbolToParser(s: Symbol): Parser[String] = terminals(s)

		lazy val identifier = ident ^^ Identifier
		lazy val program = classDefinition.* ^^ Program
		lazy val classDefinition = 'class ~> identifier <~ 'contextOpen ~ 'contextClose ^^ { case name => Class(name, Nil) }
	}
}