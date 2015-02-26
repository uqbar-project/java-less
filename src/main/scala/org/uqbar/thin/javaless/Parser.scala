package org.uqbar.thin.javaless

import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends Parser

trait Parser extends JavaTokenParsers {
	def apply(input: String) = parseAll(clazz.*, input)

	protected lazy val identifier = ident ^^ Identifier
	protected lazy val typeSignature = ident

	protected lazy val scope = "public" | "protected" | "private" | "default"

	protected lazy val clazz = scope.? ~> "class" ~> identifier ~ ("{" ~> clazzMember.* <~ "}") ^^ { case name ~ body => Class(name, body) }
	protected lazy val clazzMember = field | method
	protected lazy val field = scope.? ~> typeSignature ~ identifier <~ ";" ^^ { case tpe ~ name => Field(name, tpe) }
	protected lazy val method = scope.? ~> typeSignature ~ identifier <~ "(" ~ ")" ~ "{" ~ "}" ^^ { case tpe ~ name => Method(name, tpe) }
}