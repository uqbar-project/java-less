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
  lazy val classDefinition = 'class ~> identifier ~ 'contextOpen ~ methodDefinition.* <~ 'contextClose ^^ { case name ~ _ ~ methods => Class(name, methods) }
  lazy val methodDefinition = "public" ~> identifier ~ arguments <~ 'contextOpen ~ 'contextClose ^^ { case name ~ args => Method(name, args, Nil) }
  lazy val arguments = 'argumentOpen ~> repsep(argument, ',') <~ 'argumentClose
  lazy val argument = "void" ~ identifier ^^ {case atype ~ arg => Argument(atype, arg)}
}