package org.uqbar.thin.javaless

import scala.language.implicitConversions
import scala.util.parsing.combinator.JavaTokenParsers

class JavalessParser(_terminals: => Map[Symbol, String] = DefaultTerminals) extends JavalessParserDefinition {
  def terminals = _terminals

  def apply(input: String) = parseAll(program, input)
}

trait JavalessParserDefinition extends JavaTokenParsers {
  def terminals: Map[Symbol, String]

  protected implicit def SymbolToParser(s: Symbol): Parser[String] = terminals(s)
  
  lazy val identifier = ident
  lazy val literalString =  ("\""+"""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*+"""+"\"").r ^^ StringLiteral
  lazy val program = classDefinition.* ^^ Program
  lazy val classDefinition = 'class ~> identifier ~ 'contextOpen ~ classMember.* <~ 'contextClose ^^ { case name ~ _ ~ methods => Class(name, methods) }
  lazy val classMember = methodDefinition | fieldDefinition
  lazy val methodDefinition = identifier ~ arguments ~ 'contextOpen ~ sentence.* <~ 'contextClose ^^ { case name ~ args ~ _ ~ methodSentence => Method(name, args, methodSentence) }
  lazy val sentence: Parser[Sentence] = literalString
  lazy val fieldDefinition = identifier ^^ Field
  lazy val arguments = 'argumentOpen ~> repsep(identifier, 'argumentSeparator) <~ 'argumentClose
}