package org.uqbar.thin.javaless

sealed trait SyntaxElement

case class Program(classes: List[Class]) extends SyntaxElement

case class Class(name: Identifier, body: List[ClassMember]) extends SyntaxElement

sealed trait ClassMember extends SyntaxElement
case class Method(name: Identifier, arguments: List[Identifier],  body: List[Sentence]) extends ClassMember
case class Field(name: Identifier) extends ClassMember

sealed trait Sentence
sealed trait Expression

sealed trait Literal extends Expression
case class IntLiteral(value: Int) extends Literal