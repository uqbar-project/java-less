package org.uqbar.thin.javaless

case class Identifier(value: String)

sealed trait SyntaxElement

case class Program(definitions: List[Class]) extends SyntaxElement

case class Class(name: Identifier, body: List[ClassMember]) extends SyntaxElement
sealed trait ClassMember