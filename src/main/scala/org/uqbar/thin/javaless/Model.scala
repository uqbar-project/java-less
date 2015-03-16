package org.uqbar.thin.javaless

sealed trait SyntaxElement

case class Program(definitions: List[Class]) extends SyntaxElement

case class Class(name: Identifier, body: List[ClassMember]) extends SyntaxElement
sealed trait ClassMember