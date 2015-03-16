package org.uqbar.thin.javaless

sealed trait SyntaxElement

case class Program(definitions: List[Class]) extends SyntaxElement

case class Class(name: Identifier, body: List[ClassMember]) extends SyntaxElement
sealed trait ClassMember extends SyntaxElement

case class Method(name: Identifier, arguments: List[Argument],  body: List[MethodMember]) extends ClassMember
sealed trait MethodMember

case class Argument(atype: Identifier, name: Identifier) extends SyntaxElement