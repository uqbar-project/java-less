package org.uqbar.thin.javaless

case class Identifier(value: String)

case class Class(name: Identifier, body: List[ClassMember])

sealed trait ClassMember
case class Field(name: Identifier, typeSignature: String) extends ClassMember
case class Method(name: Identifier, typeSignature: String) extends ClassMember