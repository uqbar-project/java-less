package org.uqbar.thin.lambda

trait View[T] {
	type EncodePreferences
	type DecodePreferences
	
	def encode(target: SyntaxElement)(implicit preferences: EncodePreferences): T
	def decode[U<:SyntaxElement](target: String)(implicit preferences: DecodePreferences): U
}

class SourceView(language: Language) extends View[String] {
	type EncodePreferences = SourceEncodePreferences
	type DecodePreferences = SourceDecodePreferences
	
	case class SourceEncodePreferences()
	case class SourceDecodePreferences()
	
	def encode(target: SyntaxElement)(implicit preferences: EncodePreferences): String = ???
	def decode[U<:SyntaxElement](target: String)(implicit preferences: DecodePreferences): U = ???

}