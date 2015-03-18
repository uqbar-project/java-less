package org.uqbar.thin.javaless

import scala.language.implicitConversions
import scala.collection.JavaConversions._
import java.util.IdentityHashMap

object Encoder extends Encoder {
	def apply(input: Program) = encode(input)
}

trait Encoder {
	protected val terminals: Map[Symbol, String] = DefaultTerminals

	implicit protected def StringToEncoderResult(s: String) = Success(s)
	implicit protected def SymbolToEncoderResult(s: Symbol) = StringToEncoderResult(terminals(s))
  
	protected def encode(element: SyntaxElement): EncoderResult = {
    val content: EncoderResult = element match {
      case Program(definitions) => (EncoderResult.base /: definitions){ _ ~ encode(_) }
      case Class(name, body) => 'class ~ " " ~ name ~ " " ~ 'contextOpen ~ (EncoderResult.base /: body){ _ ~ encode(_)} ~ 'contextClose
      case Method(name, arguments,  body) => "public" ~ " " ~ name ~ 'argumentOpen ~ arguments.map(a => a.atype ++ " " ++ a.name).mkString(terminals('sentenceSep) ++ " ") ~ 'argumentClose ~ 'contextOpen ~ 'contextClose
      case Argument(atype, name) => atype ++ " " ++ name
    }

		content.updated(element, 0 until content.text.size)
	}
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// ENCODER RESULT
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

object EncoderResult {
	def base: EncoderResult = Success()
}
sealed trait EncoderResult {
	def ~(other: EncoderResult): EncoderResult
	def updated(key: SyntaxElement, value: Range): EncoderResult
	def text: String
}
case class Error(reason: String) extends EncoderResult {
	def ~(other: EncoderResult) = this
	def updated(key: SyntaxElement, value: Range) = this
	def text = ""
}
case class Success(text: String = "", references: IdentityHashMap[SyntaxElement, Range] = new IdentityHashMap) extends EncoderResult {
	def ~(other: EncoderResult) = other match {
		case Success(otherText, otherReferences) => copy(text ++ otherText, references ++ otherReferences.shifted(text.size))
		case error => error
	}
	def updated(key: SyntaxElement, value: Range) = copy(references = references.updated(key, value))
}
