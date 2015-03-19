package org.uqbar.thin.javaless

import scala.language.implicitConversions
import scala.collection.JavaConversions._
import java.util.IdentityHashMap
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.universe

class Encoder(_terminals: => Map[Symbol, String] = DefaultTerminals) extends EncoderDefinition {
	val terminals = _terminals

	def apply(input: Program) = encode(program)(input)
}

trait EncoderDefinition extends Encoders {
	val terminals: Map[Symbol, String]

	implicit protected def SymbolToEncoder(s: Symbol): Encoder[Any] = terminals(s)
	
	lazy val program = classDefinition.* ^^ {p: Program => p.definitions }
	lazy val classDefinition = 'class ~ " " ~ __ ~ " " ~ 'contextOpen ~ method.*  ~ 'contextClose ^^ {c: Class => c.name -> c.body}
  lazy val method = "public" ~ " " ~ __ ~ 'argumentOpen ~  argument.repsep(",") ~ 'argumentClose ~ 'contextOpen ~ 'contextClose ^^ {m : Method => m.name -> m.arguments}
  lazy val argument = __ ~ " " ~ __ ^^ {a: Argument => a.atype -> a.name}
	
//	protected def encode(element: SyntaxElement): EncoderResult = {
		//		val content: EncoderResult = element match {
		//			case Program(definitions) => (EncoderResult.base /: definitions){ _ ~ encode(_) }
		//			case Class(name, body) => 'class ~ " " ~ name ~ " " ~ 'contextOpen ~ " " ~ 'contextClose
		//		}
		//
		//		content.updated(element, 0 until content.text.size)
//	}
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// ENCODER RESULT
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

sealed class EncoderResult(val pending: List[Any]) {
	def ~(other: EncoderResult) = this
	def updated(key: SyntaxElement, value: Range) = this
	def text = ""
  def repsep(sep: String) = this;
}
//case class Error(pending: List[Any]) extends EncoderResult(pending)
//case class Failure(pending: List[Any]) extends EncoderResult(pending)
//case class Success(text: String = "", references: IdentityHashMap[SyntaxElement, Range] = new IdentityHashMap, pending: List[Any]) extends EncoderResult(pending) {
//	def ~(other: EncoderResult) = other match {
//		case Success(otherText, otherReferences,_) => copy(text ++ otherText, references ++ otherReferences.shifted(text.size))
//		case _ => other
//	}
//	def updated(key: SyntaxElement, value: Range) = copy(references = references.updated(key, value))
//}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// ENCODERS
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

trait Encoders {
	trait EncoderResult
	case class Success(text: String = "", references: IdentityHashMap[SyntaxElement, Range] = new IdentityHashMap, pending: List[Any] = Nil) extends EncoderResult
	case class Failure(reason: String, pending: List[Any]) extends EncoderResult

	object Encoder {
		def apply[T](f: EncoderResult => EncoderResult) = new Encoder[T] { def apply(r: EncoderResult) = f(r) }
	}
	trait Encoder[+T] extends (EncoderResult => EncoderResult) {
		def ~[U, R](other: Encoder[U]) = Encoder[R](this andThen other)
		def ^^[R](f: R => Product)(implicit tt: TypeTag[R]) = {
			def isInstanceOf(o: Any, tt: TypeTag[_]) = universe.runtimeMirror(o.getClass.getClassLoader).reflect(o).symbol.toType <:< tt.tpe
			Encoder[R] {
				case Success(text, references, p :: pending) =>
					 if(isInstanceOf(p, tt)) this(Success(text, references, f(p.asInstanceOf[R]).productIterator.toList ::: pending))
					else Failure(s"$p could not be extracted as ${tt.tpe}", p :: pending)
				case other => other
			}
		}
		def |[U, R](other: Encoder[U]) = Encoder[R]{ r =>
			this(r) match {
			case s: Success => s
			case f => other(r)
			}
		}
		def * = Encoder[List[T]]{
			case Success(text,references,(p: List[T]) :: pending) => Success(text+p.map{e=> this(Success(pending = e::Nil))}.mkString,references,pending)
      case other => other
		}
    
    def repsep(s : => String) = Encoder[List[T]]{
      case Success(text,references,(p: List[T]) :: pending) => Success(text+p.map{e=> this(Success(pending = e::Nil))}.mkString(s),references,pending)
      case other => other
    }
  }

	object __ extends Encoder[String] {
		def apply(target: EncoderResult) = target match {
			case Success(text, references, p :: pending) => Success(text + p, references, pending)
			case other => other
		}
	}

	implicit def value2tuple[A](x: A) = Tuple1(x)
	implicit def StringToEncoder(s: String): Encoder[Any] = Encoder {
		case Success(text, references, pending) => Success(text + s, references, pending)
		case other => other
	}
	
	def encode[T](encoder: Encoder[T])(target: T*) = encoder(Success(pending= target.toList))
}