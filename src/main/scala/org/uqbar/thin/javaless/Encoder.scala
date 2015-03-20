package org.uqbar.thin.javaless

import scala.language.implicitConversions
import scala.collection.JavaConversions._
import java.util.IdentityHashMap
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.universe.typeOf
import scala.reflect.runtime.universe

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// ENCODER RESULT
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

//sealed class EncoderResult(val pending: List[Any]) {
//	def ~(other: EncoderResult) = this
//	def updated(key: SyntaxElement, value: Range) = this
//	def text = ""
//}
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
	type ReferenceTable = IdentityHashMap[SyntaxElement, Range]
	trait EncoderResult {
		def map(f: (String, ReferenceTable, List[Any]) => (String, ReferenceTable, List[Any])): EncoderResult
		def flatMap(f: (String, ReferenceTable, List[Any]) => EncoderResult): EncoderResult
		def fold[T](seed: T)(f: (String, ReferenceTable, List[Any]) => T): T
	}
	case class Success(text: String = "", references: ReferenceTable = new IdentityHashMap, pending: List[Any] = Nil) extends EncoderResult {
		def map(f: (String, ReferenceTable, List[Any]) => (String, ReferenceTable, List[Any])) = Success.tupled(f(text, references, pending))
		def flatMap(f: (String, ReferenceTable, List[Any]) => EncoderResult) = f(text, references, pending)
		def fold[T](seed: T)(f: (String, ReferenceTable, List[Any]) => T) = f(text, references, pending)
	}
	case class Failure(reason: String, pending: List[Any]) extends EncoderResult {
		def map(f: (String, ReferenceTable, List[Any]) => (String, ReferenceTable, List[Any])) = this
		def flatMap(f: (String, ReferenceTable, List[Any]) => EncoderResult) = this
		def fold[T](seed: T)(f: (String, ReferenceTable, List[Any]) => T) = seed
	}

	implicit def StringToEncoder(s: String): Encoder[Any] = _.map{ (text, references, pending) => (text + s, references, pending) }
	implicit def EncoderToEncoder[U <: Product: TypeTag](e: Encoder[List[Any]]) = e.^^[U]

	implicit class Encoder[T<: Any](tx: EncoderResult => EncoderResult) extends (EncoderResult => EncoderResult) {
		def apply(target: EncoderResult) = tx(target)

		protected def canBeAppliedTo[X](o: Any)(implicit tt: TypeTag[X]) = universe.runtimeMirror(o.getClass.getClassLoader).reflect(o).symbol.toType <:< tt.tpe

		def ~[U: TypeTag](other: Encoder[U]): Encoder[List[Any]] = this andThen other

		def ^^[U <: Product: TypeTag](): Encoder[U] = this ^^ { u: U => u.productIterator.toList }
		def ^^[U: TypeTag, R >: T](f: U => List[R]): Encoder[U] = _.flatMap{
			case (text, references, p :: pending) if (canBeAppliedTo[U](p)) => this(Success(text, references, f(p.asInstanceOf[U]) ::: pending))
			case (_, _, pending) => Failure(s"Stack top could not be extracted as ${typeOf[U]}", pending)
		}

		def |[U >: T: TypeTag](other: Encoder[U]): Encoder[U] = r => this(r).fold(other(r))(Success.tupled(_, _, _))
		def * : Encoder[List[T]] = _.flatMap{
			case (text, references, (p: List[T]) :: pending) => Success(text + p.map{ e => this(Success(pending = e :: Nil)) }.mkString, references, pending)
			case (_, _, pending) => Failure("Stack top can't be extracted to list", pending)
		}
    
    def repsep(sep: String): Encoder[List[T]] = _.flatMap{
      case (text, references, (p: List[T]) :: pending) => Success(text + p.map{ e => this(Success(pending = e :: Nil)) }.mkString(sep), references, pending)
      case (_, _, pending) => Failure("Stack top can't be extracted to list", pending)
    }
	}

	object __ extends Encoder[String](_ flatMap {
		case (text, references, p :: pending) => Success(text + p, references, pending)
		case (_, _, Nil) => Failure("Empty stack can't be extracted", Nil)
	})

	def encode[T](encoder: Encoder[T])(target: T) = encoder(Success(pending = target :: Nil))
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// JAVALESS
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

class Encoder(_terminals: => Map[Symbol, String] = DefaultTerminals) extends EncoderDefinition {
	val terminals = _terminals

	def apply(input: Program) = encode(program)(input)
}

trait EncoderDefinition extends Encoders {

	val terminals: Map[Symbol, String]

	implicit protected def SymbolToEncoder(s: Symbol): Encoder[Any] = terminals(s)

	lazy val program: Encoder[Program] = classDefinition.*
	lazy val classDefinition: Encoder[Class] = 'class ~ " " ~ __ ~ " " ~ 'contextOpen ~ classMember.* ~ 'contextClose
	lazy val classMember: Encoder[Any] = method
  lazy val method = "public" ~ " " ~ __ ~ 'argumentOpen ~  argument.repsep(",") ~ 'argumentClose ~ 'contextOpen ~ 'contextClose
  lazy val argument = __ ~ " " ~ __

}