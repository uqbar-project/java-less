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
		def map(f:(String,ReferenceTable, (Any, Any))=>(String,ReferenceTable,(Any, Any))): EncoderResult
		def flatMap(f:(String,ReferenceTable,(Any, Any))=>EncoderResult): EncoderResult
		def fold[T](seed: T)(f:(String,ReferenceTable, (Any, Any))=> T): T
	}
	case class Success(text: String = "", references: ReferenceTable = new IdentityHashMap, pending: (Any,Any) = None -> None) extends EncoderResult {
		def map(f:(String,ReferenceTable,(Any,Any))=>(String,ReferenceTable,(Any,Any))) = Success.tupled(f(text,references,pending))
		def flatMap(f:(String,ReferenceTable,(Any,Any))=>EncoderResult) = f(text,references,pending)
		def fold[T](seed: T)(f:(String,ReferenceTable,(Any,Any))=> T) = f(text,references,pending)
	}
	case class Failure(reason: String, pending: (Any,Any)) extends EncoderResult {
		def map(f:(String,ReferenceTable,(Any,Any))=>(String,ReferenceTable,(Any,Any))) = this
		def flatMap(f:(String,ReferenceTable,(Any,Any))=>EncoderResult) = this
		def fold[T](seed: T)(f:(String,ReferenceTable,(Any,Any))=> T) = seed
	}

	implicit def ValueToSegg[A](x: A) = x -> (None -> None)
			implicit def StringToEncoder(s: String): Encoder[Any] = Encoder {
			case Success(text, references, pending) => Success(text + s, references, pending)
			case other => other
	}

	object Encoder {
		def apply[T: TypeTag](f: EncoderResult => EncoderResult) = new Encoder[T] { def apply(r: EncoderResult) = f(r) }
	}
	abstract class Encoder[+T: TypeTag] extends (EncoderResult => EncoderResult) {
		protected def canBeAppliedTo[X](o: Any)(implicit tt: TypeTag[X]) = universe.runtimeMirror(o.getClass.getClassLoader).reflect(o).symbol.toType <:< tt.tpe

		def ~[U: TypeTag](other: Encoder[U]) = Encoder[(T, U)](this andThen other)

		def ^^[U: TypeTag, R >: T](f: U => R) = Encoder[U] (_.flatMap{ case (text,references,(p, pending)) =>
			if (canBeAppliedTo[U](p)) this(Success(text, references, f(p.asInstanceOf[U]) -> pending))
			else Failure(s"$p could not be extracted as ${typeOf[U]}", p -> pending)
		})

		def |[U >: T: TypeTag](other: Encoder[U]) = Encoder[U] {r => this(r).fold(other(r))(Success.tupled(_,_,_))}
		def * = Encoder[List[T]]{_.map{case (text,references, (p: List[T], pending)) =>
			(text + p.map{e => this(Success(pending = e))}.mkString, references, pending)
		}}
	}
	
	object __ extends Encoder[Any] {
		def apply(target: EncoderResult) = target map { case (text, references, (p, (pending: (Any,Any)))) => (text + p, references, pending) }
	}


	def encode[T](encoder: Encoder[T])(target: T*) = encoder(Success(pending = (new (Any,Any)((), ()) /: target){ new ~(_, _) }))
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

	lazy val program = classDefinition.* ^^ { p: Program => p.definitions }
	lazy val classDefinition = 'class ~ " " ~ __ ~ " " ~ 'contextOpen ~ classMember.* ~ 'contextClose ^^ { c: Class => new ~(c.name, c.body) }
	lazy val classMember: Encoder[Any] = ""

	//	protected def encode(element: SyntaxElement): EncoderResult = {
	//		val content: EncoderResult = element match {
	//			case Program(definitions) => (EncoderResult.base /: definitions){ _ ~ encode(_) }
	//			case Class(name, body) => 'class ~ " " ~ name ~ " " ~ 'contextOpen ~ " " ~ 'contextClose
	//		}
	//
	//		content.updated(element, 0 until content.text.size)
	//	}
}