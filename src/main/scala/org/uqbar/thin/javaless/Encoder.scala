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

object EncoderResult {
	def apply(content: (String, IdentityHashMap[Any, Range], List[Any])): EncoderResult = Success.tupled(content)
}
trait EncoderResult {
	def map(f: ((String, IdentityHashMap[Any, Range], List[Any])) => (String, IdentityHashMap[Any, Range], List[Any])): EncoderResult
	def flatMap(f: ((String, IdentityHashMap[Any, Range], List[Any])) => EncoderResult): EncoderResult
	def fold[T](seed: T)(f: (String, IdentityHashMap[Any, Range], List[Any]) => T): T
	def withFilter(f: ((String, IdentityHashMap[Any, Range], List[Any])) => Boolean): EncoderResult
	def foreach(f: ((String, IdentityHashMap[Any, Range], List[Any])) => Unit): Unit
}
case class Success(text: String = "", references: IdentityHashMap[Any, Range] = new IdentityHashMap, pending: List[Any] = Nil) extends EncoderResult {
	def map(f: ((String, IdentityHashMap[Any, Range], List[Any])) => (String, IdentityHashMap[Any, Range], List[Any])) = Success.tupled(f(text, references, pending))
	def flatMap(f: ((String, IdentityHashMap[Any, Range], List[Any])) => EncoderResult) = f(text, references, pending)
	def fold[T](seed: T)(f: (String, IdentityHashMap[Any, Range], List[Any]) => T) = f(text, references, pending)
	def withFilter(f: ((String, IdentityHashMap[Any, Range], List[Any])) => Boolean) = if (f(text, references, pending)) this else Failure(s"$this could not be extracted as expected")
	def foreach(f: ((String, IdentityHashMap[Any, Range], List[Any])) => Unit) = f(text, references, pending)
}
case class Failure(reason: String) extends EncoderResult {
	def map(f: ((String, IdentityHashMap[Any, Range], List[Any])) => (String, IdentityHashMap[Any, Range], List[Any])) = this
	def flatMap(f: ((String, IdentityHashMap[Any, Range], List[Any])) => EncoderResult) = this
	def fold[T](seed: T)(f: (String, IdentityHashMap[Any, Range], List[Any]) => T) = seed
	def withFilter(f: ((String, IdentityHashMap[Any, Range], List[Any])) => Boolean) = this
	def foreach(f: ((String, IdentityHashMap[Any, Range], List[Any])) => Unit) {}
}

trait Encoders {
	implicit def StringToEncoder(s: String): Encoder[Any] = new Encoder(r => for ((_, references, pending) <- r) yield (s, references, pending))
	implicit def EncoderToEncoder[U <: Product: TypeTag](e: Encoder[_]): Encoder[U] = e.^^[U]

	class Encoder[T <: Any: TypeTag](tx: EncoderResult => EncoderResult) extends (EncoderResult => EncoderResult) {
		def apply(target: EncoderResult) = tx(target)

		protected def canBeAppliedTo[X](o: Any)(implicit tt: TypeTag[X]) = universe.runtimeMirror(o.getClass.getClassLoader).reflect(o).symbol.toType <:< tt.tpe

		def ~[U: TypeTag](other: Encoder[U]): Encoder[List[Any]] = new Encoder(r => for {
			previous @ (previousText, previousReferences, previousPending) <- this(r)
			(nextText, nextReferences, nextPending) <- other(EncoderResult(previous))
		} yield (previousText + nextText, nextReferences.shifted(previousText.size) ++ previousReferences, nextPending)
		)

		def ^^[U <: Product: TypeTag](): Encoder[U] = this ^^ { u: U => u.productIterator.toList }
		def ^^[U: TypeTag, R >: T](f: U => List[R]): Encoder[U] = new Encoder(r =>
			for {
				(previousText, previousReferences, p :: previousPending) <- r if canBeAppliedTo[U](p)
				(nextText, nextReferences, nextPending) <- this(Success(previousText, previousReferences, f(p.asInstanceOf[U]) ::: previousPending))
			} yield (nextText, nextReferences.updated(p, 0 until nextText.size), nextPending)
		)
		
		def |[U >: T: TypeTag, V <: U](other: Encoder[V]): Encoder[U] = new Encoder(r =>
			this(r).fold(other(r))(EncoderResult(_, _, _))
		)

		def * = *~("")

		def *~(separator: Encoder[_]): Encoder[List[T]] = new Encoder(_.flatMap {
			case (text, references, (ps: List[_]) :: pending) =>
				val encoder = if (ps.isEmpty) {r: EncoderResult => r} else 1.until(ps.size).map{ _ => separator ~ this }.fold(this)(_ ~ _)
				encoder(EncoderResult(text, references, ps))
			case (_, _, pending) => Failure(s"Stack top can't be extracted to list on $this")
		})
	}

	object __ extends Encoder[Any](_ flatMap {
		case (text, references, p :: pending) => Success(p.toString, references, pending)
		case (_, _, Nil) => Failure(s"Empty stack can't be extracted on $this")
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
	lazy val classMember = method
	lazy val method: Encoder[Method] = "public" ~ " " ~ __ ~ 'argumentOpen ~ argument.*~('argumentSeparator ~ " ") ~ 'argumentClose ~ 'contextOpen ~ 'contextClose
	lazy val argument: Encoder[Argument] = __ ~ " " ~ __

}