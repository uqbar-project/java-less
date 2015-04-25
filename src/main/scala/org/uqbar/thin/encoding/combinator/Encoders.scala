package org.uqbar.thin.encoding.combinator

import scala.language.implicitConversions
import scala.reflect.runtime.universe
import scala.util.Try

import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.universe

trait Encoders {

	def preferences: EncoderPreferences
	def terminals: Map[Symbol, String]

	implicit protected def StringToEncoder(s: String): Encoder[Any] = Constant(s)
	implicit protected def SymbolToEncoder(s: Symbol): Encoder[Any] = terminals(s)
	implicit protected def EncoderToEncoder[U <: Product: TypeTag](e: Encoder[_]): Encoder[U] = e.^^[U]

	def encode[T](encoder: Encoder[T])(target: T) = encoder(EncoderResult(target))

	class Encoder[T <: Any: TypeTag](tx: EncoderResult => EncoderResult) extends (EncoderResult => EncoderResult) {
		def apply(target: EncoderResult) = tx(target)

		def ~[U: TypeTag](other: Encoder[U]): Encoder[List[Any]] = Append(this, other)

		def |[U >: T: TypeTag, V <: U](other: Encoder[V]): Encoder[U] = Or(this, other)

		def * = *~("")

		def *~(separator: Encoder[_]): Encoder[List[T]] = RepSep(this, separator)

		def ^^[U <: Product: TypeTag](): Encoder[U] = this ^^ { u: U => u.productIterator.toList }
		def ^^[U: TypeTag, R >: T](f: U => List[R]): Encoder[U] = Extract[T, U, R](this)(f)
	}

	//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
	// ENCODERS
	//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

	case object __ extends Encoder[Any](target =>
		for { (text, references, p :: pending) <- target } yield (p.toString, references, pending)
	)

	case class Constant(value: String) extends Encoder[Any](target =>
		for { (_, references, pending) <- target } yield (value, references, pending)
	)

	case class Extract[T: TypeTag, U, R >: T](before: Encoder[T])(f: U => List[R])(implicit tt: TypeTag[U]) extends Encoder[U](target =>
		for {
			(previousText, previousReferences, p :: previousPending) <- target
			if universe.runtimeMirror(p.getClass.getClassLoader).reflect(p).symbol.toType <:< tt.tpe
			(nextText, nextReferences, nextPending) <- before(Try(previousText, previousReferences, f(p.asInstanceOf[U]) ::: previousPending))
		} yield (nextText, nextReferences + (p, 0 until nextText.size), nextPending)
	)

	case class Or[T: TypeTag, U >: T: TypeTag, V <: U](some: Encoder[T], other: Encoder[V]) extends Encoder[U](target =>
		some(target).orElse(other(target))
	)

	case class RepSep[T: TypeTag, S: TypeTag](body: Encoder[T], separator: Encoder[S]) extends Encoder[List[T]](target =>
		for {
			(_, references, (ps: List[_]) :: _) <- target
			encoder = if (ps.isEmpty) { r: EncoderResult => r } else 1.until(ps.size).map{ _ => separator ~ body }.fold(body)(_ ~ _)
			result <- encoder(Try("", references, ps))
		} yield result
	)

	case class Append[L: TypeTag, R: TypeTag](left: Encoder[L], right: Encoder[R]) extends Encoder[List[Any]](target =>
		for {
			previous @ (previousText, previousReferences, previousPending) <- left(target)
			(nextText, nextReferences, nextPending) <- right(Try("", previousReferences, previousPending))
			shiftedNextReferences = nextReferences.map{ case (k, v) => k -> (v.start + previousText.size until v.end + previousText.size) }
		} yield (previousText + nextText, shiftedNextReferences ++ previousReferences, nextPending)
	)

}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// PREFERENCES
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

class EncoderPreferences(spacing: Map[Location, Boolean])

trait Location
case class Before(target: Encoders#Encoder[Any]) extends Location
case class After(target: Encoders#Encoder[Any]) extends Location
case class Between(left: Encoders#Encoder[Any], right: Encoders#Encoder[Any]) extends Location