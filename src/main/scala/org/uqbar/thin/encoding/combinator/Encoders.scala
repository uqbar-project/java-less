package org.uqbar.thin.encoding.combinator

import scala.language.implicitConversions
import scala.reflect.runtime.universe
import scala.util.Try

import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.universe

trait Encoders {
	implicit def StringToEncoder(s: String): Encoder[Any] = new Encoder(r => for ((_, references, pending) <- r) yield (s, references, pending))
	implicit def EncoderToEncoder[U <: Product: TypeTag](e: Encoder[_]): Encoder[U] = e.^^[U]

	class Encoder[T <: Any: TypeTag](tx: EncoderResult => EncoderResult) extends (EncoderResult => EncoderResult) {
		def apply(target: EncoderResult) = tx(target)

		protected def canBeAppliedTo[X](o: Any)(implicit tt: TypeTag[X]) = universe.runtimeMirror(o.getClass.getClassLoader).reflect(o).symbol.toType <:< tt.tpe

		def ~[U: TypeTag](other: Encoder[U]): Encoder[List[Any]] = new Encoder(r => for {
			previous @ (previousText, previousReferences, previousPending) <- this(r)
			(nextText, nextReferences, nextPending) <- other(Try("", previousReferences, previousPending))
			shiftedNextReferences = nextReferences.map{ case (k, v) => k -> (v.start + previousText.size until v.end + previousText.size) }
		} yield (previousText + nextText, shiftedNextReferences ++ previousReferences, nextPending)
		)

		def ^^[U <: Product: TypeTag](): Encoder[U] = this ^^ { u: U => u.productIterator.toList }
		def ^^[U: TypeTag, R >: T](f: U => List[R]): Encoder[U] = new Encoder(r =>
			for {
				(previousText, previousReferences, p :: previousPending) <- r if canBeAppliedTo[U](p)
				(nextText, nextReferences, nextPending) <- this(Try(previousText, previousReferences, f(p.asInstanceOf[U]) ::: previousPending))
			} yield (nextText, nextReferences + (p, 0 until nextText.size), nextPending)
		)

		def |[U >: T: TypeTag, V <: U](other: Encoder[V]): Encoder[U] = new Encoder(r =>
			this(r).orElse(other(r))
		)

		def * = *~("")

		def *~(separator: Encoder[_]): Encoder[List[T]] = new Encoder(_.flatMap {
			case (_, references, (ps: List[_]) :: _) =>
				val encoder = if (ps.isEmpty) { r: EncoderResult => r } else 1.until(ps.size).map{ _ => separator ~ this }.fold(this)(_ ~ _)
				encoder(Try("", references, ps))
			case (_, _, pending) => throw new RuntimeException(s"Stack top can't be extracted to list on $this")
		})
	}

	object __ extends Encoder[Any](_ flatMap {
		case (text, references, p :: pending) => Try(p.toString, references, pending)
		case (_, _, Nil) => throw new RuntimeException(s"Empty stack can't be extracted on $this")
	})

	def encode[T](encoder: Encoder[T])(target: T) = encoder(EncoderResult(target))
}