package org.uqbar.thin.encoding.combinator

import scala.language.implicitConversions
import scala.language.existentials
import scala.reflect.runtime.universe
import scala.util.Try
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.universe
import org.uqbar.utils.collections.immutable.IdentityMap

trait Encoders {
	def preferences: EncoderPreferences
	def terminals: Map[Symbol, String]

	implicit protected def StringToEncoder(s: String): Encoder[Any] = Constant(s)
	implicit protected def SymbolToEncoder(s: Symbol): Encoder[Any] = Constant(terminals(s))
	implicit protected def EncoderToEncoder[U <: Product: TypeTag](e: Encoder[_]): Encoder[U] = e.^^[U]

	def encode[T](encoder: Encoder[T])(target: T) = encoder(preferences)(EncoderResult(target))
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// ENCODERS
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

abstract class Encoder[T <: Any: TypeTag] {
	def apply(preferences: EncoderPreferences, level: Int = 0)(target: EncoderResult): EncoderResult

	def ~[U: TypeTag](other: Encoder[U]): Encoder[List[Any]] = Append(this, other)

	def ~~[U: TypeTag](other: Encoder[U]): Encoder[List[Any]] = this ~ Subcontext(other)

	def |[U >: T: TypeTag, V <: U](other: Encoder[V]): Encoder[U] = Or(this, other)

	def * = *~(Constant(""))

	def *~(separator: Encoder[_]): Encoder[List[T]] = RepSep(this, separator)

	def ^^[U <: Product: TypeTag](): Encoder[U] = this ^^ { u: U => u.productIterator.toList }
	def ^^[U: TypeTag, R >: T](f: U => List[R]): Encoder[U] = Extract[T, U, R](this)(f)
	
	protected def tabulation(preferences: EncoderPreferences, level: Int) = "\t" * level
}

case object Id extends Encoder[Any] {
	def apply(preferences: EncoderPreferences, level: Int)(target: EncoderResult) = target
}

case object __ extends Encoder[Any] {
	def apply(preferences: EncoderPreferences, level: Int)(target: EncoderResult) = {
		for { (text, references, p :: pending) <- target } yield (s"${tabulation(preferences,level)}$p", references, pending)
	}
}

case class Constant(value: String) extends Encoder[Any] {
	def apply(preferences: EncoderPreferences, level: Int)(target: EncoderResult) = {
		val beforeSpace = if (preferences.spacing(Before(Constant(value)))) " " else ""
		val afterSpace = if (preferences.spacing(After(Constant(value)))) " " else ""

		for { (_, references, pending) <- target } yield (s"${tabulation(preferences,level)}$beforeSpace$value$afterSpace", references, pending)
	}
}

case class Extract[T: TypeTag, U, R >: T](before: Encoder[T])(f: U => List[R])(implicit tt: TypeTag[U]) extends Encoder[U] {
	def apply(preferences: EncoderPreferences, level: Int)(target: EncoderResult) = for {
		(previousText, previousReferences, p :: previousPending) <- target
		if universe.runtimeMirror(p.getClass.getClassLoader).reflect(p).symbol.toType <:< tt.tpe
		(nextText, nextReferences, nextPending) <- before(preferences, level)(Try(previousText, previousReferences, f(p.asInstanceOf[U]) ::: previousPending))
	} yield (nextText, nextReferences + (p, 0 until nextText.size), nextPending)
}

case class Or[T: TypeTag, U >: T: TypeTag, V <: U](some: Encoder[T], other: Encoder[V]) extends Encoder[U] {
	def apply(preferences: EncoderPreferences, level: Int)(target: EncoderResult) = some(preferences, level)(target).orElse(other(preferences, level)(target))
}

case class RepSep[T: TypeTag, S: TypeTag](body: Encoder[T], separator: Encoder[S]) extends Encoder[List[T]] {
	def apply(preferences: EncoderPreferences, level: Int)(target: EncoderResult) = for {
		(_, references, (ps: List[_]) :: _) <- target
		encoder = if (ps.isEmpty) Id else 1.until(ps.size).map{ _ => separator ~ body }.fold(body)(_ ~ _)
		result <- encoder(preferences, level)(Try("", references, ps))
	} yield result
}

case class Subcontext[T: TypeTag](body: Encoder[T]) extends Encoder[List[Any]] {
	def apply(preferences: EncoderPreferences, level: Int)(target: EncoderResult) = for {
		(text, references, pending) <- body(preferences, level + 1)(target)
	} yield (if(text.trim.isEmpty) text else s"\n$text\n", references.map{ case (k, v) => k -> (v.start + 1 until v.end + 1) }, pending)
}

case class Append[L: TypeTag, R: TypeTag](left: Encoder[L], right: Encoder[R]) extends Encoder[List[Any]] {
	def apply(preferences: EncoderPreferences, level: Int)(target: EncoderResult) = {
		for {
			previous @ (previousText, previousReferences, previousPending) <- left(preferences, 0)(target)
			(nextText, nextReferences, nextPending) <- right(preferences, 0)(Try("", previousReferences, previousPending))
			shiftedNextReferences = nextReferences.map{ case (k, v) => k -> (v.start + previousText.size until v.end + previousText.size) }
			allShiftedReferences : IdentityMap[Any,Range] = shiftedNextReferences ++ previousReferences
			tabs = tabulation(preferences,level)
		} yield (tabs + previousText + nextText, allShiftedReferences.map{ case (k, v) => k -> (v.start + tabs.size until v.end + tabs.size) }, nextPending)
	}
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// PREFERENCES
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

class EncoderPreferences(val spacing: Map[Location, Boolean])

trait Location
case class Before(target: Encoder[Any]) extends Location
case class After(target: Encoder[Any]) extends Location