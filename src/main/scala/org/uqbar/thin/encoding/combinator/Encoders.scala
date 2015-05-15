package org.uqbar.thin.encoding.combinator

import scala.language.dynamics
import scala.language.existentials
import scala.language.implicitConversions
import scala.util.Try

import org.uqbar.utils.collections.immutable.IdentityMap

trait Encoders {
	def preferences: EncoderPreferences
	def terminals: Map[Symbol, String]

	implicit protected def StringToEncoder(s: String): Encoder[Any] = Constant(s)
	implicit protected def SymbolToEncoder(s: Symbol): Encoder[Any] = Constant(terminals(s))

	def encode[T](encoder: Encoder[T])(target: T) = encoder(preferences)(target)
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// ENCODERS
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

abstract class Encoder[-T] {
	def apply(preferences: EncoderPreferences, level: Int = 0)(target: T): EncoderResult

	def apply[U](f: U => T): Encoder[U] = Tx(this)(f)

	def ~[U <: T](other: Encoder[U]): Encoder[U] = Append(this, other)

	def ~~[U <: T](other: Encoder[U]): Encoder[U] = Append(this, Subcontext(other))

	def |[V <: T, U >: V, R <: U](other: Encoder[R]): Encoder[U] = Or(this, other)

	def * = *~(Constant(""))

	def *~(separator: Encoder[Any]): Encoder[List[T]] = RepSep(this, separator)

	protected def tabulation(preferences: EncoderPreferences, level: Int) = "\t" * level

	protected implicit class ExtendedIdentityMap(m: IdentityMap[Any, Range]) {
		def shifted(n: Int): IdentityMap[Any, Range] = m.map{ case (k, v) => k -> (v.start + n until v.end + n) }
	}
}

case object __ extends Encoder[Any] {
	def apply(preferences: EncoderPreferences, level: Int)(target: Any) = EncoderResult(tabulation(preferences, level) + target)
}

case class Constant(value: String) extends Encoder[Any] {
	def apply(preferences: EncoderPreferences, level: Int)(target: Any) = {
		val beforeSpace = if (preferences.spacing(Before(Constant(value)))) " " else ""
		val afterSpace = if (preferences.spacing(After(Constant(value)))) " " else ""

		EncoderResult(tabulation(preferences, level) + beforeSpace + value + afterSpace)
	}
}

case class Append[-T](left: Encoder[T], right: Encoder[T]) extends Encoder[T] {
	def apply(preferences: EncoderPreferences, level: Int)(target: T) = for {
		(previousText, previousReferences) <- left(preferences, 0)(target)
		(nextText, nextReferences) <- right(preferences, 0)(target)
		shiftedReferences: IdentityMap[Any, Range] = (nextReferences.shifted(previousText.size) ++ previousReferences)
		tabs = tabulation(preferences, level)
	} yield (tabs + previousText + nextText, shiftedReferences.shifted(tabs.size))
}

case class Tx[-T, S](before: Encoder[S])(f: T => S) extends Encoder[T] {
	def apply(preferences: EncoderPreferences, level: Int)(target: T) = for {
		(nextText, nextReferences) <- before(preferences, level)(f(target))
		references: IdentityMap[Any, Range] = nextReferences + (f(target), 0 until nextText.size)
	} yield (nextText, references)
}

case class Or[T, -L <: T, -R <: T](some: Encoder[L], other: Encoder[R]) extends Encoder[T] {
	def apply(preferences: EncoderPreferences, level: Int)(target: T) = {
		val left = try some(preferences, level)(target.asInstanceOf[L]) catch { case e: Exception => Try{ throw e } }
		val right = try other(preferences, level)(target.asInstanceOf[R]) catch { case e: Exception => Try{ throw e } }
		left.orElse(right)
	}
}

case class RepSep[-T](body: Encoder[T], separator: Encoder[Any]) extends Encoder[List[T]] {
	def apply(preferences: EncoderPreferences, level: Int)(target: List[T]) =
		if (target.isEmpty) EncoderResult()
		else (body(preferences, level)(target.head) /: target.tail){ (previous, elem) =>
			for {
				(previousText, previousReferences) <- previous
				(separatorText, separatorReferences) <- separator(preferences, level)(())
				(bodyText, bodyReferences) <- body(preferences, level)(elem)
				tabs = tabulation(preferences, level)
				elemReferences: IdentityMap[Any, Range] = bodyReferences.shifted(previousText.size + separatorText.size) ++ separatorReferences.shifted(previousText.size)
				nextReferences: IdentityMap[Any, Range] = elemReferences ++ previousReferences
			} yield (previousText + separatorText + bodyText, nextReferences)
		}
}

case class Subcontext[-T](body: Encoder[T]) extends Encoder[T] {
	def apply(preferences: EncoderPreferences, level: Int)(target: T) = for {
		(text, references) <- body(preferences, level + 1)(target)
	} yield (if (text.trim.isEmpty) text else s"\n$text\n", references.shifted(1))
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// PREFERENCES
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

class EncoderPreferences(val spacing: Map[Location, Boolean])

trait Location
case class Before(target: Encoder[Any]) extends Location
case class After(target: Encoder[Any]) extends Location