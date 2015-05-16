package org.uqbar.thin.encoding.combinator

import scala.language.implicitConversions
import scala.util.Try

trait Encoders {
	def preferences: EncoderPreferences
	def terminals: Map[Symbol, String]

	implicit protected def StringToEncoder(s: String): Encoder[Any] = Constant(s)
	implicit protected def SymbolToEncoder(s: Symbol): Encoder[Any] = Constant(terminals(s))

	def encode[T](encoder: Encoder[T])(target: T) = encoder(preferences)(target).map(_ referencing target)
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// ENCODERS
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

abstract class Encoder[-T] {
	def apply(preferences: EncoderPreferences, level: Int = 0)(target: T): EncoderResult

	def apply[U](f: U => T): Encoder[U] = Transform(this)(f)

	def ~[U <: T](other: Encoder[U]): Encoder[U] = Append(this, other)

	def ~~[U <: T](other: Encoder[U]): Encoder[U] = Append(this, Subcontext(other))

	def |[V <: T, U >: V, R <: U](other: Encoder[R]): Encoder[U] = Or(this, other)

	def * = *~(Constant(""))

	def *~(separator: Encoder[Any]): Encoder[List[T]] = RepSep(this, separator)

	protected def tabulation(preferences: EncoderPreferences, level: Int) = "\t" * level
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
		tabs <- EncoderResult(tabulation(preferences, level))
		previous <- left(preferences, 0)(target)
		next <- right(preferences, 0)(target)
	} yield tabs ++ previous ++ next
}

case class Transform[-T, S](before: Encoder[S])(f: T => S) extends Encoder[T] {
	def apply(preferences: EncoderPreferences, level: Int)(target: T) = {
		val transformedTarget = f(target)
		for { next <- before(preferences, level)(transformedTarget) } yield next.referencing(transformedTarget)
	}
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
		else (body(preferences, level)(target.head).map{_ referencing target.head} /: target.tail){ (previous, elem) =>
			for {
				tabs <- EncoderResult(tabulation(preferences, level))
				previous <- previous
				separator <- separator(preferences, level)(())
				body <- body(preferences, level)(elem)
			} yield tabs ++ previous ++ separator ++ body.referencing(elem)
		}
}

case class Subcontext[-T](body: Encoder[T]) extends Encoder[T] {
	def apply(preferences: EncoderPreferences, level: Int)(target: T) = for {
		br <- EncoderResult("\n")
		body <- body(preferences, level + 1)(target)
	} yield if (body._1.isEmpty) body else br ++ body ++ br
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// PREFERENCES
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

class EncoderPreferences(val spacing: Map[Location, Boolean])

trait Location
case class Before(target: Encoder[Any]) extends Location
case class After(target: Encoder[Any]) extends Location