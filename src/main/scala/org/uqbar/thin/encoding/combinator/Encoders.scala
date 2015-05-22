package org.uqbar.thin.encoding.combinator

import scala.language.implicitConversions
import scala.util.Try

trait Encoders {
	def preferences: EncoderPreferences
	def terminals: Map[Symbol, String]

	def encode[T](encoder: Encoder[T])(target: T) = encoder.encode(preferences)(target).map(_ referencing target)
	
	protected def $[T] = new Binder[T]
	protected class Binder[T] {
		def ~>(right: Encoder[T]): Encoder[T] = right
	}

	protected abstract class EncoderSyntaxSugar[T, U <% Encoder[T]] {
		def left: U
		def ~(right: Encoder[T]): Encoder[T] = Append(left, right)
		def ~~(right: Encoder[T]): Encoder[T] = Append(left, Subcontext(right))
		def |[V <: T, U >: V, R <: U](right: Encoder[R]): Encoder[U] = Or(left,right)
		def *~(separator: Encoder[Any]): Encoder[List[T]] = RepSep(left, separator)
		def * = RepSep(left,Constant(""))
		def apply[U](f: U => T): Encoder[U] = Transform(left)(f)
	}
	
	implicit protected class ExtEncoder[T](val left: Encoder[T]) extends EncoderSyntaxSugar[T, Encoder[T]]
	implicit protected class ExtString[T](val left: String) extends EncoderSyntaxSugar[T, String]
	implicit protected class ExtSymbol[T](val left: Symbol) extends EncoderSyntaxSugar[T, Symbol]
	
	implicit protected def StringToEncoder(s: String): Encoder[Any] = Constant(s)
	implicit protected def SymbolToEncoder(s: Symbol): Encoder[Any] = Constant(terminals(s))
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// ENCODERS
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

abstract class Encoder[-T] {
	def encode(preferences: EncoderPreferences, level: Int = 0)(target: T): EncoderResult

	protected def tabulation(preferences: EncoderPreferences, level: Int) = "\t" * level
}

case object & extends Encoder[Any] {
	def encode(preferences: EncoderPreferences, level: Int)(target: Any) = EncoderResult(tabulation(preferences, level) + target)
}

case class Constant(value: String) extends Encoder[Any] {
	def encode(preferences: EncoderPreferences, level: Int)(target: Any) = {
		val beforeSpace = if (preferences.spacing(Before(Constant(value)))) " " else ""
		val afterSpace = if (preferences.spacing(After(Constant(value)))) " " else ""

		EncoderResult(tabulation(preferences, level) + beforeSpace + value + afterSpace)
	}
}

case class Append[T](left: Encoder[T], right: Encoder[T]) extends Encoder[T] {
	def encode(preferences: EncoderPreferences, level: Int)(target: T) = for {
		tabs <- EncoderResult(tabulation(preferences, level))
		previous <- left.encode(preferences, 0)(target)
		next <- right.encode(preferences, 0)(target)
	} yield tabs ++ previous ++ next
}

case class Transform[T, S](before: Encoder[S])(f: T => S) extends Encoder[T] {
	def encode(preferences: EncoderPreferences, level: Int)(target: T) = {
		val transformedTarget = f(target)
		for { next <- before.encode(preferences, level)(transformedTarget) } yield next.referencing(transformedTarget)
	}
}

case class Or[T, -L <: T, -R <: T](some: Encoder[L], other: Encoder[R]) extends Encoder[T] {
	def encode(preferences: EncoderPreferences, level: Int)(target: T) = {
		val left = try some.encode(preferences, level)(target.asInstanceOf[L]) catch { case e: Exception => Try{ throw e } }
		val right = try other.encode(preferences, level)(target.asInstanceOf[R]) catch { case e: Exception => Try{ throw e } }
		left.orElse(right)
	}
}

case class RepSep[-T](body: Encoder[T], separator: Encoder[Any]) extends Encoder[List[T]] {
	def encode(preferences: EncoderPreferences, level: Int)(target: List[T]) =
		if (target.isEmpty) EncoderResult()
		else (body.encode(preferences, level)(target.head).map{ _ referencing target.head } /: target.tail){ (previous, elem) =>
			for {
				tabs <- EncoderResult(tabulation(preferences, level))
				previous <- previous
				separator <- separator.encode(preferences, level)(())
				body <- body.encode(preferences, level)(elem)
			} yield tabs ++ previous ++ separator ++ body.referencing(elem)
		}
}

case class Subcontext[-T](body: Encoder[T]) extends Encoder[T] {
	def encode(preferences: EncoderPreferences, level: Int)(target: T) = for {
		br <- EncoderResult("\n")
		body <- body.encode(preferences, level + 1)(target)
	} yield if (body._1.isEmpty) body else br ++ body ++ br
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// PREFERENCES
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

class EncoderPreferences(val spacing: Map[Location, Boolean])

trait Location
case class Before(target: Encoder[Any]) extends Location
case class After(target: Encoder[Any]) extends Location