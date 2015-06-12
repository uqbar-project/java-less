package org.uqbar.thin.encoding.combinator

import scala.language.implicitConversions
import scala.language.existentials
import scala.util.Try

trait Encoders {
	def preferences: EncoderPreferences
	def terminals: Map[Symbol, String]

	def encode[T](encoder: Encoder[T])(target: T) = encoder.encode(target)(preferences, terminals)

	protected def $[T] = new Binder[T]
	protected class Binder[T] {
		def ~>(right: Encoder[T]): Encoder[T] = right
	}

	protected abstract class EncoderSyntaxSugar[T, U <% Encoder[T]] {
		def left: U
		def |[V <: T, U >: V, R <: U](right: Encoder[R]): Encoder[U] = Or(left, right)
		def ~(right: Encoder[T]): Encoder[T] = Append(left, right)
		def * : Encoder[List[T]] = RepSep(left, Empty)
		def *~(separator: Encoder[Any]): Encoder[List[T]] = RepSep(left, separator)
		def apply[U](f: U => T): Encoder[U] = Transform(left)(f)
	}

	implicit protected class ExtEncoder[T](val left: Encoder[T]) extends EncoderSyntaxSugar[T, Encoder[T]]
	implicit protected class ExtSymbol[T](val left: Symbol) extends EncoderSyntaxSugar[T, Symbol]

	implicit protected def SymbolToEncoder(s: Symbol): Encoder[Any] = Constant(s)
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// ENCODERS
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

abstract class Encoder[-T] {
	def encode(target: T, level: Int = 0)(implicit preferences: EncoderPreferences, terminals: Map[Symbol,String]) = for {
		beforeLineBreaks <- EncoderResult(preferences.lineBreak(After(this), target))
		beforeSpace <- EncoderResult(preferences.space(Before(this), target))
		content <- doEncode(target, level + preferences.tabulationLevelIncrements(InBetween(this)))
		afterLineBreaks <- EncoderResult(preferences.lineBreak(After(this), target))
		afterSpace <- EncoderResult(preferences.space(After(this), target))
	} yield beforeLineBreaks ++ beforeSpace ++ content ++ afterLineBreaks ++ afterSpace

	protected def doEncode(target: T, level: Int)(implicit preferences: EncoderPreferences, terminals: Map[Symbol,String]): EncoderResult

	protected def tabulate(target: EncoderResult, level: Int)(implicit preferences: EncoderPreferences): EncoderResult = for {
		tabulation <- EncoderResult(preferences.tabulation(level))
		content <- target
	} yield tabulation ++ content
}

case object Empty extends Encoder[Any] {
	protected def doEncode(target: Any, level: Int)(implicit preferences: EncoderPreferences, terminals: Map[Symbol,String]) = EncoderResult()
}

case object & extends Encoder[Any] {
	protected def doEncode(target: Any, level: Int)(implicit preferences: EncoderPreferences, terminals: Map[Symbol,String]) = tabulate(EncoderResult(target.toString).map(_ referencing target), level)
}

case class Constant(terminal: Symbol) extends Encoder[Any] {
	protected def doEncode(target: Any, level: Int)(implicit preferences: EncoderPreferences, terminals: Map[Symbol,String]) = tabulate(EncoderResult(terminals(terminal)).map(_ referencing target), level)
}

case class Append[T](left: Encoder[T], right: Encoder[T]) extends Encoder[T] {
	protected def doEncode(target: T, level: Int)(implicit preferences: EncoderPreferences, terminals: Map[Symbol,String]) = tabulate(for {
		previous <- left.encode(target)
		next <- right.encode(target)
	} yield previous ++ next referencing target, level)
}

case class Transform[T, S](before: Encoder[S])(f: T => S) extends Encoder[T] {
	protected def doEncode(target: T, level: Int)(implicit preferences: EncoderPreferences, terminals: Map[Symbol,String]) = for {
		next <- before.encode(f(target), level)
	} yield next
}

case class Or[T, -L <: T, -R <: T](left: Encoder[L], right: Encoder[R]) extends Encoder[T] {
	protected def doEncode(target: T, level: Int)(implicit preferences: EncoderPreferences, terminals: Map[Symbol,String]) =
		Try{ target.asInstanceOf[L] }.flatMap{ target => left.encode(target, level) } orElse
			Try{ target.asInstanceOf[R] }.flatMap{ target => right.encode(target, level) }
}

case class RepSep[-T](body: Encoder[T], separator: Encoder[Unit]) extends Encoder[List[T]] {
	protected def doEncode(target: List[T], level: Int)(implicit preferences: EncoderPreferences, terminals: Map[Symbol,String]) =
		(if (target.isEmpty) EncoderResult()
		else (body.encode(target.head, level) /: target.tail){ (previous, elem) =>
			for {
				previous <- previous
				(separatorText,_) <- separator.encode(())
				separator <- EncoderResult(separatorText)
				break <- EncoderResult(preferences.lineBreak(InBetween(body), elem))
				body <- body.encode(elem, level)
			} yield previous ++ separator ++ break ++ body
		}).map(_ referencing target)
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// PREFERENCES
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

class EncoderPreferences(
	spacing: Set[Location],
	tabulationSequence: String,
	tabulationSize: Int,
	lineBreaks: Map[Location, Int],
	val tabulationLevelIncrements: Map[Location, Int]) {
	def space[T](location: Location, target: T) = spacing.collectFirst{ case l: Location if l.matches(location, target) => " " } getOrElse ""
	def lineBreak[T](location: Location, target: T) = "\n" * lineBreaks.collect{ case (l: Location, count) if l.matches(location, target) => count }.sum
	def tabulation(level: Int) = tabulationSequence * tabulationSize * level
}

trait Location {
	def matches(other: Location, targetValue: Any): Boolean = this == other
}
case class Before(target: Encoder[_]) extends Location
case class After(target: Encoder[_]) extends Location
case class InBetween(target: Encoder[_]) extends Location
case class ConditionalLocation(location: Location)(val condition: PartialFunction[Any, Boolean]) extends Location {
	override def matches(other: Location, targetValue: Any): Boolean = location == other && condition.isDefinedAt(targetValue) && condition(targetValue)
}
