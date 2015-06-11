package org.uqbar.thin.encoding.combinator

import scala.language.implicitConversions
import scala.util.Try

trait Encoders {
	def preferences: EncoderPreferences
	def terminals: Map[Symbol, String]

	def encode[T](encoder: Encoder[T])(target: T) = encoder.encode(target)(preferences).map(_ referencing target)

	protected def $[T] = new Binder[T]
	protected class Binder[T] {
		def ~>(right: Encoder[T]): Encoder[T] = right
	}

	protected abstract class EncoderSyntaxSugar[T, U <% Encoder[T]] {
		def left: U
		def |[V <: T, U >: V, R <: U](right: Encoder[R]): Encoder[U] = Or(left, right)
		def ~(right: Encoder[T]): Encoder[T] = Append(left, right)
		def * : Encoder[List[T]] = RepSep(left, Constant(""))
		def *~(separator: Encoder[Any]): Encoder[List[T]] = RepSep(left, separator)
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
	def encode(target: T, level: Int = 0)(implicit preferences: EncoderPreferences): EncoderResult

	protected def tabulate(target: EncoderResult, level: Int)(implicit preferences: EncoderPreferences): EncoderResult = for {
		tabulation <- EncoderResult(preferences.tabulation(level))
		content <- target
	} yield tabulation ++ content

	def format(result: EncoderResult, target: T)(implicit preferences: EncoderPreferences): EncoderResult = for {
		beforeSpace <- EncoderResult(preferences.space(Before(this), target))
		afterSpace <- EncoderResult(preferences.space(After(this), target))
		beforeLineBreaks <- EncoderResult(preferences.lineBreak(After(this), target))
		afterLineBreaks <- EncoderResult(preferences.lineBreak(After(this), target))
		content <- result
	} yield beforeLineBreaks ++ beforeSpace ++ content ++ afterLineBreaks ++ afterSpace
}

case object & extends Encoder[Any] {
	def encode(target: Any, level: Int)(implicit preferences: EncoderPreferences) = tabulate(format(EncoderResult(target.toString), target), level)
}

case class Constant(value: String) extends Encoder[Any] {
	def encode(target: Any, level: Int)(implicit preferences: EncoderPreferences) = tabulate(format(EncoderResult(value), target), level)
}

case class Append[T](left: Encoder[T], right: Encoder[T]) extends Encoder[T] {
	def encode(target: T, level: Int)(implicit preferences: EncoderPreferences) = tabulate(format(for {
		previous <- left.encode(target)
		next <- right.encode(target)
	} yield previous ++ next, target), level)
}

case class Transform[T, S](before: Encoder[S])(f: T => S) extends Encoder[T] {
	def encode(target: T, level: Int)(implicit preferences: EncoderPreferences) = {
		val transformedTarget = f(target)
		format(for { next <- before.encode(transformedTarget, level) } yield next.referencing(transformedTarget), target)
	}
}

case class Or[T, -L <: T, -R <: T](left: Encoder[L], right: Encoder[R]) extends Encoder[T] {
	def encode(target: T, level: Int)(implicit preferences: EncoderPreferences) = format(
		Try{ target.asInstanceOf[L] }.flatMap{ target => left.encode(target, level) } orElse
			Try{ target.asInstanceOf[R] }.flatMap{ target => right.encode(target, level) }, target)
}

case class RepSep[-T](body: Encoder[T], separator: Encoder[Any]) extends Encoder[List[T]] {
	def encode(target: List[T], level: Int)(implicit preferences: EncoderPreferences) =
		if (target.isEmpty) EncoderResult()
		else format((body.encode(target.head, level).map{ _ referencing target.head } /: target.tail){ (previous, elem) =>
			tabulate(for {
				previous <- previous
				separator <- separator.encode((), level)
				break <- EncoderResult(preferences.lineBreak(InBetween(body), elem))
				body <- body.encode(elem, level)
			} yield previous ++ separator ++ break ++ body.referencing(elem), level)
		}, target)
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// PREFERENCES
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

class EncoderPreferences(
	spacing: Set[Location[_]],
	tabulationSequence: String,
	tabulationSize: Int,
	lineBreaks: Map[Location[_], Int]) {
	def space[T](location: Location[T], target: T) = spacing.collectFirst{ case l: Location[T] if l.matches(location, target) => " " } getOrElse ""
	def lineBreak[T](location: Location[T], target: T) = "\n" * lineBreaks.collect{ case (l: Location[T], count) if l.matches(location, target) => count }.sum
	def tabulation(level: Int) = tabulationSequence * tabulationSize * level
}

trait Location[-T] {
	def matches[U <: T](other: Location[U], targetValue: U): Boolean = this match {
		case cl @ ConditionalLocation(`other`) => cl.condition(targetValue)
		case _ => this == other
	}
}
case class Before[-T](target: Encoder[T]) extends Location[T]
case class After[-T](target: Encoder[T]) extends Location[T]
case class InBetween[-T](target: Encoder[T]) extends Location[T]
case class ConditionalLocation[-T](location: Location[T])(val condition: T => Boolean) extends Location[T]
