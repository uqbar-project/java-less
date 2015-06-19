package org.uqbar.thin.encoding.combinator

import scala.language.existentials
import scala.language.implicitConversions
import scala.util.Try

import org.uqbar.utils.collections.immutable.IdentityMap

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
	def encode(target: T, level: Int = 0)(implicit preferences: EncoderPreferences, terminals: Map[Symbol, String]) = for {
		content <- doEncode(target, level + preferences.tabulationLevelIncrements(InBetween(this)))
	} yield formated(content referencing target, target)

	protected def formated(result: EncoderResult, target: T)(implicit preferences: EncoderPreferences) =
		preferences.lineBreak(After(this), target) ++
		preferences.space(Before(this), target) ++
		result ++
		preferences.lineBreak(After(this), target) ++
		preferences.space(After(this), target)

	protected def tabulate(target: EncoderResult, level: Int)(implicit preferences: EncoderPreferences): EncoderResult = preferences.tabulation(level) ++ target

	protected def doEncode(target: T, level: Int)(implicit preferences: EncoderPreferences, terminals: Map[Symbol, String]): Try[EncoderResult]
}

case object Empty extends Encoder[Any] {
	protected def doEncode(target: Any, level: Int)(implicit preferences: EncoderPreferences, terminals: Map[Symbol, String]) = Try("")
}

case object & extends Encoder[Any] {
	protected def doEncode(target: Any, level: Int)(implicit preferences: EncoderPreferences, terminals: Map[Symbol, String]) = Try(tabulate(target.toString, level))
}

case class Constant(terminal: Symbol) extends Encoder[Any] {
	protected def doEncode(target: Any, level: Int)(implicit preferences: EncoderPreferences, terminals: Map[Symbol, String]) = Try(tabulate(terminals(terminal), level))
}

case class Append[T](left: Encoder[T], right: Encoder[T]) extends Encoder[T] {
	protected def doEncode(target: T, level: Int)(implicit preferences: EncoderPreferences, terminals: Map[Symbol, String]) = for {
		previous <- left.encode(target, level)
		next <- right.encode(target)
	} yield previous ++ next
}

case class Transform[T, S](before: Encoder[S])(f: T => S) extends Encoder[T] {
	protected def doEncode(target: T, level: Int)(implicit preferences: EncoderPreferences, terminals: Map[Symbol, String]) = for {
		next <- before.encode(f(target), level)
	} yield next
}

case class Or[T, -L <: T, -R <: T](left: Encoder[L], right: Encoder[R]) extends Encoder[T] {
	protected def doEncode(target: T, level: Int)(implicit preferences: EncoderPreferences, terminals: Map[Symbol, String]) =
		Try(target.asInstanceOf[L]).flatMap{ left.encode(_, level) } orElse Try(target.asInstanceOf[R]).flatMap{ right.encode(_, level) }
}

case class RepSep[-T](body: Encoder[T], separator: Encoder[Unit]) extends Encoder[List[T]] {
	protected def doEncode(target: List[T], level: Int)(implicit preferences: EncoderPreferences, terminals: Map[Symbol, String]) =
		if (target.isEmpty) Try("")
		else (body.encode(target.head, level) /: target.tail){ (previous, elem) =>
			for {
				previous <- previous
				separator <- separator.encode(())
				elemBody <- body.encode(elem, level)
			} yield previous ++ separator.dropReferences ++ preferences.lineBreak(InBetween(body), elem) ++ elemBody
		}
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

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// ENCODER RESULTS
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

case class EncoderResult(text: String = "", references: IdentityMap[Any, Range] = IdentityMap()) {
	implicit class ExtendedIdentityMap(m: IdentityMap[Any, Range]) {
	}

	def ++(text: String): EncoderResult = this ++ EncoderResult(text)
	def ++(other: EncoderResult) = {
		def shifted(m: IdentityMap[Any, Range], n: Int): IdentityMap[Any, Range] = m.map{ case (k, v) => (k, if(v.isInclusive) v.start + n to v.end + n else v.start + n until v.end + n) }
		
		EncoderResult(text + other.text, shifted(other.references, text.size) ++ references)
	}

	def referencing(target: Any) = {
		def fillingCount(s: Iterator[_]) = s.takeWhile(" \t\n".contains(_)).size

		val start = fillingCount(text.iterator)
		val end = text.size - fillingCount(text.reverseIterator)

		copy(references = references + (target, start until end))
	}

	def dropReferences = EncoderResult(text)
}
