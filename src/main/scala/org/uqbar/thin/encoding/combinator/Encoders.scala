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
		content <- doEncode(target, level + preferences.tabulationLevelIncrement(On(this) on target))
	} yield formated(content referencing target, target)

	protected def formated(result: EncoderResult, target: T)(implicit preferences: EncoderPreferences) =
		preferences.lineBreak(After(this) on target) ++
			preferences.space(Before(this) on target) ++
			result ++
			preferences.lineBreak(After(this) on target) ++
			preferences.space(After(this) on target)

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

	protected def doEncode(target: List[T], level: Int)(implicit preferences: EncoderPreferences, terminals: Map[Symbol, String]) = {
		val sortedTarget = preferences.sortOrder(this).fold(target){target.sortWith(_)}
		if (sortedTarget.isEmpty) Try("")
		else ((body.encode(sortedTarget.head, level), sortedTarget.head) /: sortedTarget.tail){
			case ((previous, previousElem), elem) =>
				(for {
					previous <- previous
					separator <- separator.encode(())
					elemBody <- body.encode(elem, level)
				} yield previous ++ separator.dropReferences ++ preferences.lineBreak(InBetween(this) on (previousElem, elem, sortedTarget)) ++ elemBody, elem)
		}._1
	}
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// PREFERENCES
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

case class EncoderPreferences(
	protected val spacing: Set[LocationRule[Any]] = Set(),
	protected val tabulationSequence: String = "\t",
	protected val tabulationSize: Int = 1,
	protected val lineBreaks: Map[LocationRule[Any], Int] = Map(),
	protected val tabulationLevelIncrements: Map[LocationRule[Any], Int] = Map(),
	protected val sortOrders: Set[Order[_]] = Set()
){
	def tabulationLevelIncrement(locationKey: LocationKey[_]) = tabulationLevelIncrements.collectFirst{ case (l, i) if l.matches(locationKey) => i } getOrElse 0
	def space(locationKey: LocationKey[_]) = spacing.collectFirst{ case l if l.matches(locationKey) => " " } getOrElse ""
	def lineBreak(locationKey: LocationKey[_]) = "\n" * lineBreaks.collect{ case (l, count) if l.matches(locationKey) => count }.sum
	def tabulation(level: Int) = tabulationSequence * tabulationSize * level
	def sortOrder[T](target: Encoder[List[T]]) = sortOrders.collectFirst { case order@Order(`target`) => order.criteria.asInstanceOf[(T,T)=>Boolean] }
}

case class Order[T](target: Encoder[List[T]])(val criteria: (T,T) => Boolean)

trait Location[+T] {
	def on[U>:T](target: U) = LocationKey(this, target)
	def apply(condition: PartialFunction[Any, Boolean] = null) = LocationRule(this)(Option(condition))
}
case class After[T](target: Encoder[T]) extends Location[T]
case class Before[T](target: Encoder[T]) extends Location[T]
case class On[T](target: Encoder[T]) extends Location[T]
case class InBetween[T](target: Encoder[List[T]]) extends Location[(T,T,List[T])]

protected case class LocationKey[+T](val location: Location[T], val target: T)

protected case class LocationRule[+T](location: Location[T])(condition: Option[PartialFunction[Any,Boolean]]) {
	def matches[U>:T](key: LocationKey[U]) = {
		key.location == location && condition.forall{ condition => condition.applyOrElse(key.target, {_: Any => false})
		}
	}
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// ENCODER RESULTS
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

case class EncoderResult(text: String = "", references: IdentityMap[Any, Range] = IdentityMap()) {
	implicit class ExtendedIdentityMap(m: IdentityMap[Any, Range]) {
	}

	def ++(text: String): EncoderResult = this ++ EncoderResult(text)
	def ++(other: EncoderResult) = {
		def shifted(m: IdentityMap[Any, Range], n: Int): IdentityMap[Any, Range] = m.map{ case (k, v) => (k, if (v.isInclusive) v.start + n to v.end + n else v.start + n until v.end + n) }

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
