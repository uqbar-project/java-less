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
		content <- doEncode(target, level + preferences.tabulationLevelIncrement(On(this)(target)))
	} yield formated(content referencing target, target)

	protected def formated(result: EncoderResult, target: T)(implicit preferences: EncoderPreferences) =
		preferences.lineBreak(After(this)(target)) ++
			preferences.space(Before(this)(target)) ++
			result ++
			preferences.lineBreak(After(this)(target)) ++
			preferences.space(After(this)(target))

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
		if (target.isEmpty) Try("")
		else ((body.encode(target.head, level), target.head) /: target.tail){
			case ((previous, previousElem), elem) =>
				(for {
					previous <- previous
					separator <- separator.encode(())
					elemBody <- body.encode(elem, level)
				} yield previous ++ separator.dropReferences ++ preferences.lineBreak(InBetween(this)(previousElem, elem, target)) ++ elemBody, elem)
		}._1
	}
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// PREFERENCES
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

class EncoderPreferences(
	spacing: Set[LocationRule[_]],
	tabulationSequence: String,
	tabulationSize: Int,
	lineBreaks: Map[LocationRule[_], Int],
	tabulationLevelIncrements: Map[LocationRule[_], Int]) {
	def tabulationLevelIncrement[T](locationKey: LocationKey[T]) = tabulationLevelIncrements.collectFirst{ case (l: LocationRule[T], i) if l.matches(locationKey) => i } getOrElse 0
	def space[T](locationKey: LocationKey[T]) = spacing.collectFirst{ case l: LocationRule[T] if l.matches(locationKey) => " " } getOrElse ""
	def lineBreak[T](locationKey: LocationKey[T]) = "\n" * lineBreaks.collect{ case (l: LocationRule[T], count) if l.matches(locationKey) => count }.sum
	def tabulation(level: Int) = tabulationSequence * tabulationSize * level
}

trait Location[T]
trait SimpleLocation[T] extends Location[T] {
	def apply(target: T) = SimpleLocationKey(this, target)
	def apply(condition: T => Boolean = null) = SimpleLocationRule(this)(Option(condition))
}
case class After[T](target: Encoder[T]) extends SimpleLocation[T]
case class Before[T](target: Encoder[T]) extends SimpleLocation[T]
case class On[T](target: Encoder[T]) extends SimpleLocation[T]
case class InBetween[T](target: Encoder[List[T]]) extends Location[List[T]] {
	def apply(left: T, right: T, context: List[T]) = InfixLocationKey(this, left, right, context)
	def apply(condition: PartialFunction[(T,T,List[T]), Boolean] = null) = InfixLocationRule(this)(Option(condition))
}

protected trait LocationKey[T]
protected case class SimpleLocationKey[T](location: Location[T], target: T) extends LocationKey[T]
protected case class InfixLocationKey[T](location: Location[List[T]], left: T, right: T, context: List[T]) extends LocationKey[List[T]]

protected trait LocationRule[T] { def matches(key: LocationKey[T]): Boolean }
protected case class SimpleLocationRule[T](location: Location[T])(condition: Option[T => Boolean] = None) extends LocationRule[T] {
	def matches(key: LocationKey[T]) = Option(key).
		collect{ case SimpleLocationKey(`location`, target) => target }.
		exists{ target => condition.forall{ _(target) } }
}
protected case class InfixLocationRule[T](location: Location[List[T]])(condition: Option[PartialFunction[(T, T, List[T]), Boolean]]) extends LocationRule[T] {
	def matches(key: LocationKey[T]) = Option(key).
		collect{ case key: InfixLocationKey[T] if key.location == location => (key.left, key.right, key.context) }.
		exists{ case (left,right, context) => condition.forall{ _.applyOrElse[(T,T,List[T]),Boolean]((left,right,context),{_ => false}) } }
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
