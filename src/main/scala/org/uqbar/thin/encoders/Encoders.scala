package org.uqbar.thin.encoders

import scala.language.existentials
import scala.language.implicitConversions
import scala.util.Try
import org.uqbar.utils.collections.immutable.IdentityMap
import Encoder.StringToEncoderResult
import org.uqbar.thin.grammars.On
import org.uqbar.thin.grammars.After
import org.uqbar.thin.grammars.Before
import org.uqbar.thin.grammars.InBetween
import org.uqbar.thin.grammars.GrammarPreferences


object Encoder {
	implicit def StringToEncoderResult(s: String) = EncoderResult(s)
}

abstract class Encoder[-T](grammarPreferences: GrammarPreferences) {
	def preferences = grammarPreferences.encodingPreferences
	def terminals = grammarPreferences.terminals
	
	def apply(target: T, level: Int = 0) = for {
		content <- doEncode(target, level + preferences.tabulationLevelIncrement(On(this) on target))
	} yield formated(content referencing target, target)

	protected def formated(result: EncoderResult, target: T) =
		preferences.lineBreak(After(this) on target) ++
			preferences.space(Before(this) on target) ++
			result ++
			preferences.lineBreak(After(this) on target) ++
			preferences.space(After(this) on target)

	protected def tabulate(target: EncoderResult, level: Int): EncoderResult = preferences.tabulation(level) ++ target

	protected def doEncode(target: T, level: Int): Try[EncoderResult]
}

case class Empty()(grammarPreferences: GrammarPreferences) extends Encoder[Any](grammarPreferences) {
	protected def doEncode(target: Any, level: Int) = Try("")
}

case class &()(grammarPreferences: GrammarPreferences) extends Encoder[Any](grammarPreferences) {
	protected def doEncode(target: Any, level: Int) = Try(tabulate(target.toString, level))
}

case class Constant(terminal: Symbol)(grammarPreferences: GrammarPreferences) extends Encoder[Any](grammarPreferences) {
	protected def doEncode(target: Any, level: Int) = Try(tabulate(terminals(terminal), level))
}

class Append[T,S](left: =>Encoder[T], right: =>Encoder[S])(grammarPreferences: GrammarPreferences) extends Encoder[(T,S)](grammarPreferences) {
	protected def doEncode(target: (T,S), level: Int) = for {
		previous <- left(target._1, level)
		next <- right(target._2)
	} yield previous ++ next
}

class Transform[T, S](before: =>Encoder[S])(f: T => S)(grammarPreferences: GrammarPreferences) extends Encoder[T](grammarPreferences) {
	protected def doEncode(target: T, level: Int) = for {
		next <- before(f(target), level)
	} yield next
}

class Or[T, -L <: T, -R <: T](left: =>Encoder[L], right: =>Encoder[R])(grammarPreferences: GrammarPreferences) extends Encoder[T](grammarPreferences) {
	protected def doEncode(target: T, level: Int) =
		Try(target.asInstanceOf[L]).flatMap{ left(_, level) } orElse Try(target.asInstanceOf[R]).flatMap{ right(_, level) }
}

class RepSep[-T](body: =>Encoder[T], separator: =>Encoder[Unit])(grammarPreferences: GrammarPreferences) extends Encoder[Seq[T]](grammarPreferences) {

	protected def doEncode(target: Seq[T], level: Int) = {
		val sortedTarget = preferences.sortOrder(this).fold(target){target.sortWith(_)}
		if (sortedTarget.isEmpty) Try("")
		else ((body(sortedTarget.head, level), sortedTarget.head) /: sortedTarget.tail){
			case ((previous, previousElem), elem) =>
				(for {
					previous <- previous
					separator <- separator(())
					elemBody <- body(elem, level)
				} yield previous ++ separator.dropReferences ++ preferences.lineBreak(InBetween(this) on (previousElem, elem, sortedTarget)) ++ elemBody, elem)
		}._1
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
