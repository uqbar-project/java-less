package org.uqbar.thin.grammars

import org.uqbar.thin.encoders.Encoder
import scala.util.parsing.combinator.RegexParsers
import org.uqbar.thin.encoders.{ Constant => TerminalEncoder, & => ValueEncoder, RepSep => RepeatEncoder }
import org.uqbar.thin.encoders.{ Empty => EmptyEncoder, Append => AppendEncoder, Transform => TransformEncoder, Or => OrEncoder }
import scala.util.matching.Regex
import scala.language.higherKinds

trait Grammars extends RegexParsers {

	trait Grammar[-E, +D] {
		def encoder(options: GrammarPreferences): Encoder[E]
		def decoder(options: GrammarPreferences): CodeParser[D]
	}

	object Empty extends Grammar[Any,String] {
		def encoder(options: GrammarPreferences) = new EmptyEncoder()(options)
		def decoder(options: GrammarPreferences) = new CodeParser("".r)
	}
	
	object Fail extends Grammar[Any,Nothing] {
		def encoder(options: GrammarPreferences) = throw new RuntimeException("grammar reached fail point")
		def decoder(options: GrammarPreferences) = throw new RuntimeException("grammar reached fail point")
	}
	
	case class Value(restriction: Regex) extends Grammar[String,String] {
		def encoder(options: GrammarPreferences) = ValueEncoder()(options)
		def decoder(options: GrammarPreferences) = new CodeParser(restriction)
	}

	class Terminal(key: Symbol) extends Grammar[Any,String] {
		def encoder(options: GrammarPreferences) = new TerminalEncoder(key)(options)
		def decoder(options: GrammarPreferences) = new CodeParser(if (options.terminals(key) == " ") "".r else options.terminals(key)) //TODO: Find better way to handle the space as parseable value
	}

	class Append[L,X,R,Y](_left: => Grammar[L,X], _right: => Grammar[R,Y]) extends Grammar[(L,R),(X,Y)] {
		lazy val left = _left
		lazy val right = _right
		def encoder(options: GrammarPreferences) = new AppendEncoder(left.encoder(options), right.encoder(options))(options)
		def decoder(options: GrammarPreferences) = {
			lazy val leftDecoder = left.decoder(options)
			lazy val rightDecoder = right.decoder(options)
			new CodeParser(leftDecoder.inner ~ rightDecoder.inner ^^{case l ~ r => (l,r)})
		}
	}
	
	class Transform[U,V,T,S](target: =>Grammar[T,S])(tx: U => T)(xt: S => V) extends Grammar[U,V] {
		def encoder(options: GrammarPreferences) = new TransformEncoder[U,T](target.encoder(options))(tx)(options)
		def decoder(options: GrammarPreferences) = new CodeParser(target.decoder(options).inner.^^(xt))
	}

	class Or[T <: V, U <: V, V, X<:Z, Y<:Z, Z](left: => Grammar[T,X], right: => Grammar[U,Y]) extends Grammar[V,Z] {
		def encoder(options: GrammarPreferences) = new OrEncoder(left.encoder(options),right.encoder(options))(options)
		def decoder(options: GrammarPreferences) = new CodeParser(left.decoder(options).inner | right.decoder(options).inner)
	}
	
	class Repeat[T,S](body: =>Grammar[T,S], separator: =>Grammar[Any,Any] = Empty) extends Grammar[List[T],List[S]] {
		def encoder(options: GrammarPreferences) = new RepeatEncoder(body.encoder(options),separator.encoder(options))(options) 
		def decoder(options: GrammarPreferences) = new CodeParser(repsep(body.decoder(options).inner,separator.decoder(options).inner))
	}

	class CodeParser[+T](_inner: =>Parser[T]) {
		lazy val inner = _inner
		def apply(input: String) = parseAll(inner, input)
	}

}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// PREFERENCES
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

case class GrammarPreferences(
	terminals: Map[Symbol, String],
	encodingPreferences: EncodingPreferences
)

case class EncodingPreferences(
	protected val spacing: Set[LocationRule[Any]] = Set(),
	protected val tabulationSequence: String = "\t",
	protected val tabulationSize: Int = 1,
	protected val lineBreaks: Map[LocationRule[Any], Int] = Map(),
	protected val tabulationLevelIncrements: Map[LocationRule[Any], Int] = Map(),
	protected val sortOrders: Set[Order[_]] = Set()) {
	def tabulationLevelIncrement(locationKey: LocationKey[_]) = tabulationLevelIncrements.collectFirst{ case (l, i) if l.matches(locationKey) => i } getOrElse 0
	def space(locationKey: LocationKey[_]) = spacing.collectFirst{ case l if l.matches(locationKey) => " " } getOrElse ""
	def lineBreak(locationKey: LocationKey[_]) = "\n" * lineBreaks.collect{ case (l, count) if l.matches(locationKey) => count }.sum
	def tabulation(level: Int) = tabulationSequence * tabulationSize * level
	def sortOrder[T](target: Encoder[List[T]]) = sortOrders.collectFirst { case order @ Order(`target`) => order.criteria.asInstanceOf[(T, T) => Boolean] }
}

case class Order[T](target: Encoder[List[T]])(val criteria: (T, T) => Boolean)

trait Location[+T] {
	def on[U >: T](target: U) = LocationKey(this, target)
	def apply(condition: PartialFunction[Any, Boolean] = null) = LocationRule(this)(Option(condition))
}
case class After[T](target: Encoder[T]) extends Location[T]
case class Before[T](target: Encoder[T]) extends Location[T]
case class On[T](target: Encoder[T]) extends Location[T]
case class InBetween[T](target: Encoder[List[T]]) extends Location[(T, T, List[T])]

protected case class LocationKey[+T](val location: Location[T], val target: T)

protected case class LocationRule[+T](location: Location[T])(condition: Option[PartialFunction[Any, Boolean]]) {
	def matches[U >: T](key: LocationKey[U]) = {
		key.location == location && condition.forall{ condition => condition.applyOrElse(key.target, { _: Any => false })
		}
	}
}