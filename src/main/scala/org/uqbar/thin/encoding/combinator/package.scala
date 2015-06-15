package org.uqbar.thin.encoding

import java.util.IdentityHashMap

import scala.collection.JavaConversions.asScalaSet
import scala.util.Try

import org.uqbar.utils.collections.immutable.IdentityMap

package object combinator {
	type EncoderResult = Try[(String, IdentityMap[Any, Range])]

	def EncoderResult(text: String = "", references: IdentityMap[Any, Range] = IdentityMap()): EncoderResult = Try(text, references)

	implicit class ExtendedEncoderResult(r: (String, IdentityMap[Any, Range])) {
		implicit class ExtendedIdentityMap(m: IdentityMap[Any, Range]) {
			def shifted(n: Int): IdentityMap[Any, Range] = m.map{ case (k, v) => k -> (v.start + n until v.end + n) }
		}

		def ++(other: (String, IdentityMap[Any, Range])): (String, IdentityMap[Any, Range]) = (r._1 + other._1, other._2.shifted(r._1.size) ++ r._2)

		def referencing(target: Any): (String, IdentityMap[Any, Range]) = {
			def fillingCount(s: Iterator[_]) = s.takeWhile(" \t\n".contains(_)).size
			val start = fillingCount(r._1.iterator)
			val end = r._1.size - fillingCount(r._1.reverseIterator)
			(r._1, r._2 + (target, start until end))
		}
	}

}