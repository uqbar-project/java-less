package org.uqbar.thin.encoding

import scala.util.Try

import org.uqbar.utils.collections.immutable.IdentityMap

package object combinator {
	type EncoderResult = Try[(String, IdentityMap[Any, Range])]
	def EncoderResult(text: String = "", references: IdentityMap[Any, Range] = IdentityMap()): EncoderResult = Try(text, references)
}