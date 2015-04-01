package org.uqbar.thin.encoding

import scala.util.Try

import org.uqbar.utils.collections.immutable.IdentityMap

package object combinator {
	type EncoderResult = Try[(String, IdentityMap[Any, Range], List[Any])]
	def EncoderResult(pending: Any*): EncoderResult = Try("", IdentityMap(), pending.toList)
}