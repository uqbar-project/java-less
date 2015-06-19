package org.uqbar.thin.encoding

import scala.language.implicitConversions

package object combinator {

	implicit def StringToEncoderResult(s: String) = EncoderResult(s)
	
}