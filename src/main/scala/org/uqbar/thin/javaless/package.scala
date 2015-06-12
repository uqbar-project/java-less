package org.uqbar.thin

import scala.language.implicitConversions

package object javaless {

	val DefaultTerminals = Map(
		'class -> "class",
		'contextOpen -> "{", 
		'contextClose -> "}",
		'argumentOpen -> "(",
		'argumentClose -> ")",
		'argumentSeparator -> ",",
		'public -> "public",
		'typeApplication -> ""
	)
	
	type Identifier = String

}