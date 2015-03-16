package org.uqbar.thin

import scala.language.implicitConversions
import scala.collection.JavaConversions._
import java.util.IdentityHashMap

package object javaless {

	val DefaultTerminals = Map(
		'class -> "class",
		'contextOpen -> "{",
		'contextClose -> "}"
	)

	type Identifier = String
	
	implicit class ExtendedIdentityHashMap(inner: IdentityHashMap[SyntaxElement, Range]) {
		def ++(other: IdentityHashMap[SyntaxElement, Range]) = {
			val next = new IdentityHashMap[SyntaxElement, Range]
			next.putAll(inner)
			next.putAll(other)
			next
		}
		def shifted(ammount: Int) = {
			val next = new IdentityHashMap[SyntaxElement, Range]
			for (entry <- inner.entrySet) next.put(entry.getKey, entry.getValue.start + ammount until entry.getValue.end + ammount)
			next
		}

		def updated(key: SyntaxElement, value: Range) = {
			val updatedReferences = new IdentityHashMap[SyntaxElement, Range]
			updatedReferences.putAll(inner)
			updatedReferences.put(key, value)
			updatedReferences
		}
	}
}