package org.uqbar.thin.encoding.combinator

import java.io.PrintWriter
import java.io.StringWriter
import scala.language.implicitConversions
import scala.util.Failure
import scala.util.Success
import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import scala.util.Try

trait EncoderMatchers extends Matchers {

	protected case class resultIn(tabulatedExpectedText: String, expectedPending: List[Any] = Nil)(expectedReferencesSeq: (Any, Range)*) extends Matcher[Try[EncoderResult]] {
		def apply(target: Try[EncoderResult]) = target.matches(tabulatedExpectedText, expectedReferencesSeq.toMap)
	}

	case class beEncodedTo[T](tabulatedExpectedText: String)(expectedReferencesSeq: (Any, Range)*)(implicit encoder: Encoder[T], preferences: EncoderPreferences, terminals: Map[Symbol, String]) extends Matcher[T] {
		def apply(target: T) = encoder.encode(target).matches(tabulatedExpectedText, expectedReferencesSeq.toMap)
	}

	case class beEncoded(implicit encoder: Encoder[T], preferences: EncoderPreferences, terminals: Map[Symbol, String]) extends Matcher[T] {
		def apply(target: T) = {
			val result = encoder encode target

			MatchResult(
				result.isSuccess,
				s"Encode was not a success: $result",
				s"Encode was a success: $result"
			)
		}
	}

	protected implicit class EncoderResultExt(target: Try[EncoderResult]) {
		def matches(tabulatedExpectedText: String, expectedReferences: Map[Any, Range]) = {

			val unwantedTabulation = """^\n?([\t| ]*)[^\$]*""".r.unapplySeq(tabulatedExpectedText).fold("")(_.head)
			val expectedTextParts = tabulatedExpectedText.
				split("\n").
				map(s => if (s.startsWith(unwantedTabulation)) s.replaceFirst(unwantedTabulation, "") else s).
				toList

			val expectedText = expectedTextParts.zipWithIndex.filterNot{
				case ("", 0) => true
				case (part, n) => n == 0 && part.isEmpty || n == expectedTextParts.size - 1 && part.forall{ " \t" contains _ }
			}.map(_._1).mkString("\n")

			val (success, message) = target match {
				case Success(EncoderResult(text, _)) if text != expectedText => false -> s"Encoded text: ${pretty(text)} did not match expected text: ${pretty(expectedText)}"
				case Success(EncoderResult(_, references)) =>
					val unexpectedReferences = references.filterNot(expectedReferences isDefinedAt _._1)
					val missedReferences = expectedReferences.filterNot(references isDefinedAt _._1)
					val wrongReferences = references.filter{ case (key, value) => expectedReferences.get(key).exists(_ != value) }
					if (unexpectedReferences.nonEmpty) false -> s"Encoding yielded references to unexpected objects: ${pretty(unexpectedReferences)}"
					else if (missedReferences.nonEmpty) false -> s"Encoding didn't yield references to expected objects: ${pretty(missedReferences)}"
					else if (wrongReferences.nonEmpty) false -> s"Encoding yielded wrong references: ${pretty(wrongReferences, expectedReferences)}"
					else true -> "Encoded was as expected"
				case Failure(e) =>
					val stack = new StringWriter
					e.printStackTrace(new PrintWriter(stack))
					false -> s"Encoding failed because of $e: ${stack.toString}}"
			}
			MatchResult(success, message, message)
		}

		protected def pretty(text: String): String = s""""${text.replaceAll(" ", "·").replaceAll("\t", "»").replaceAll("\n", "¶\n")}""""
		protected def pretty(range: Range): String = s"${if (range.isEmpty) range.start else range.head} to ${if (range.isEmpty) range.end else range.last}"
		protected def pretty(references: Map[Any, Range]): String = references.map(r => s"${r._1}: ${pretty(r._2)}").mkString("[", ", ", "]")
		protected def pretty(references: Map[Any, Range], expectedReferences: Map[Any, Range]): String = references.map{ r => s"${r._1}: ${pretty(r._2)} wasn't ${pretty(expectedReferences(r._1))}" }.mkString("[", ", ", "]")
	}

}