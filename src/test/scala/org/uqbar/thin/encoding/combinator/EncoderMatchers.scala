package org.uqbar.thin.encoding.combinator

import java.io.PrintWriter
import java.io.StringWriter

import scala.language.implicitConversions
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

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

	implicit class TaggedString(val context: StringContext) {
		def beEncodedTo(tagSeq: Int*) = {
			val (tags, _) = ((Map[Any, Int]() -> 0) /: tagSeq.zip(context.parts)){
				case ((map, soFar), (tag, part)) => map.updated(tag, soFar + part.size) -> (soFar + part.size)
			}

			object TaggedStringReturn {
				def apply[T](expectedReferencesSeq: (Any, Range)*)(implicit encoder: Encoder[T], preferences: EncoderPreferences, terminals: Map[Symbol, String]) = {
					new Matcher[T] {
						def apply(target: T) = encoder.encode(target).matches(context.parts.mkString(""), expectedReferencesSeq.toMap, tags)
					}
				}
			}

			TaggedStringReturn
		}
	}

	protected implicit class EncoderResultExt(target: Try[EncoderResult]) {
		def matches(tabulatedExpectedText: String, references: Map[Any, Range], tabulatedTags: Map[Any, Int] = Map()) = {

			val (expectedText, tags) = trimUnwantedTabulation(tabulatedExpectedText, tabulatedTags)
			val expectedReferences = if (tags.nonEmpty) references.mapValues { range => if (range.isInclusive) tags(range.start) to tags(range.end) else tags(range.start) until tags(range.end) } else references

			val (success, message) = target match {
				case Success(EncoderResult(text, _)) if text != expectedText => false -> s"Encoded text: ${pretty(text)} did not match expected text: ${pretty(expectedText)}"
				case Success(EncoderResult(text, references)) =>
					val unexpectedReferences = references.filterNot(expectedReferences isDefinedAt _._1)
					val missedReferences = expectedReferences.filterNot(references isDefinedAt _._1)
					val wrongReferences = references.filter{ case (key, value) => expectedReferences.get(key).exists(_ != value) }
					if (unexpectedReferences.nonEmpty) false -> s"Encoding yielded references to unexpected objects: ${pretty(unexpectedReferences,text)}"
					else if (missedReferences.nonEmpty) false -> s"Encoding didn't yield references to expected objects: ${pretty(missedReferences,text)}"
					else if (wrongReferences.nonEmpty) false -> s"Encoding yielded wrong references: ${pretty(wrongReferences, expectedReferences,text)}"
					else true -> "Encoded was as expected"
				case Failure(e) =>
					val stack = new StringWriter
					e.printStackTrace(new PrintWriter(stack))
					false -> s"Encoding failed because of $e: ${stack.toString}}"
			}
			MatchResult(success, message, message)
		}

		protected def trimUnwantedTabulation(tabulatedText: String, tabulatedTags: Map[Any, Int]) = {
			val unwantedTabulation :: _ = """^\n?([\t| ]*)[^\$]*""".r.unapplySeq(tabulatedText).get

			def dropLeadingEmptyLine(lines: Seq[String], tags: Map[Any, Int]) =
				if (lines.nonEmpty && lines.head.isEmpty)
					lines.tail -> tags.map{ case (k, v) => (k, v - 1) }
				else lines -> tags

			def dropUnwantedTabulation(lines: Seq[String], tags: Map[Any, Int]) = {
				val (trimmedLines, trimmedTags, _) = ((List[String](), tags, -1) /: lines){
					case ((trimmedLines, tags, lastBreakIndex), part) =>
						val nextBreakIndex = lastBreakIndex + part.size + 1
						val nextTrimmedLines = trimmedLines :+ part.replaceFirst(unwantedTabulation, "")
						val nextTags = tags.map {
							case (k, v) if lastBreakIndex to nextBreakIndex contains v => (k, v - unwantedTabulation.size * (lines.take(trimmedLines.size).filter(_.startsWith(unwantedTabulation)).size + 1))
							case other => other
						}
						(nextTrimmedLines, nextTags, nextBreakIndex)
				}

				(trimmedLines, trimmedTags)
			}

			def dropTrailingEmptyLine(lines: Seq[String], tags: Map[Any, Int]) =
				if (lines.nonEmpty && lines.last.forall(" \t" contains _))
					lines.init -> tags
				else lines -> tags

			val (lines, tags) = (
				(dropLeadingEmptyLine _).tupled andThen
				(dropUnwantedTabulation _).tupled andThen
				(dropTrailingEmptyLine _).tupled
			)(tabulatedText.split("\n"), tabulatedTags)

			(lines.mkString("\n"), tags)
		}

		protected def pretty(text: String): String = s""""${text.replaceAll(" ", "·").replaceAll("\t", "»").replaceAll("\n", "¶")}""""
		protected def pretty(range: Range, text: String): String = s"[${range.start}, ${range.end}${if (range.isInclusive) "]" else ")"}: ${pretty(text.substring(range.start, if(range.isInclusive)range.end + 1 else range.end))}"
		protected def pretty(references: Map[Any, Range],text: String): String = references.map(r => s"${r._1}: ${pretty(r._2,text)}").mkString("{", ", ", "}")
		protected def pretty(references: Map[Any, Range], expectedReferences: Map[Any, Range], text: String): String = references.map{ r => s"${r._1} -> ${pretty(r._2,text)} wasn't ${pretty(expectedReferences(r._1),text)}" }.mkString("{", ", ", "}")
	}
}