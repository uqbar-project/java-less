package org.uqbar.thin.grammars

import org.scalatest.FreeSpec
import org.scalatest.Matchers

class GrammarsTest extends FreeSpec with Grammars with Matchers {

	"Grammar" - {

		val options = GrammarPreferences(
			terminals = Map('one -> "1", 'x -> "x", ': -> ":", 'y -> "y"),
			encodingPreferences = EncodingPreferences()
		)

		"Terminal" - {
			val grammar = new Terminal('one)

			"should encode" in {
				val encoder = grammar.encoder(options)
				encoder(()).get.text should (be("1"))
			}

			"should decode" in {
				val decoder = grammar.decoder(options)
				decoder("1").get should (be("1"))
			}
		}

		"Append" - {
			val grammar = new Append(new Terminal('one), new Terminal('one))

			"should encode" in {
				val encoder = grammar.encoder(options)
				encoder(("x", "y")).get.text should (be("11"))
			}

			"should decode" in {
				val decoder = grammar.decoder(options)
				decoder("11").get should (be(("1", "1")))
			}
		}

		"Value" - {
			val grammar = new Value("[0-9]*".r)

			"should encode" in {
				val encoder = grammar.encoder(options)
				encoder("192").get.text should (be("192"))
			}

			"should decode" in {
				val decoder = grammar.decoder(options)
				decoder("192").get should (be("192"))
			}
		}

		"Transform" - {
			case class X(s: String)

			val grammar = new Transform(new Append(new Terminal('x), new Append(new Terminal(':), new Value(".*".r))))({ x: X => (null, (null, x.s)) })({ case (_, (_, s)) => X(s) })

			"should encode" in {
				val encoder = grammar.encoder(options)
				encoder(X("foo")).get.text should (be("x:foo"))
			}

			"should decode" in {
				val decoder = grammar.decoder(options)
				decoder("x:foo").get should (be(X("foo")))
			}
		}

		"Or" - {
			trait Z
			case class X(s: String) extends Z
			case class Y(n:Int,m:Int) extends Z

			val x = new Transform(new Append(new Terminal('x), new Append(new Terminal(':), new Value(".*".r))))({ x: X => (null, (null, x.s)) })({ case (_, (_, s)) => X(s) })
			val y = new Transform(new Append(new Terminal('y), new Append(new Terminal(':), new Append(new Transform(new Value("[0-9]".r))({n: Int => n.toString})({s: String => s.toInt}),new Transform(new Value("[0-9]".r))({n: Int => n.toString})({s: String => s.toInt})))))({ y: Y => (null, (null, (y.n, y.m))) })({ case (_, (_, (n,m))) => Y(n,m) })
			val grammar: Grammar[Z,Z] = new Or(x,y)

			"should encode" in {
				val encoder = grammar.encoder(options)
				encoder(X("foo")).get.text should (be("x:foo"))
				encoder(Y(5,6)).get.text should (be("y:56"))
			}

			"should decode" in {
				val decoder = grammar.decoder(options)
				decoder("x:foo").get should (be(X("foo")))
				decoder("y:56").get should (be(Y(5,6)))
			}
		}
		
		"Repeat without separator" - {
			val grammar = new Repeat(new Value(".".r))
			
			"should encode" in {
				val encoder = grammar.encoder(options)
				encoder("a"::Nil).get.text should (be("a"))
				encoder("a"::"b"::Nil).get.text should (be("ab"))
				encoder("a"::"b"::"c"::Nil).get.text should (be("abc"))
			}

			"should decode" in {
				val decoder = grammar.decoder(options)
				decoder("a").get should (be(List("a")))
				decoder("ab").get should (be(List("a","b")))
				decoder("abc").get should (be(List("a","b","c")))
			}
		}

		"Repeat with separator" - {
			val grammar = new Repeat(new Value(".".r),new Terminal(':))
			
			"should encode" in {
				val encoder = grammar.encoder(options)
						encoder("a"::Nil).get.text should (be("a"))
						encoder("a"::"b"::Nil).get.text should (be("a:b"))
						encoder("a"::"b"::"c"::Nil).get.text should (be("a:b:c"))
			}
			
			"should decode" in {
				val decoder = grammar.decoder(options)
						decoder("a").get should (be(List("a")))
						decoder("a:b").get should (be(List("a","b")))
						decoder("a:b:c").get should (be(List("a","b","c")))
			}
		}

	}

}