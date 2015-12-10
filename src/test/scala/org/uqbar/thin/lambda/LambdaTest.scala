package org.uqbar.thin.lambda

import scala.language.implicitConversions
import scala.language.reflectiveCalls
import org.scalatest.Finders
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.uqbar.thin.grammars._

class LambdaTest extends FreeSpec with Matchers with LambdaCalculus {

	"LambdaCalculus" - {

		lazy val options: GrammarPreferences = GrammarPreferences(
			terminals = Map('lambdaHeader -> "\\", 'lambdaSeparator -> ".", 'openParens -> "(", 'closeParens -> ")", 'application -> " "),
			encodingPreferences = EncodingPreferences()
		)

		"variables" - {
			"should encode" in {
				val encoder = variable.encoder(options)
				encoder(Variable("x")).get.text should be("x")
			}

			"should decode" in {
				val decoder = variable.decoder(options)
				decoder("x").get should be(Variable("x"))
			}
		}

		"lambda" - {
			"should encode" in {
				val encoder = lambda.encoder(options)
				encoder(Lambda(Variable("x"), Variable("y"))).get.text should be("\\x.y")
			}

			"should decode" in {
				val decoder = lambda.decoder(options)
				decoder("\\x.y").get should be(Lambda(Variable("x"), Variable("y")))
			}
		}
		
		"application chain" - {
			"should encode" - {
				val encoder = applicationChain.encoder(options)
				encoder(Nil).get.text should be("")
				encoder(Variable("x")::Nil).get.text should be(" x")
				encoder(Variable("x")::Variable("y")::Nil).get.text should be(" x y")
				encoder(Variable("x")::Variable("y")::Variable("z")::Nil).get.text should be(" x y z")
			}
			
			"should decode" - {
				val decoder = applicationChain.decoder(options)
				decoder("").get should be(Nil)
				decoder(" x").get should be(Variable("x")::Nil)
				decoder(" x y").get should be(Variable("x")::Variable("y")::Nil)
				decoder(" x y z").get should be(Variable("x")::Variable("y")::Variable("z")::Nil)
			}
		}
		
		"parens" - {
		}

		"expression" - {
			"should encode" - {
				val encoder = expression.encoder(options)

				"variables" in {
					encoder(Variable("x")).get.text should be("x")
				}
				"applications" in {
					encoder(Application(Variable("x"), Variable("y"))).get.text should be("x y")
					encoder(Application(Application(Variable("x"), Variable("y")), Variable("z"))).get.text should be("x y z")
				}
				"lambdas" in {
					encoder(Lambda(Variable("x"), Variable("y"))).get.text should be("\\x.y")
				}
			}

			"should decode" - {
				val decoder = expression.decoder(options)
				
				"variables" in {
					decoder("x").get should be(Variable("x"))
				}
				"applications" in {
					decoder("x y").get should be(Application(Variable("x"), Variable("y")))
					decoder("x y z").get should be(Application(Application(Variable("x"), Variable("y")),Variable("z")))
				}
				"lambdas" in {
					decoder("\\x.y").get should be(Lambda(Variable("x"), Variable("y")))
				}
			}

			
		}

	}
}
