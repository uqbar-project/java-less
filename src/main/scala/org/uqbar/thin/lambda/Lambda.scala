package org.uqbar.thin.lambda

trait Expressions extends LanguageModule {
	trait Expression extends SyntaxElement

	def expression: Grammar[Expression, Expression] = Fail
}

trait LambdaCalculus extends LanguageModule with Expressions {
	case class Lambda(argument: Variable, body: Expression) extends Expression
	case class Application(left: Expression, right: Expression) extends Expression
	case class Variable(name: String) extends Expression


	def applicable: Grammar[Expression,Expression] = new Or(variable, new Or(lambda, parens): Grammar[Expression,Expression])
			
	def variable = new Transform(Value("[a-zA-Z_]+".r))({ v: Variable => v.name })(Variable(_))
	
	def lambda = new Transform(
		new Append(new Append(new Append(new Terminal('lambdaHeader),	variable),new Terminal('lambdaSeparator)),expression)
	)({ l: Lambda => (((null, l.argument), null), l.body) })({ case (((_, a), _), b) => Lambda(a, b) })

	def parens = new Transform(new Append(new Terminal('openParens), new Append(expression, new Terminal('closeParens))))({e:Expression => (null,(e,null))})({case (_,(e,_)) => e})
	
	def applicationChain: Grammar[List[Expression],List[Expression]] = new Repeat(new Transform(new Append(new Terminal('application), applicable))({e:Expression => (null,e)})({case (_,e) => e}))
	
	override def expression: Grammar[Expression,Expression] = {
		def flattenApp(e: Expression): List[Expression] = e match {
			case Application(r,a) => flattenApp(r):::List(a) 
			case _ => List(e)
		}
		
		new Transform(new Append(applicable, applicationChain))({
		e: Expression =>
			val a::ac = flattenApp(e)
			(a,ac)
		})({case (a,ac) => (a/:ac){(a,e) => Application(a,e)}})
	}
}

object MyCustomLanguage extends Language with LambdaCalculus