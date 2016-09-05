package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    for ((name, exp) <- namedExpressions) yield (name -> Signal(eval(exp(), namedExpressions)))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    for ((name, exp) <- references) println("name %s expr %s".format(name, exp()))
    expr match {
      case Literal(v)  => v
      case Plus(l, r)  => eval(l, references) + eval(r, references)
      case Minus(l, r) => eval(l, references) - eval(r, references)
      case Times(l, r) => eval(l, references) * eval(r, references)
      case Divide(l, r) => {
        if (eval(r, references) == 0) {
          println("exp %s dividing by zero".format(expr))
          return (Double.NaN)
        } else eval(l, references) / eval(r, references)
      }
      case Ref(name) => {
        if (!references.contains(name)) {
          println("cyclic or non existing reference to %s".format(name))
          return (Double.NaN)
        } else {
          //--------------------------------------------------------------------------------
          // When variable 'X' refers to another variable, then remove 'X' from the references,
          // so that additional reference to 'X' down in the reference recursion will be caught
          // as a reference to non-existing variable.
          //--------------------------------------------------------------------------------
          eval(references(name)(), (references - name))
        }
      }
    }
  }

  /**
   * Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
