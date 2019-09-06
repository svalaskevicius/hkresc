package hkresc

import org.openjdk.jmh.annotations.Benchmark
import com.svalaskevicius.hkresc._

class Main {
  import Main._

  @Benchmark
  def base(): Int =
    evaluatorRec(simpleExpr)

  @Benchmark
  def cata() =
    hCata(evaluator).apply(simpleExpr)
}

object Main {

  type Id[A] = A

  sealed trait ExprOp[F[_], A]
  case class Const[F[_], A](c: A) extends ExprOp[F, A]
  case class Add[F[_]](a: F[Int], b: F[Int]) extends ExprOp[F, Int]
  case class Eq[F[_]](a: F[Int], b: F[Int]) extends ExprOp[F, Boolean]
  case class Choose[F[_], A](c: F[Boolean], a: F[A], b: F[A])
      extends ExprOp[F, A]

  implicit val hFunctor: HFunctor[ExprOp] = new HFunctor[ExprOp] {
    def hmap[I[_], J[_]](nt: I ~> J) = Lambda[ExprOp[I, ?] ~> ExprOp[J, ?]] {
      case Const(a)        => Const(a)
      case Add(a, b)       => Add(nt(a), nt(b))
      case Eq(a, b)        => Eq(nt(a), nt(b))
      case Choose(c, a, b) => Choose(nt(c), nt(a), nt(b))
    }
  }

  type Expr[A] = HFix[ExprOp, A]
  def const[A](a: A): Expr[A] = HFix(Const(a))
  def add(a: Expr[Int], b: Expr[Int]): Expr[Int] = HFix(Add(a, b))
  def eqv(a: Expr[Int], b: Expr[Int]): Expr[Boolean] = HFix(Eq(a, b))
  def choose[A](c: Expr[Boolean], a: Expr[A], b: Expr[A]): Expr[A] =
    HFix(Choose(c, a, b))

  val evaluator = new HAlgebra[ExprOp, Id] {
    def apply[A](fa: ExprOp[Id, A]) = fa match {
      case Const(a)        => a
      case Add(a, b)       => a + b
      case Eq(a, b)        => a == b
      case Choose(c, a, b) => if (c) a else b
    }
  }

  def evaluatorRec[A](fa: Expr[A]): A = fa.unfix match {
    case Const(a)  => a
    case Add(a, b) => evaluatorRec(a) + evaluatorRec(b)
    case Eq(a, b)  => evaluatorRec(a) == evaluatorRec(b)
    case Choose(c, a, b) =>
      if (evaluatorRec(c)) evaluatorRec(a) else evaluatorRec(b)
  }

  val unfolder = new HCoalgebra[ExprOp, Id] {
    def apply[A](a: A): ExprOp[Id, A] = a match {
      case 1      => Const[Id, Int](1).asInstanceOf[ExprOp[Id, A]]
      case x: Int => Add[Id](x - 1, 1).asInstanceOf[ExprOp[Id, A]]
    }
  }

  val simpleExpr: Expr[Int] = choose(
    eqv(const(5), const(5)),
    add(const(2), add(const(1), const(3))),
    const(0)
  )

}
