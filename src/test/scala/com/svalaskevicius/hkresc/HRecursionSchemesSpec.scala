package com.svalaskevicius.hkresc

import org.specs2.mutable.Specification

class HRecursionSchemesSpec extends Specification {
  
  "hCata" in {
    val simpleExpr = choose(
      eq(const(5), const(5)),
      add(const(2), add(const(1), const(3))),
      const(0)
    )
    hCata(evaluator).apply(simpleExpr) must be equalTo 6
  }

  "hAna" in {
    hAna(unfolder).apply(3) must be equalTo add(add(const(1), const(1)), const(1))
  }

  "hHylo" in {
    hHylo(evaluator)(unfolder).apply(3) must be equalTo 3
  }

  "hPara" in {
    val alg = Lambda[ExprOp[Lambda[A => (Expr[A], (A, List[Expr[A]]))], ?] ~> Lambda[A => (A, List[Expr[A]])]] {
      case Const(a)  => (a, List.empty)
      case Add(a, b) => (a._2._1 + b._2._1, a._1 :: b._1 :: a._2._2 ++ b._2._2)
      case _         => ???
    }
    val ret = hPara[ExprOp, Lambda[A => (A, List[Expr[A]])]](alg).apply(add(add(const(1), const(2)), const(3)))
    ret must be equalTo (6, List(add(const(1), const(2)), const(3), const(1), const(2)))
  }

  "hApo" in {
    val fullUnfolder = new (Id ~> ExprOp[Lambda[A => Either[Expr[A], Id[A]]], ?]) {
      def apply[A](a: A): ExprOp[Lambda[A => Either[Expr[A], Id[A]]], A] = a match {
        case 1      => Const[Lambda[A => Either[Expr[A], A]], Int](1).asInstanceOf[ExprOp[Lambda[A => Either[Expr[A], Id[A]]], A]]
        case x: Int => Add[Lambda[A => Either[Expr[A], A]]](Right(x - 1), Right(1)).asInstanceOf[ExprOp[Lambda[A => Either[Expr[A], Id[A]]], A]]
      }
    }
    val precomputedUnfolder = new (Id ~> ExprOp[Lambda[A => Either[Expr[A], Id[A]]], ?]) {
      def apply[A](a: A): ExprOp[Lambda[A => Either[Expr[A], Id[A]]], A] = a match {
        case 3 => Add[Lambda[A => Either[Expr[A], A]]](Left(const(2)), Left(const(1))).asInstanceOf[ExprOp[Lambda[A => Either[Expr[A], Id[A]]], A]]
      }
    }
    hApo[ExprOp, Id](fullUnfolder).apply(3) must be equalTo add(add(const(1), const(1)), const(1))
    hApo[ExprOp, Id](precomputedUnfolder).apply(3) must be equalTo add(const(2), const(1))
  }

  "hHisto" in {
    type G[A] = (A, List[String])
    val alg = Lambda[ExprOp[HCofree[ExprOp, G, ?], ?] ~> G] {
      case Const(a)  => (a, List.empty)
      case Add(a, b) => (a.head._1 + b.head._1, a.tail.toString :: a.head._2 ++ (b.tail.toString :: b.head._2))
      case _         => ???
    }
    val ret = hHisto[ExprOp, G](alg).apply(add(add(const(1), const(2)), const(3)))
    ret must be equalTo (6, List("Add(HCofree((1,List()),Const(1)),HCofree((2,List()),Const(2)))", "Const(1)", "Const(2)", "Const(3)"))
  }

  "hDyna" in {
    type G[A] = (A, List[String])
    val alg = Lambda[ExprOp[HCofree[ExprOp, G, ?], ?] ~> G] {
      case Const(a)  => (a, List.empty)
      case Add(a, b) => (a.head._1 + b.head._1, a.tail.toString :: a.head._2 ++ (b.tail.toString :: b.head._2))
      case _         => ???
    }
    val ret = hDyna[ExprOp, G, Id](alg)(unfolder).apply(3)
    ret must be equalTo (3, List("Add(HCofree((1,List()),Const(1)),HCofree((1,List()),Const(1)))", "Const(1)", "Const(1)", "Const(1)"))
  }

  "hFutu" in {
    val fullUnfolder = new (Id ~> ExprOp[HFree[ExprOp, Id, ?], ?]) {
      def apply[A](a: A): ExprOp[HFree[ExprOp, Id, ?], A] = a match {
        case 1 => Const[HFree[ExprOp, Id, ?], A](1.asInstanceOf[A])
        case x: Int =>
          Add[HFree[ExprOp, Id, ?]](HContinue[ExprOp, Id, Int](x - 1), HContinue[ExprOp, Id, Int](1)).asInstanceOf[ExprOp[HFree[ExprOp, Id, ?], A]]
      }
    }
    val precomputedUnfolder = new (Id ~> ExprOp[HFree[ExprOp, Id, ?], ?]) {
      def apply[A](a: A): ExprOp[HFree[ExprOp, Id, ?], A] = a match {
        case 1 =>
          Const[HFree[ExprOp, Id, ?], A](1.asInstanceOf[A])
        case x: Int if x > 2 =>
          Add[HFree[ExprOp, Id, ?]](
            HCombine[ExprOp, Id, Int](
              Const[HFree[ExprOp, Id, ?], Int](x - 2)
            ),
            HContinue[ExprOp, Id, Int](2)
          ).asInstanceOf[ExprOp[HFree[ExprOp, Id, ?], A]]
        case x: Int =>
          Add[HFree[ExprOp, Id, ?]](HContinue[ExprOp, Id, Int](x - 1), HContinue[ExprOp, Id, Int](1)).asInstanceOf[ExprOp[HFree[ExprOp, Id, ?], A]]
      }
    }
    hFutu[ExprOp, Id](fullUnfolder).apply(3) must be equalTo add(add(const(1), const(1)), const(1))
    hFutu[ExprOp, Id](precomputedUnfolder).apply(8) must be equalTo add(const(6), add(const(1), const(1)))
  }

  type Id[A] = A

  sealed trait ExprOp[F[_], A]
  case class Const[F[_], A](c: A)                             extends ExprOp[F, A]
  case class Add[F[_]](a: F[Int], b: F[Int])                  extends ExprOp[F, Int]
  case class Eq[F[_]](a: F[Int], b: F[Int])                   extends ExprOp[F, Boolean]
  case class Choose[F[_], A](c: F[Boolean], a: F[A], b: F[A]) extends ExprOp[F, A]

  implicit val hFunctor: HFunctor[ExprOp] = new HFunctor[ExprOp] {
    def hmap[I[_], J[_]](nt: I ~> J) = Lambda[ExprOp[I, ?] ~> ExprOp[J, ?]] {
      case Const(a)        => Const(a)
      case Add(a, b)       => Add(nt(a), nt(b))
      case Eq(a, b)        => Eq(nt(a), nt(b))
      case Choose(c, a, b) => Choose(nt(c), nt(a), nt(b))
    }
  }

  type Expr[A] = HFix[ExprOp, A]
  def const[A](a: A): Expr[A]                                      = HFix(Const(a))
  def add(a: Expr[Int], b: Expr[Int]): Expr[Int]                   = HFix(Add(a, b))
  def eq(a: Expr[Int], b: Expr[Int]): Expr[Boolean]                = HFix(Eq(a, b))
  def choose[A](c: Expr[Boolean], a: Expr[A], b: Expr[A]): Expr[A] = HFix(Choose(c, a, b))

  val evaluator = new HAlgebra[ExprOp, Id] {
    def apply[A](fa: ExprOp[Id, A]) = fa match {
      case Const(a)        => a
      case Add(a, b)       => a + b
      case Eq(a, b)        => a == b
      case Choose(c, a, b) => if (c) a else b
    }
  }

  val unfolder = new HCoalgebra[ExprOp, Id] {
    def apply[A](a: A): ExprOp[Id, A] = a match {
      case 1      => Const[Id, Int](1).asInstanceOf[ExprOp[Id, A]]
      case x: Int => Add[Id](x - 1, 1).asInstanceOf[ExprOp[Id, A]]
    }
  }
}
