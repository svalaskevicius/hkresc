package com.svalaskevicius

package object hkresc {
  // https://medium.com/disney-streaming/fix-point-type-for-gadt-scala-dc4e2cde349b
  // https://free.cofree.io/2017/11/13/recursion/
  // https://b-studios.de/assets/guide-to-morphisms.pdf


  trait ~>[A[_], B[_]] {
    def apply[X](a: A[X]): B[X]
  }

  case class HFix[F[_[_], _], A](unfix: F[HFix[F, ?], A]) extends AnyVal

  trait HFunctor[F[_[_], _]] {
    def hmap[I[_], J[_]](nt: I ~> J): F[I, ?] ~> F[J, ?]
  }

  type HAlgebra[F[_[_], _], G[_]]   = F[G, ?] ~> G
  type HCoalgebra[F[_[_], _], G[_]] = G ~> F[G, ?]

  type HRAlgebra[F[_[_], _], G[_]]   = F[Lambda[A => (HFix[F, A], G[A])], ?] ~> G
  type HRCoalgebra[F[_[_], _], G[_]] = G ~> F[Lambda[A => Either[HFix[F, A], G[A]]], ?]

  /**
    * Catamorphism is the dual of anamorphism, it generalises fold.
    */
  def hCata[F[_[_], _], G[_]](alg: HAlgebra[F, G])(implicit F: HFunctor[F]): HFix[F, ?] ~> G =
    Lambda[HFix[F, ?] ~> G](hfix => alg(F.hmap(hCata(alg))(hfix.unfix)))

  /**
    * Anamorphism is one of the basic types of recursion schemes. It generalizes unfold,
    * meaning you can use it to create a recursive structure based on a recursive type.
    */
  def hAna[F[_[_], _], G[_]](alg: HCoalgebra[F, G])(implicit F: HFunctor[F]): G ~> HFix[F, ?] =
    Lambda[G ~> HFix[F, ?]](a => HFix(F.hmap(hAna(alg))(alg(a))))

  /**
    * Hylomorphism is the composition of anamorphism and catamorphism.
    * It fuses the unfold and fold operation and in such way avoids building big temporary structures in memory.
    */
  def hHylo[F[_[_], _], H[_], G[_]](f: HAlgebra[F, G])(g: HCoalgebra[F, H])(implicit F: HFunctor[F]): H ~> G =
    Lambda[H ~> G](a => f(F.hmap(hHylo(f)(g))(g(a))))

  /**
    * Paramorphism is also a generalization of fold. It is an extension of catamorphism, and offers more power.
    *
    * Paramorphism is more powerful than catamorphism in the sense that in the algebra f, we not only have an F[A] to work with,
    * but we also have an F[Fix[F]], which means we have access to the Fix structure that yields the A when being folded.
    *
    * @todo would be good to be lazy on recursive hPara application - use Eval
    */
  def hPara[F[_[_], _], G[_]](f: HRAlgebra[F, G])(implicit F: HFunctor[F]): HFix[F, ?] ~> G =
    Lambda[HFix[F, ?] ~> G](
      fix =>
        f(
          F.hmap(
            Lambda[HFix[F, ?] ~> Lambda[A => (HFix[F, A], G[A])]](
              fix2 => fix2 -> hPara(f).apply(fix2)
            )
          )(fix.unfix)
        )
    )

  /**
    * Apomorphism is the dual of paramorphism, and is an extension of anamorphism.
    *
    * Compared to anamorphism, apomorphism gives you more control on when to stop the recursion.
    * In anamorphism, the recursion is terminated when a node representing the base case (e.g., Done or Zero)
    * is visited, on which the map function is a no-op. In apomorphism, the recursion can be terminated either by
    * visiting a base case, or if f returns a Left containing a Fix[F].
    */
  def hApo[F[_[_], _], G[_]](f: HRCoalgebra[F, G])(implicit F: HFunctor[F]): G ~> HFix[F, ?] =
    Lambda[G ~> HFix[F, ?]](
      a =>
        HFix(
          F.hmap[Lambda[A => Either[HFix[F, A], G[A]]], HFix[F, ?]](
            Lambda[Lambda[A => Either[HFix[F, A], G[A]]] ~> HFix[F, ?]] {
              case Left(fix) => fix
              case Right(aa) => hApo(f).apply(aa)
            }
          )(f(a))
        )
    )

  final case class HCofree[F[_[_], _], G[_], A](head: G[A], tail: F[HCofree[F, G, ?], A])

  /**
    * Histomorphism is yet another recursion scheme for fold, and is more powerful than paramorphism.
    *
    * Histomorphism operates on an enhanced version of Fix, called Cofree, where each node in the structure is annotated by some value.
    */
  def hHisto[F[_[_], _], G[_]](f: F[HCofree[F, G, ?], ?] ~> G)(implicit F: HFunctor[F]): HFix[F, ?] ~> G = {
    def toCofree: HFix[F, ?] ~> HCofree[F, G, ?] =
      Lambda[HFix[F, ?] ~> HCofree[F, G, ?]](
        fix => HCofree(head = hHisto(f).apply(fix), tail = F.hmap(toCofree)(fix.unfix))
      )

    Lambda[HFix[F, ?] ~> G](fix => f(F.hmap(toCofree)(fix.unfix)))
  }

  /**
    * Dynamorphism is the composition of anamorhpism and histomorphism.
    */
  def hDyna[F[_[_], _], G[_], H[_]](f: F[HCofree[F, G, ?], ?] ~> G)(g: HCoalgebra[F, H])(implicit F: HFunctor[F]): H ~> G = {
    def toCofree: HAlgebra[F, HCofree[F, G, ?]] =
      Lambda[F[HCofree[F, G, ?], ?] ~> HCofree[F, G, ?]](
        fc => HCofree(head = f(fc), tail = fc)
      )

    Lambda[H ~> G](fix => hHylo(toCofree)(g).apply(fix).head)
  }

  sealed trait HFree[F[_[_], _], G[_], A]
  final case class HContinue[F[_[_], _], G[_], A](a: G[A])                 extends HFree[F, G, A]
  final case class HCombine[F[_[_], _], G[_], A](fa: F[HFree[F, G, ?], A]) extends HFree[F, G, A]

  /**
    * Futumorphism - a dual of histomorphism.
    */
  def hFutu[F[_[_], _], G[_]](f: G ~> F[HFree[F, G, ?], ?])(implicit F: HFunctor[F]): G ~> HFix[F, ?] = {
    def toFix: HFree[F, G, ?] ~> HFix[F, ?] =
      Lambda[HFree[F, G, ?] ~> HFix[F, ?]] {
        case HContinue(a) => hFutu(f).apply(a)
        case HCombine(fa) => HFix(F.hmap(toFix)(fa))
      }

    Lambda[G ~> HFix[F, ?]](a => HFix(F.hmap(toFix)(f(a))))
  }
  


}
