package example

import cats.{Functor, Id, ~>}

// Type definition of a monad transformer.
trait MTransDef[LT[_[_], _]] {
  // For any foreign monad `M`, we have a monad instance for `LT[M, ?]`.
  def transformed[M[_] : CatsMonad : Functor]: CatsMonad[LT[M, ?]]
}

// A fully-featured monad transformer has the methods lift, blift, mrun, brun.
abstract class MTrans[LT[_[_], _] : MTransDef, L[_] : CatsMonad : Functor] {
  def lift[M[_] : CatsMonad : Functor, A](ma: M[A]): LT[M, A]

  def blift[M[_] : CatsMonad : Functor, A](la: L[A]): LT[M, A]

  def mrun[M[_] : CatsMonad : Functor, N[_] : CatsMonad](mn: M ~> N): LT[M, ?] ~> LT[N, ?]

  def brun[M[_] : CatsMonad : Functor](lrun: L ~> Id): LT[M, ?] ~> M
}
