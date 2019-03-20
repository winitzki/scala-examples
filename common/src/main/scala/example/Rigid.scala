package example

import cats.Functor

abstract class Rigid[R[_] : Functor] {
  def fuseIn[A, B](kleisli: A ⇒ R[B]): R[A ⇒ B]

  // These methods do not need to be defined by implementations.
  // Any functor has a `fuseOut` method.
  def fuseOut[A, B](app: R[A ⇒ B]): A ⇒ R[B] = a ⇒ Functor[R].map(app)(ab ⇒ ab(a))

  // Non-degeneracy law: `fuseIn andThen fuseOut = identity` as functions on `A ⇒ R[B]`.

  // Any rigid functor is pointed; we can define `point` and `wu` if we have `fuseIn`.
  def point[A](a: A): R[A] = Functor[R].map(fuseIn[R[A], A](identity))(_ ⇒ a)

  // Wrapped unit: a unique value of type R[Unit].
  def wu: R[Unit] = Functor[R].map(fuseIn[R[Unit], Unit](identity))(_ ⇒ Unit)
}

object Rigid {

  // Support the syntax `Rigid[R].fuseIn` etc.
  def apply[R[_] : Rigid]: Rigid[R] = implicitly[Rigid[R]]

  implicit class RigidSyntax[R[_] : Rigid, A, B](f: A ⇒ R[B]) {
    def fuseIn: R[A ⇒ B] = Rigid[R].fuseIn(f)
  }

}

/*
  Show that the type R[Unit] is isomorphic to Unit,
i.e. all values of this type are equal to each other,
i.e. this type has only a single distinct value, which is equal to `Rigid[R].wu`.

  0. The idea is to compute fuseOut(fuseIn(identity)) == identity as functions of type R[Unit] ⇒ R[Unit].
  
  We will show that `fuseOut(fuseIn(identity))` is a function that always returns the same value, namely `wu`.
  
  Since this function is the identity function on `R[Unit]` due to the non-degeneracy law,
we will then have shown that the type `R[Unit]` has only one value, `wu`.

  1. For any fixed type `A`, the type `A ⇒ Unit` is isomorphic to `Unit`,
because there exists only one pure function of type `A ⇒ Unit`, namely `_ ⇒ ()`.

So there is only one distinct value of the type `A ⇒ Unit`, and it is `_ ⇒ ()`.

The isomorphism is realized by the function isoU: Unit ⇒ A ⇒ Unit = { () ⇒ _ ⇒ () }
and uIso: (A ⇒ Unit) ⇒ Unit = { _ ⇒ () }

  2. Apply `fuseIn` to the identity function of type `R[Unit] ⇒ R[Unit]`.

The result is 
   
   val rruu: R[R[Unit] ⇒ Unit] = Rigid[R].fuseIn(identity)
   
We map this via the isomorphism `uIso`, and obtain a value of type R[Unit]:
   
   val ru: R[Unit] = rruu.map(_ ⇒ ())

Note that this is the same value as `Rigid[R].wu`. We can now express `rruu` via that:

  rruu == wu.map(() ⇒ _ ⇒ ())

  3. Now compute `fuseOut(rruu)`, which is of type R[Unit] ⇒ R[Unit].
  
  val ruru: R[Unit] ⇒ R[Unit] = Rigid[R].fuseOut(rruu)
  
  Insert the definition of fuseOut:
  
  ruru = a ⇒ rruu.map(ab ⇒ ab(a))
       = a ⇒ wu.map(() ⇒ _ ⇒ ()).map(ab ⇒ ab(a))
       = a ⇒ wu.map(() ⇒ ())  // The forward composition `{ () ⇒ _ ⇒ () } andThen { ab ⇒ ab(a) }` is equal to `() ⇒ ()`.
       = a ⇒ wu
       = _ ⇒ wu
   
   So, `ruru` is a function that ignores its argument `a` and always returns the same value, `wu`.
   
   4. By virtue of the non-degeneracy law, `ruru = identity`. If the identity function always returns the same value,
   it means that all values of type `R[Unit]` are equal to `wu`. 
 */

