package example

import org.scalatest.{FlatSpec, Matchers}

class Chapter11_01_examplesSpec extends FlatSpec with Matchers {

  behavior of "monad transformers"

  it should "fail to compose Option with Reader" in {
    def withParams[R] = {
      type OR[A] = Option[R ⇒ A]

      var orMonad: CatsMonad[OR] = new CatsMonad[OR] {

        override def flatMap[A, B](fa: OR[A])(f: A ⇒ OR[B]): OR[B] = fa match {
          // Need to return Option[R ⇒ B]. We should avoid always returning `None`,
          // because that would violate the identity law `fa.flatMap(pure) = fa`.
          case None ⇒ None // This is the only possibility here.
          case Some(rToA) ⇒ // Here, we need to return `None` or `Some[R ⇒ B]`.
            // However, we don't have any values of types A or R here, 
            // so we can't apply `f` to anything at this point.
            // Therefore, we have to decide in advance whether to return `None` or `Some[R ⇒ B]`.
            // We would like to try returning a `Some[R ⇒ B]` so that we could satisfy the identity law.
            Some { r ⇒
              // We need to get a `B` here. We can apply `f` to `rToA(r) : A`.
              val orb: Option[R ⇒ B] = f(rToA(r))
              // Now, `orb` is either `None` or `Some[R ⇒ B]`.
              orb match {
                case Some(rToB) ⇒ rToB(r)
                case None ⇒ ??? // Here we get stuck: there is no way to compute a `B`.
              }
            }
        }

        override def pure[A](x: A): OR[A] = Some { _ ⇒ x }
      }
    }

  }

  behavior of "State monad does not compose with other monads"

  it should "fail to implement composed-outside transformer for State monad" in {
    def withParams[R, S] = {
      type St[A] = S ⇒ (S, A) // State monad with state type S.
      type SR[A] = St[R ⇒ A] // Composition of State and Reader.

      // Implement the type signature of Monad for SR[?].

      val srMonad: CatsMonad[SR] = new CatsMonad[SR] {
        override def flatMap[A, B](fa: SR[A])(f: A ⇒ SR[B]): SR[B] = { s ⇒
          val (s1, ra): (S, R ⇒ A) = fa(s)
          (s1, { r ⇒ // Have to use s or s1 here, since `r` is only available inside the closure.
            val a: A = ra(r)
            val (s2, rb): (S, R ⇒ B) = f(a)(s1)
            rb(r) // Have to discard s2.
          })
        }

        override def pure[A](x: A): SR[A] = { s ⇒ (s, _ ⇒ x) }
      }

      // We have not used `f` in computing the new `s` after flatMap(f).
      // This means we are discarding the effect of the second monad.
      // This will violate the identity law pure(x).flatMap(f) = f(x):
      /*
      For any f : A ⇒ St[B] and for any x : A we should have f(x) = pure(x).flatMap(f)
      
      pure(x).flatMap(f) = flatMap { s ⇒ (s, _ ⇒ x) } (f) = { s ⇒
        val (s1, ra) = (s, _ ⇒ x)
        (s, ...)
      }
      
      The result always has the initial unmodified `s` as the first element of the pair.
      However, f(x) : St[B] could have the form { s ⇒ (s1, ...) } where s1 != s, depending on x.
      Therefore, pure(x).flatMap(f) is not equal to f(x) for arbitrary functions f.
       */
    }
  }

  it should "fail to implement composed-inside transformer for State monad" in {
    def withParams[R, S] = {
      type St[A] = S ⇒ (S, A) // State monad with state type S.
      type OS[A] = Option[St[A]] // Composition of Option and State.

      // Implement the type signature of Monad for OS[?].

      val osMonad: CatsMonad[OS] = new CatsMonad[OS] {
        override def flatMap[A, B](fa: OS[A])(f: A ⇒ OS[B]): OS[B] = fa match {
          // Need to return Option[St[B]]. We should avoid always returning `None`,
          // because that would violate the identity law `fa.flatMap(pure) = fa`.
          case None ⇒ None // This is the only possibility here.
          case Some(sta) ⇒
            // If we return None here, we will violate the identity law.
            Some { s ⇒ // We are trying to return a Some(St[B]) here.
              // We can now apply `sta` to `s` and get some more values.
              val (s1, a) = sta(s)
              // Can we get a value sb : (S, B)?
              val s2: S = s // We could return various values of type s.
            val b: B = {
              // The only way of possibly getting `b: B` is to apply `f` to an `A`.
              val osb: Option[St[B]] = f(a)
              // Now, `osb` could be either `None` or `Some(...)`.
              osb match {
                case Some(stb) ⇒ stb(s2)._2
                case None ⇒ ??? // Here we get stuck: there is no way of computing a `b : B`.
              }
            }
              (s2, b)
            }
        }

        override def pure[A](x: A): OS[A] = Some { s ⇒ (s, x) }
      }
    }
  }

}
