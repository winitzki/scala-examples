package swscala

//import cats.syntax.functor._
import cats.{Contravariant, Functor, derive}
//import cats.syntax.functor._
//import example.Filterable._
import example.FilterableWithFilter

object Ch6 {

  object Part1 {

    object Problem1 {
      sealed trait Confucious[W]
      final case class ForgotAll[W]() extends Confucious[W]
      final case class ForgotFri[W](wSat: W) extends Confucious[W]
      final case class ForgotThu[W](wFri: W, wSat: W) extends Confucious[W]
      final case class ForgotWed[W](wThu: W, wFri: W, wSat: W) extends Confucious[W]
      final case class ForgotTue[W](wWed: W, wThu: W, wFri: W, wSat: W) extends Confucious[W]
      final case class ForgotMon[W](wTue: W, wWed: W, wThu: W, wFri: W, wSat: W) extends Confucious[W]
      final case class ForgotSun[W](wMon: W, wTue: W, wWed: W, wThu: W, wFri: W, wSat: W) extends Confucious[W]
      final case class RememberAll[W](wSun: W, wMon: W, wTue: W, wWed: W, wThu: W, wFri: W, wSat: W) extends Confucious[W]

      implicit val functorConfucious = derive.functor[Confucious]

      implicit val filterableConfucious = new FilterableWithFilter[Confucious] {
        override def withFilter[A](p: A => Boolean)(fa: Confucious[A]): Confucious[A] = fa match {
          case ForgotAll() => ForgotAll()
          case ForgotFri(wSat) => if (!p(wSat)) ForgotAll() else fa
          case ForgotThu(wFri, wSat) => if (!p(wFri)) withFilter(p)(ForgotFri(wSat)) else fa
          case ForgotWed(wThu, wFri, wSat) => if (!p(wThu)) withFilter(p)(ForgotThu(wFri, wSat)) else fa
          case ForgotTue(wWed, wThu, wFri, wSat) => if (!p(wWed)) withFilter(p)(ForgotWed(wThu, wFri, wSat)) else fa
          case ForgotMon(wTue, wWed, wThu, wFri, wSat) => if (!p(wTue)) withFilter(p)(ForgotTue(wWed, wThu, wFri, wSat)) else fa
          case ForgotSun(wMon, wTue, wWed, wThu, wFri, wSat) => if (!p(wMon)) withFilter(p)(ForgotMon(wTue, wWed, wThu, wFri, wSat)) else fa
          case RememberAll(wSun, wMon, wTue, wWed, wThu, wFri, wSat) => if (!p(wSun)) withFilter(p)(ForgotSun(wMon, wTue, wWed, wThu, wFri, wSat)) else fa
        }
      }

      // Confucious is not a filterable functor because it violates the conjunction law
      // For example, if you have val c = RememberAll(1, 2, 4, 1, 2, 3, 6)
      // predicate 1: x > 3
      // predicate 2: x < 3
      // c.filter(p1).filter(p2) = ForgotMon(4, 1, 2, 3, 6).filter(p2) = ForgotTue(1, 2, 3, 6)
      // c.filter(p1 && p2) = ForgotAll()
    }

    // object Problem2 {

    // }

  }

}
