package acme

import scala.language.higherKinds
import scala.language.implicitConversions

object Tag {
  type Tagged[U] = { type Tag = U }
  type @@[T, U] = T with Tagged[U]
  def tag[T, U](t: T) = t.asInstanceOf[T @@ U]
}

trait NonNegative

import Tag._

trait NonNegativeAlgebra[T, U <: _ @@ NonNegative] {
  def |+|(s1: U, s2: T): Option[U]
  def |-|(s1: U, s2: T): Option[U]
}

object NonNegativeAlgebra {
  // abstract constructor
  def apply[T, U <: _ @@ NonNegative](from: T)(constr: T => Option[U]): Option[U] = constr(from)
  // implicit postfix to infix
  // explicit version
  //  implicit class addAlgebra[T, U <: _ @@ NonNegative](t: U)(implicit ev: NonNegativeAlgebra[T, U]) {
  //    def |+|(s: T): Option[U] = ev.|+|(t, s)
  //    def |-|(s: T): Option[U] = ev.|-|(t, s)
  //  }

  // implicit version
  implicit class addAlgebraImplicit[T, U <: _ @@ NonNegative: ({ type Alg[C] = NonNegativeAlgebra[T, C] })#Alg](t: U) {
    def |+|(s: T): Option[U] = implicitly[NonNegativeAlgebra[T, U]].|+|(t, s)
    def |-|(s: T): Option[U] = implicitly[NonNegativeAlgebra[T, U]].|-|(t, s)
  }
}

object NonNegativeTest {
  import NonNegativeAlgebra._
  // when double conveys non negative value
  // constructor
  def unit(k: Double) = NonNegativeAlgebra[Double, Double @@ NonNegative](k) { k =>
    if (k < 0.0) None
    else Option(tag(k))
  }
  // non negative algebra for double
  implicit val doubleAlgebra = new NonNegativeAlgebra[Double, Double @@ NonNegative] {
    def |+|(s1: Double @@ NonNegative, s2: Double) = unit(s1 + s2)
    def |-|(s1: Double @@ NonNegative, s2: Double) = unit(s1 - s2)
  }

  // just test
  def test(k: Double): Option[Double @@ NonNegative] = {
    // safe construction
    val one = unit(k)
      .flatMap { _ |+| 5.0 flatMap { _ |-| 1.0 } }
      .flatMap { _ |-| 3.0 }
    // haphazard construction
    val two = tag[Double, NonNegative](5.0)
    // combination example
    two |-| 2.0 flatMap { t => one.flatMap { o => unit(o + t) } }
  }
}

object NonNegativeAsOption {
  trait Applicative[A, B, F[_]] {
    def <*>(fa: F[A])(fb: F[A => B]): F[B]
    //def <*>[A, B](fa: F[A])(fb: F[A => B]): F[B]
  }

  type NonNegOpt[T] = Option[T] @@ NonNegative

  trait NonNegativeOpt[T] extends Applicative[T, T, NonNegOpt] {
    def unit(t: T): NonNegOpt[T]
    def <*>(nno: NonNegOpt[T])(nnott: NonNegOpt[T => T]): NonNegOpt[T] =
      tag(nno.flatMap { d => nnott.flatMap { f => unit(f(d)) } })
  }

  implicit def funToNonNegOpt[T](f: T => T): NonNegOpt[T => T] = tag(Option(f))

  implicit class addNonNegOpt[T: NonNegativeOpt](fa: NonNegOpt[T])(implicit val funToNonNegOpt: (T => T) => NonNegOpt[T => T]) {
    def <*>(fb: T => T): NonNegOpt[T] = implicitly[NonNegativeOpt[T]].<*>(fa)(fb)
  }

  // Double example
  implicit val nonNegOptForDouble = new NonNegativeOpt[Double] {
    def unit(d: Double): NonNegOpt[Double] = tag(if (d < 0.0) None else Option(d))
  }

  def testDouble(t: NonNegOpt[Double]) = {
    t <*> { _ + 2.0 } <*> { _ - 10.0 }
  }

  // Money example
  type MoneyTag
  trait Money {
    private def normalize(s: (Int, Int) @@ MoneyTag): (Int, Int) @@ MoneyTag = {
      val (r1, r2) = (s._1 + s._2 / 100, s._2 % 100)
      tag(if (r1 > 0 && r2 < 0) (r1 - 1, r2 + 100) else (r1, r2))
    }

    def |+|(s1: (Int, Int) @@ MoneyTag, s2: (Int, Int)): (Int, Int) @@ MoneyTag =
      normalize(tag((s1._1 + s2._1, s1._2 + s2._2)))

    def |-|(s1: (Int, Int) @@ MoneyTag, s2: (Int, Int)): (Int, Int) @@ MoneyTag =
      normalize(tag((s1._1 - s2._1, s1._2 - s2._2)))
  }
  object Money extends Money

  implicit class addMoney(s1: (Int, Int) @@ MoneyTag) {
    def |+|(s2: (Int, Int)): (Int, Int) @@ MoneyTag = Money.|+|(s1, s2)
    def |-|(s2: (Int, Int)): (Int, Int) @@ MoneyTag = Money.|-|(s1, s2)
  }

  implicit val nonNegOptForMoney = new NonNegativeOpt[(Int, Int) @@ MoneyTag] {
    def unit(d: (Int, Int) @@ MoneyTag): NonNegOpt[(Int, Int) @@ MoneyTag] = tag(if (d._1 < 0 || d._2 < 0) None else Option(d))
  }

  def testMoney(t: NonNegOpt[(Int, Int) @@ MoneyTag]) = {
    t <*> { _ |+| (2, 3) } <*> { _ |+| (3, 5) }
  }
}

import NonNegativeAsOption._

object Test extends App {
  val m = nonNegOptForMoney.unit(tag((1, 0)))
  println(m <*> { _ |-| (0, 65) })
  println(m <*> { _ |-| (0, 65) } <*> { _ |+| (0, 223) } <*> { _ |-| (0, 59) })
}