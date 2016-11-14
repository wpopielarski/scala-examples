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
    val one = unit(k).flatMap { _ |+| 5.0 }.flatMap { _ |-| 3.0 }
    val two = tag[Double, NonNegative](5.0)
    two |-| 2.0 flatMap { t => one.flatMap { o => unit(o + t) } }
  }
}