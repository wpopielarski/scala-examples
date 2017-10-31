package priv

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait MappedTo[T] {
  type Underlying = T
  def id: Underlying
}

class BaseId(val id: Long) extends MappedTo[Long]
class StringId(val id: String) extends MappedTo[String]

trait Iso[T, U] {
  def apply(from: T): U
}

object Iso {
  implicit def iso[U, T <: MappedTo[U]] = macro Macros.isoImpl[T, T#Underlying]
}

object Macros {
  def isoImpl[T <: MappedTo[U]: c.WeakTypeTag, U: c.WeakTypeTag](c: Context): c.Expr[Iso[T, T#Underlying]] = {
    import c.universe._
    reify {
      new Iso[T, T#Underlying] {
        def apply(b: T) = b.id
      }
    }
  }
}