package priv

import scala.language.experimental.macros
import scala.language.higherKinds

object Test {
  def random[IdType <: MappedTo[Long]](implicit iso: Iso[IdType, IdType#Underlying]): IdType => IdType#Underlying =
    iso.apply _

  val baseIdToLong = random[BaseId]
  val baseIdToLong2: BaseId => Long = random
  
  //val stringIdToString: StringId => String = random // error

  class OtherId(val id: Long) extends MappedTo[Long]

  val otherIdToLong = random[OtherId]
}
