package Instructions

import scala.collection.mutable

case class Symbol(address:Int) extends Word{
  override def toBinStr:String={
    val value = toBin
    f"$value%08x"
  }

  def toBin: Int = {
    address
  }

  def toString(invMap: mutable.HashMap[Int, String]): String = {
    "%3x(%s)".format(address, invMap.get(address).get)
  }
}