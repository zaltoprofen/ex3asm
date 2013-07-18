package Instructions

import scala.collection.mutable

case class Data(value:Int) extends Word{
  override def toBin ={
    value
  }
  override def toBinStr:String={
    "%08x".format(value)
  }

  def toString(invMap: mutable.HashMap[Int, String]):String={
    "%08x(%d)".format(value, value)
  }
}
