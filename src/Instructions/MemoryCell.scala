package Instructions
import scala.collection.mutable

case class MemoryCell(address:Int, word:Word){
  def toBinStr:String={
    "@%03x %s".format(address, word.toBinStr)
  }

  def toBin:Int = word.toBin

  def toString(invMap:mutable.HashMap[Int,String]):String={
    "%5s: %03x: %s".format(addressResolve(invMap,address), address, word.toString(invMap))
  }

  def addressResolve(invMap:mutable.HashMap[Int,String], address:Int)={
    invMap.get(address) match {
      case Some(label) => label
      case None => ""
    }
  }
}
