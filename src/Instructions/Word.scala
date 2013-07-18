package Instructions
import scala.collection.mutable

trait Word{
  def toBin:Int
  def toBinStr:String
  def toString(invMap:mutable.HashMap[Int,String]):String
}
