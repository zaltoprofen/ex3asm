package Instructions

case class Data(value:Int) extends Word{
  override def toBin ={
    value
  }
  override def toBinStr:String={
    "%08x".format(value)
  }
}
