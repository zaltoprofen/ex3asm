package Instructions

case class Data(value:Int) extends Word{
  override def toBin ={
    value.toShort
  }
  override def toBinStr:String={
    "%04x".format(value.toShort)
  }
}
