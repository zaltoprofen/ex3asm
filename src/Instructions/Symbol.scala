package Instructions

case class Symbol(address:Short) extends Word{
  override def toBinStr:String={
    val value = toBin
    f"$value%04x"
  }

  def toBin: Short = {
    address
  }
}