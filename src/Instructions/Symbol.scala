package Instructions

case class Symbol(address:Int) extends Word{
  override def toBinStr:String={
    val value = toBin
    f"$value%08x"
  }

  def toBin: Int = {
    address
  }
}