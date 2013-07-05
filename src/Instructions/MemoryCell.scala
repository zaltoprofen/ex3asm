package Instructions

case class MemoryCell(address:Int, word:Word){
  def toBinStr:String={
    "@%03x %s".format(address, word.toBinStr)
  }

  def toBin:Short = word.toBin
}
