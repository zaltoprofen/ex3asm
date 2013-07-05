package Instructions

import scala.Option

object Instruction{
  def unapply(opCode:Short):Option[Instruction]={
    opCode & 0xF000 match {
      case 0xF000 => None
      case 0x7000 => None
      case _ => Some(MemRefInstruction(opCode))
    }
  }
}

abstract class Instruction() extends Word{
  override def toBinStr:String={
      val opCode = toBin
      "%04x".format(opCode)
  }
}
