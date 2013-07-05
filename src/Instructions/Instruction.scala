package Instructions

import scala.Option

object Instruction{
  def apply(opCode:Short):Instruction={
    opCode & 0xF000 match {
      case 0xF000 => NonRefInstruction(opCode)
      case 0x7000 => NonRefInstruction(opCode)
      case _ => MemRefInstruction(opCode)
    }
  }

  def unapply(opCode:Short):Option[Instruction]={
    Some(Instruction(opCode))
  }
}

abstract class Instruction() extends Word{
  override def toBinStr:String={
      val opCode = toBin
      "%04x".format(opCode)
  }
}
