package Instructions

import scala.Option
import scala.collection.mutable

object Instruction{
  def apply(opCode:Int):Instruction={
    ((opCode & 0xE0000000) >> 29)&7 match {
      case 0x0 => NoLiteralOneOperandInstruction(opCode)
      case 0x1 => TwoOperandInstruction(opCode)
      case 0x2 => NoLiteralOneOperandInstruction(opCode)
      case 0x3 => TwoOperandInstruction(opCode)
      case 0x4 => NoLiteralNoOperandInstruction(opCode)
      case 0x5 => OneLiteralOneOperandInstruction(opCode)
      case 0x6 => OneLiteralNoOperandInstruction(opCode)
      case 0x7 => OneLiteralOneOperandInstruction(opCode)
    }
  }

  def unapply(opCode:Int):Option[Instruction]={
    Some(Instruction(opCode))
  }
}

abstract class Instruction() extends Word{
  override def toBinStr:String={
      val opCode = toBin
      f"$opCode%08x"
  }
  def execute(executor:Ex3Executor)
  def signExt(in:Int):Int={
    val in12=in & 0xfff
    if((0x800&in12)!=0){0xfffff000 | in12}
    else in12
  }
}
