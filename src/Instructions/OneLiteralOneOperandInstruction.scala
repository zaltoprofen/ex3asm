package Instructions

object OneLiteralOneOperandInstruction{
  def apply(opCode:Int):OneLiteralOneOperandInstruction={
    val inst = opCode & (0xff000000 & ~0x40000000) match{
      case 0xa1000000 => "ADDI"
      case 0xa2000000 => "ANDI"
      case 0xa4000000 => "ORI"
      case 0xa8000000 => "STAI"
      case _ => throw new Exception(f"This binary is not 11 instruction:$opCode%08x")
    }
    val operand = (opCode & 0x00fff000) >> 12
    val literal = opCode & 0x00000fff
    val indirect = (opCode & 0x40000000) != 0
    OneLiteralOneOperandInstruction(inst, operand, literal, indirect)
  }
}

case class OneLiteralOneOperandInstruction(inst:String, operand1:Int, literal:Int, indirect:Boolean) extends Instruction{
  override def toBin:Int={
    val op = (operand1 & 0xfff) << 12
    val lit = literal & 0xfff
    val indirect_bit = if(indirect){0x40000000}else 0
    val opcode = inst match{
      case "ADDI" => 0xa1000000
      case "ANDI" => 0xa2000000
      case "ORI"  => 0xa4000000
      case "STAI" => 0xa8000000
      case _ => throw new Exception("Not 11 inst.:"+inst)
    }
    op | lit | indirect_bit | opcode
  }

  def execute(executor: Ex3Executor) {}
}
