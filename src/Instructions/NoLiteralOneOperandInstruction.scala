package Instructions

object NoLiteralOneOperandInstruction{
  def apply(opCode:Int):NoLiteralOneOperandInstruction ={
    val inst = opCode & (0xfffff000 & ~0x40000000) match{
      case 0x00001000 => "ADD"
      case 0x00002000 => "SUB"
      case 0x00004000 => "AND"
      case 0x00008000 => "OR"
      case 0x00010000 => "XOR"
      case 0x00020000 => "LDA"
      case 0x00040000 => "STA"
      case 0x00080000 => "BUN"
      case 0x00100000 => "BSA"
      case 0x00200000 => "JPA"
      case 0x00400000 => "JZA"
      case 0x00800000 => "JNA"
      case 0x01000000 => "JZE"
      case 0x02000000 => "ISZ"
      case _ => throw new Exception(f"This binary is not N1 instruction:$opCode%08x")
    }
    val op1 = opCode & 0xfff
    val indirect = (opCode & 0x40000000) != 0
    NoLiteralOneOperandInstruction(inst, op1, indirect)
  }
}

case class NoLiteralOneOperandInstruction(inst:String, operand1:Int, indirect:Boolean) extends Instruction {
  override def toBin:Int={
    val operand = operand1 & 0xfff
    val indirect_bit = if(indirect){0x40000000}else 0
    val opcode = inst match{
      case "ADD" => 0x00001000
      case "SUB" => 0x00002000
      case "AND" => 0x00004000
      case "OR"  => 0x00008000
      case "XOR" => 0x00010000
      case "LDA" => 0x00020000
      case "STA" => 0x00040000
      case "BUN" => 0x00080000
      case "BSA" => 0x00100000
      case "JPA" => 0x00200000
      case "JZA" => 0x00400000
      case "JNA" => 0x00800000
      case "JZE" => 0x01000000
      case "ISZ" => 0x02000000
      case _ => throw new Exception("Not N1 instruction.:"+inst)
    }
    operand | indirect_bit | opcode
  }

  def execute(executor: Ex3Executor) {}
}
