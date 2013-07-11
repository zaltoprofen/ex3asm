package Instructions

object TwoOperandInstruction{
  def apply(opCode:Int):TwoOperandInstruction={
    val inst = opCode & (0xff000000 & ~0x40000000) match{
      case 0x21000000 => "ADD"
      case 0x22000000 => "SUB"
      case 0x23000000 => "AND"
      case 0x24000000 => "OR"
      case 0x25000000 => "XOR"
      case 0x26000000 => "MOVE"
    }
    val op1 = (opCode & 0x00fff000) >> 12
    val op2 = opCode & 0x00000fff
    val indirect = (opCode & 0x40000000) != 0
    TwoOperandInstruction(inst, op1, op2, indirect)
  }
}

case class TwoOperandInstruction(inst:String, operand1:Int, operand2:Int, indirect:Boolean) extends Instruction{
  def execute(executor: Ex3Executor) {}

  def toBin: Int = {
    val op1 = (operand1 & 0xfff) << 12
    val op2 = operand2 & 0xfff
    val indirect_bit = if(indirect){0x40000000}else 0
    val opcode = inst match{
      case "ADD" => 0x21000000
      case "SUB" => 0x22000000
      case "AND" => 0x23000000
      case "OR"  => 0x24000000
      case "XOR" => 0x25000000
      case "MOVE"=> 0x26000000
      case _ => throw new Exception("Not N2 instruction.:"+inst)
    }
    opcode | indirect_bit | op1 | op2
  }
}
