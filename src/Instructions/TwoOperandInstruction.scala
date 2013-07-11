package Instructions

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
      case _ => throw new Exception("Not N2 instruction")
    }
    opcode | indirect_bit | op1 | op2
  }
}
