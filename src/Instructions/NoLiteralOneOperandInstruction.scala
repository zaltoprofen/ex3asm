package Instructions

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
      case "JPA" => 0x00100000
      case "JZA" => 0x00200000
      case "JNA" => 0x00400000
      case "JZE" => 0x00800000
      case "ISZ" => 0x01000000
      case _ => throw new Exception("Not N1 instruction.")
    }
    operand | indirect_bit | opcode
  }

  def execute(executor: Ex3Executor) {}
}
