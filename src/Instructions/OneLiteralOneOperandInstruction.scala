package Instructions

case class OneLiteralOneOperandInstruction(inst:String, operand1:Int, literal:Int, indirect:Boolean) extends Instruction{
  override def toBin:Int={
    val op = (operand1 & 0xfff) << 12
    val lit = literal & 0xfff
    val indirect_bit = if(indirect){0x40000000}else 0
    val opcode = inst match{
      case "ADDI" => 0xa1000000
      case "ANDI" => 0xa2000000
      case "ORI" => 0xa4000000
      case "STAI" => 0xa8000000
      case _ => throw new Exception("Not 11 inst.")
    }
    op | lit | indirect_bit | opcode
  }

  def execute(executor: Ex3Executor) {}
}
