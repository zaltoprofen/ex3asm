package Instructions

case class OneLiteralNoOperandInstruction(inst:String, literal:Int) extends Instruction {
  def execute(executor: Ex3Executor) {}

  override def toBin: Int ={
    val lit = literal & 0xfff
    val opcode = inst match{
      case "ADDI" => 0xc1000000
      case "ANDI" => 0xc2000000
      case "ORI"  => 0xc4000000
      case "LDAI" => 0xc8000000
      case _ => throw new Exception("Not 1N Instruction")
    }
    opcode | lit
  }
}
