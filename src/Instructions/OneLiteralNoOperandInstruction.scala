package Instructions

object OneLiteralNoOperandInstruction{
  def apply(opCode:Int):OneLiteralNoOperandInstruction={
    val inst = opCode & 0xff000000 match {
      case 0xc1000000 => "ADDI"
      case 0xc2000000 => "ANDI"
      case 0xc4000000 => "ORI"
      case 0xc8000000 => "LDAI"
      case _ => throw new Exception(f"This binary is not 1N instruction:$opCode%08x")
    }
    val literal = opCode & 0xfff
    OneLiteralNoOperandInstruction(inst, literal)
  }
}

case class OneLiteralNoOperandInstruction(inst:String, literal:Int) extends Instruction {
  def execute(executor: Ex3Executor) {}

  override def toBin: Int ={
    val lit = literal & 0xfff
    val opcode = inst match{
      case "ADDI" => 0xc1000000
      case "ANDI" => 0xc2000000
      case "ORI"  => 0xc4000000
      case "LDAI" => 0xc8000000
      case _ => throw new Exception("Not 1N Instruction.:"+inst)
    }
    opcode | lit
  }
}
