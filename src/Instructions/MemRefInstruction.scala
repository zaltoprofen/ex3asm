package Instructions

object MemRefInstruction{
  def apply(opCode:Short):MemRefInstruction={
    val instName = opCode & 0x7000 match {
      case 0x0000 => "AND"
      case 0x1000 => "ADD"
      case 0x2000 => "LDA"
      case 0x3000 => "STA"
      case 0x4000 => "BUN"
      case 0x5000 => "BSA"
      case 0x6000 => "ISZ"
      case _ => throw new Exception("This binary is not MemRefInstruction:0x%04x".format(opCode))
    }
    val operand = opCode & 0x0FFF
    val imm = (opCode & 0x8000) == 0x8000
    MemRefInstruction(instName, operand.toShort, imm)
  }
}

case class MemRefInstruction(inst:String, operand:Short, imm:Boolean) extends Instruction{
  override def toBin:Short = {
    val opImm = if(imm){0x8000}else{0x0000}
    val opCode = inst match {
      case "AND" => 0x0000
      case "ADD" => 0x1000
      case "LDA" => 0x2000
      case "STA" => 0x3000
      case "BUN" => 0x4000
      case "BSA" => 0x5000
      case "ISZ" => 0x6000
      case _ => throw new Exception("Undefined instruction:"+inst)
    }
    (opImm | opCode | operand).toShort
  }
}
