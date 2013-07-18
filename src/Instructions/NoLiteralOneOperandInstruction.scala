package Instructions
import scala.collection.mutable

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

  def execute(executor: Ex3Executor) {
    val operand = operand1 & 0xfff
    val indirect_bit = indirect
    inst match{
      case "ADD" => executor.ADD(operand,indirect_bit)
      case "SUB" => executor.SUB(operand,indirect_bit)
      case "AND" => executor.AND(operand,indirect_bit)
      case "OR"  =>  executor.OR(operand,indirect_bit)
      case "XOR" => executor.XOR(operand,indirect_bit)
      case "LDA" => executor.LDA(operand,indirect_bit)
      case "STA" => executor.STA(operand,indirect_bit)
      case "BUN" => executor.BUN(operand,indirect_bit)
      case "BSA" => executor.BSA(operand,indirect_bit)
      case "JPA" => executor.JPA(operand,indirect_bit)
      case "JZA" => executor.JNA(operand,indirect_bit)
      case "JNA" => executor.JZA(operand,indirect_bit)
      case "JZE" => executor.JZE(operand,indirect_bit)
      case "ISZ" => executor.ISZ(operand,indirect_bit)
      case _ => throw new Exception("Not N1 instruction.:"+inst)
    }
  }

  override def toString(invMap:mutable.HashMap[Int,String]):String={
    "%s 0x%03x(%s)%s".format(inst, operand1, invMap.get(operand1).get, if(indirect){" I"}else{""})
  }
}
