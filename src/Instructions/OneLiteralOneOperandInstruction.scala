package Instructions
import scala.collection.mutable

object OneLiteralOneOperandInstruction{
  def apply(opCode:Int):OneLiteralOneOperandInstruction={
    val inst = opCode & (0xff000000 & ~0x40000000) match{
      case 0xa1000000 => "ADD"
      case 0xa2000000 => "AND"
      case 0xa4000000 => "OR"
      case 0xa8000000 => "STA"
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
      case "ADD" => 0xa1000000
      case "AND" => 0xa2000000
      case "OR"  => 0xa4000000
      case "STA" => 0xa8000000
      case _ => throw new Exception("Not 11 inst.:"+inst)
    }
    op | lit | indirect_bit | opcode
  }

  def execute(executor: Ex3Executor) {
    val op = operand1 & 0xfff
    val lit = signExt(literal)
    inst match{
      case "ADD" => executor.ADDi(op,lit,indirect)
      case "AND" => executor.ANDi(op,lit,indirect)
      case "OR"  =>  executor.ORi(op,lit,indirect)
      case "STA" => executor.STAi(op,lit,indirect)
      case _ => throw new Exception("Not 11 inst.:"+inst)
    }
  }


  override def toString(invMap:mutable.HashMap[Int,String]):String={
    "%s 0x%03x(%s) literal:%d %s".format(inst, operand1, invMap.get(operand1).get,
      signExt(literal), if(indirect){" I"}else{""})
  }
}
