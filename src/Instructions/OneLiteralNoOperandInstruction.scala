package Instructions
import scala.collection.mutable

object OneLiteralNoOperandInstruction{
  def apply(opCode:Int):OneLiteralNoOperandInstruction={
    val inst = opCode & 0xff000000 match {
      case 0xc1000000 => "ADD"
      case 0xc2000000 => "AND"
      case 0xc4000000 => "OR"
      case 0xc8000000 => "LDA"
      case _ => throw new Exception(f"This binary is not 1N instruction:$opCode%08x")
    }
    val literal = opCode & 0xfff
    OneLiteralNoOperandInstruction(inst, literal)
  }
}

case class OneLiteralNoOperandInstruction(inst:String, literal:Int) extends Instruction {
  def execute(executor: Ex3Executor) {
    val lit = signExt(literal)
    inst match{
      case "ADD" => executor.ADDi(lit)
      case "AND" => executor.ANDi(lit)
      case "OR"  =>  executor.ORi(lit)
      case "LDA" => executor.LDAi(lit)
      case _ => throw new Exception("Not 1N Instruction.:"+inst)
    }
  }

  override def toBin: Int ={
    val lit = literal & 0xfff
    val opcode = inst match{
      case "ADD" => 0xc1000000
      case "AND" => 0xc2000000
      case "OR"  => 0xc4000000
      case "LDA" => 0xc8000000
      case _ => throw new Exception("Not 1N Instruction.:"+inst)
    }
    opcode | lit
  }


  override def toString(invMap:mutable.HashMap[Int,String]):String={
    "%s literal:%d".format(inst, signExt(literal))
  }
}
