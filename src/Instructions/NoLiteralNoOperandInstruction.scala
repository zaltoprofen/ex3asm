package Instructions
import scala.collection.mutable

object NoLiteralNoOperandInstruction{
  def apply(opCode:Int):NoLiteralNoOperandInstruction = {
    val inst = opCode match {
      case 0x80000001 => "CLA"
      case 0x80000002 => "CLE"
      case 0x80000004 => "CMA"
      case 0x80000008 => "CME"
      case 0x80000010 => "CIR"
      case 0x80000020 => "CIL"
      case 0x80000040 => "INC"
      case 0x80000080 => "SPA"
      case 0x80000100 => "SZA"
      case 0x80000200 => "SNA"
      case 0x80000400 => "SZE"
      case 0x80000800 => "INP"
      case 0x80001000 => "OUT"
      case 0x80002000 => "SKI"
      case 0x80004000 => "SKO"
      case 0x80008000 => "ION"
      case 0x80010000 => "IOF"
      case 0x80020000 => "SIO"
      case 0x80040000 => "PIO"
      case 0x80080000 => "IMK"
      case 0x80100000 => "HLT"
      case _ => throw new Exception(f"This binary is not NN Instruction:$opCode%08x")
    }
    NoLiteralNoOperandInstruction(inst)
  }
}

case class NoLiteralNoOperandInstruction(inst:String) extends Instruction {
  def execute(executor: Ex3Executor) {
    inst match{
      case "CLA" => executor.CLA()
      case "CLE" => executor.CLE()
      case "CMA" => executor.CMA()
      case "CME" => executor.CME()
      case "CIR" => executor.CIR()
      case "CIL" => executor.CIL()
      case "INC" => executor.INC()
      case "SPA" => executor.SPA()
      case "SZA" => executor.SZA()
      case "SNA" => executor.SNA()
      case "SZE" => executor.SZE()
      case "INP" => executor.INP()
      case "OUT" => executor.OUT()
      case "SKI" => executor.SKI()
      case "SKO" => executor.SKO()
      case "ION" => executor.ION()
      case "IOF" => executor.IOF()
      case "SIO" => executor.SIO()
      case "PIO" => executor.PIO()
      case "IMK" => executor.IMK()
      case "HLT" => executor.HLT()
      case _ => throw new Exception("Not NN instruction.:"+inst)
    }
  }

  def toBin: Int = {
    inst match{
      case "CLA" => 0x80000001
      case "CLE" => 0x80000002
      case "CMA" => 0x80000004
      case "CME" => 0x80000008
      case "CIR" => 0x80000010
      case "CIL" => 0x80000020
      case "INC" => 0x80000040
      case "SPA" => 0x80000080
      case "SZA" => 0x80000100
      case "SNA" => 0x80000200
      case "SZE" => 0x80000400
      case "INP" => 0x80000800
      case "OUT" => 0x80001000
      case "SKI" => 0x80002000
      case "SKO" => 0x80004000
      case "ION" => 0x80008000
      case "IOF" => 0x80010000
      case "SIO" => 0x80020000
      case "PIO" => 0x80040000
      case "IMK" => 0x80080000
      case "HLT" => 0x80100000
      case _ => throw new Exception("Not NN instruction.:"+inst)
    }
  }

  override def toString(invMap:mutable.HashMap[Int,String]):String={
    "%s".format(inst)
  }
}
