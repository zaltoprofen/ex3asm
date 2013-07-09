package Instructions

object NonRefInstruction{
  def apply(opCode:Short):NonRefInstruction = {
    opCode match{
      case 0x7800 => NonRefInstruction("CLA")
      case 0x7400 => NonRefInstruction("CLE")
      case 0x7200 => NonRefInstruction("CMA")
      case 0x7100 => NonRefInstruction("CME")
      case 0x7080 => NonRefInstruction("CIR")
      case 0x7040 => NonRefInstruction("CIL")
      case 0x7020 => NonRefInstruction("INC")
      case 0x7010 => NonRefInstruction("SPA")
      case 0x7008 => NonRefInstruction("SNA")
      case 0x7004 => NonRefInstruction("SZA")
      case 0x7002 => NonRefInstruction("SZE")
      case 0x7001 => NonRefInstruction("HLT")
      case 0xF800 => NonRefInstruction("INP")
      case 0xF400 => NonRefInstruction("OUT")
      case 0xF200 => NonRefInstruction("SKI")
      case 0xF100 => NonRefInstruction("SKO")
      case 0xF080 => NonRefInstruction("ION")
      case 0xF040 => NonRefInstruction("IOF")
      case 0xF020 => NonRefInstruction("SIO")
      case 0xF010 => NonRefInstruction("PIO")
      case 0xF008 => NonRefInstruction("IMK")
      case _ => throw new Exception("This binary is not NonRefInstruction:0x%04x".format(opCode))
    }
  }
}

case class NonRefInstruction(inst:String) extends Instruction {
  override def toBin:Short = {
    val opCode = inst match{
      case "CLA" => 0x7800
      case "CLE" => 0x7400
      case "CMA" => 0x7200
      case "CME" => 0x7100
      case "CIR" => 0x7080
      case "CIL" => 0x7040
      case "INC" => 0x7020
      case "SPA" => 0x7010
      case "SNA" => 0x7008
      case "SZA" => 0x7004
      case "SZE" => 0x7002
      case "HLT" => 0x7001
      case "INP" => 0xF800
      case "OUT" => 0xF400
      case "SKI" => 0xF200
      case "SKO" => 0xF100
      case "ION" => 0xF080
      case "IOF" => 0xF040
      case "SIO" => 0xF020
      case "PIO" => 0xF010
      case "IMK" => 0xF008
      case _ => throw new Exception("Undefined instruction:"+inst)
    }
    opCode.toShort
  }

  def execute(executor: Ex3Executor) = {
    inst match{
      case "CLA" => executor.CLA()
      case "CLE" => executor.CLE()
      case "CMA" => executor.CMA()
      case "CME" => executor.CME()
      case "CIR" => executor.CIR()
      case "CIL" => executor.CIL()
      case "INC" => executor.INC()
      case "SPA" => executor.SPA()
      case "SNA" => executor.SNA()
      case "SZA" => executor.SZA()
      case "SZE" => executor.SZE()
      case "HLT" => executor.HLT()
      case "INP" => executor.INP()
      case "OUT" => executor.OUT()
      case "SKI" => executor.SKI()
      case "SKO" => executor.SKO()
      case "ION" => executor.ION()
      case "IOF" => executor.IOF()
      case "SIO" => executor.SIO()
      case "PIO" => executor.PIO()
      case "IMK" => executor.IMK()
      case _ => throw new Exception("Undefined instruction:"+inst)
    }
  }
}
