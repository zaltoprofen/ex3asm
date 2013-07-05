import scala.collection.mutable

case class Line(lineNum:Int, cell:MemoryCell, comment:Comment){
  def hasCell = cell != null
  def hasComment = comment != null
  def toBinStr(conv:mutable.HashMap[String,Int]):String={
    if(hasCell){
      try{
        cell.toBinStr(conv)
      }catch{
        case e:Exception =>
          val newException=new Exception(e.getMessage+" at %d".format(lineNum))
          newException.setStackTrace(e.getStackTrace)
          throw newException
      }
    }else{
      throw new Exception("This line doesn't have binary.")
    }
  }
}

case class Comment(cmt:String)

case class MemoryCell(addr:Int, word:Word){
  def toBinStr(conv:mutable.HashMap[String,Int]):String={
    "@%03x %s".format(addr, word.toBinStr(conv))
  }
  def toBin(conv:mutable.HashMap[String,Int]):Short = word.toBin(conv)
}

trait Word{
  def toBin(conv:mutable.HashMap[String,Int]):Short
  def toBinStr(conv:mutable.HashMap[String,Int]):String
}

//TODO define subclass of Instruction
case class Instruction(inst:String, op:Short, imm:Boolean) extends Word{
  override def toBin(conv:mutable.HashMap[String,Int]):Short = {
    if(op != null){
        val opImm = if(imm){0x8000}else{0x0000}
        val opAddr = op
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
        (opImm | opCode | opAddr).toShort
    } else{
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
  }
  override def toBinStr(conv:mutable.HashMap[String,Int]):String={
      val opCode = toBin(conv)
      "%04x".format(opCode)
  }
  def unapply(opCode:Short):Option[Instruction]={
    opCode & 0xF000 match {
      case 0xF000 => decodeIOInst(opCode)
      case 0x7000 => Some(decodeNonRefInst(opCode))
      case _ => Some(decodeMemRefInst(opCode))
    }
  }

  def decodeMemRefInst(opCode:Short):Instruction={
    Instruction("ADD",0,false)
  }

  def decodeNonRefInst(opCode:Short):Instruction={
    opCode & 0x0FFF match {
      case 0x800 => Instruction("CLA",0,false)
      case 0x400 => Instruction("CLE",0,false)
    }
  }

  def decodeIOInst(opCode:Short):Option[Instruction]={
    Some(Instruction("INP",0,false))
  }
}

case class Data(value:Int) extends Word{
  override def toBin(conv:mutable.HashMap[String,Int])={
    value.toShort
  }
  override def toBinStr(conv:mutable.HashMap[String,Int]):String={
    "%04x".format(value.toShort)
  }
}

case class Symbol(label:String) extends Word{
  override def toBinStr(conv:mutable.HashMap[String,Int]):String={
    val value = toBin(conv)
    "%04x".format(value)
  }

  def toBin(conv: mutable.HashMap[String, Int]): Short = {
    conv.get(label) match{
      case Some(addr) => addr.toShort
      case _=> throw new Exception("Undefined label:"+label)
    }
  }
}