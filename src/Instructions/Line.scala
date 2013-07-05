package Instructions

case class Line(lineNum:Int, cell:MemoryCell, comment:Comment){
  def hasCell = cell != null
  def hasComment = comment != null
  def toBinStr:String={
    if(hasCell){
      try{
        cell.toBinStr
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
