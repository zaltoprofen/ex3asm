import Instructions._

class CPUStatus{
  var pc:Int=10
  var ac:Int=0
  var e:Short=0

  val memory:Array[Short]=new Array[Short](4096)
}

class Ex3Simulator(parsedAssembly:List[Line]) {
  val status = new CPUStatus()

  parsedAssembly.withFilter(_.hasCell).foreach(
  { instruction =>
    val address = instruction.cell.address
    status.memory.update(address,instruction.cell.toBin)
  })

  var pc:Int=10
  var ac:Int=0
  var e:Short=0

  def step(){
    val ir = status.memory.apply(pc)
    //TODO Immplement Instruction.execute
    Instruction(ir).execute(status)
  }

  def simulate(){

  }
}
