import scala.collection._
class Ex3Simulator(parsedAssembly:List[Line], label2Address:mutable.HashMap[String,Int]) {
  val memory:Array[Short]=new Array[Short](4096)
  parsedAssembly.withFilter(_.hasCell).foreach(
  { instruction =>
    val addr = instruction.cell.addr
    memory.update(addr,instruction.cell.toBin(label2Address))
  })

  var pc:Int=10
  var ac:Int=0
  var e:Short=0

  def step(){
    val ir=memory.apply(pc)
  }

  def simulate(){

  }
}
