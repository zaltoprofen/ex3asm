import Instructions._

class CPUStatus{
  var pc:Int=10
  var ac:Int=0
  var e:Int=0
}

class Ex3Simulator(parsedAssembly:List[Line]) extends Ex3Executor {
  val status = new CPUStatus()

  val memory:Array[Int]=new Array[Int](4096)
  parsedAssembly.withFilter(_.hasCell).foreach(
  { instruction =>
    val address = instruction.cell.address
    memory.update(address,instruction.cell.toBin)
  })

  var pc:Int=10
  var ac:Int=0
  var e:Boolean = false
  var s:Boolean = false
  var inpr:Byte = 0

  def step(){
    val ir = memory.apply(pc)
    pc = pc + 1
    Instruction(ir).execute(this)
  }

  def simulate(){

  }

  // Memory Ref. Inst.
  def AND(operand: Int, i: Boolean) {
    val op = if(i){memory.apply(operand)}else operand
    ac = (ac & memory.apply(op)).toInt
  }

  def ADD(operand: Int, i: Boolean) {
    val op = if(i){memory.apply(operand)}else operand
    val t = ac.toInt + memory.apply(op).toInt
    e = (t & ~0xFFFF) != 0
  }

  def LDA(operand: Int, i: Boolean) {
    val op = if(i){memory.apply(operand)}else operand
    ac = memory.apply(op)
  }

  def STA(operand: Int, i: Boolean) {
    val op = if(i){memory.apply(operand)}else operand
    memory.update(op, ac)
  }

  def BUN(operand: Int, i: Boolean) {
    val op = if(i){memory.apply(operand)}else operand
    ac = op
  }

  def BSA(operand: Int, i: Boolean) {
    val op = if(i){memory.apply(operand)}else operand
    memory.update(op, pc)
    pc = (op + 1).toInt
  }

  def ISZ(operand: Int, i: Boolean) {
    val op = if(i){memory.apply(operand)}else operand
    val t:Int = (memory.apply(op) + 1).toInt
    if(t==0) pc = (pc + 1).toInt
    memory.update(operand, t)
  }

  // Other Inst.
  def CLA() { ac = 0 }

  def CLE() { e = false}

  def CMA() { ac = (~ac).toInt }

  def CME() { e = !e}

  def CIR() {
    val carry:Int = (if(e){0x8000}else 0x0000).toInt
    e = (ac & 0x0001) == 0x0001
    ac = (ac.>>(1) | carry).toInt
  }

  def CIL() {
    val carry:Int = (if(e){0x0001}else 0x0000).toInt
    e = (ac & 0x8000) == 0x8000
    ac = (ac.<<(1) | carry).toInt
  }

  def INC() { ac = (ac + 1).toInt }

  def SPA() { if(ac >= 0) pc = (pc + 1).toInt }

  def SNA() { if(ac < 0) pc = (pc + 1).toInt }

  def SZA() { if(ac == 0) pc = (pc + 1).toInt }

  def SZE() { if(!e) pc = (pc + 1).toInt }

  def HLT() { s = true }

  def INP() { ac = inpr}

  var outr:Int=0

  def OUT() { outr = ac }

  def SKI() {}

  def SKO() {}

  def ION() {}

  def IOF() {}

  def SIO() {}

  def PIO() {}

  def IMK() {}
}
