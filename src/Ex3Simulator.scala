import Instructions._

class CPUStatus{
  var pc:Int=10
  var ac:Int=0
  var e:Short=0
}

class Ex3Simulator(parsedAssembly:List[Line]) extends Ex3Executor {
  val status = new CPUStatus()

  val memory:Array[Short]=new Array[Short](4096)
  parsedAssembly.withFilter(_.hasCell).foreach(
  { instruction =>
    val address = instruction.cell.address
    memory.update(address,instruction.cell.toBin)
  })

  var pc:Short=10
  var ac:Short=0
  var e:Boolean = false
  var s:Boolean = false
  var inpr:Byte = 0

  def step(){
    val ir = memory.apply(pc)
    pc = (pc + 1).toShort
    Instruction(ir).execute(this)
  }

  def simulate(){

  }

  // Memory Ref. Inst.
  def AND(operand: Short, i: Boolean) {
    val op = if(i){memory.apply(operand)}else operand
    ac = (ac & memory.apply(op)).toShort
  }

  def ADD(operand: Short, i: Boolean) {
    val op = if(i){memory.apply(operand)}else operand
    val t = ac.toInt + memory.apply(op).toInt
    e = (t & ~0xFFFF) != 0
  }

  def LDA(operand: Short, i: Boolean) {
    val op = if(i){memory.apply(operand)}else operand
    ac = memory.apply(op)
  }

  def STA(operand: Short, i: Boolean) {
    val op = if(i){memory.apply(operand)}else operand
    memory.update(op, ac)
  }

  def BUN(operand: Short, i: Boolean) {
    val op = if(i){memory.apply(operand)}else operand
    ac = op
  }

  def BSA(operand: Short, i: Boolean) {
    val op = if(i){memory.apply(operand)}else operand
    memory.update(op, pc)
    pc = (op + 1).toShort
  }

  def ISZ(operand: Short, i: Boolean) {
    val op = if(i){memory.apply(operand)}else operand
    val t:Short = (memory.apply(op) + 1).toShort
    if(t==0) pc = (pc + 1).toShort
    memory.update(operand, t)
  }

  // Other Inst.
  def CLA() { ac = 0 }

  def CLE() { e = false}

  def CMA() { ac = (~ac).toShort }

  def CME() { e = !e}

  def CIR() {
    val carry:Short = (if(e){0x8000}else 0x0000).toShort
    e = (ac & 0x0001) == 0x0001
    ac = (ac.>>(1) | carry).toShort
  }

  def CIL() {
    val carry:Short = (if(e){0x0001}else 0x0000).toShort
    e = (ac & 0x8000) == 0x8000
    ac = (ac.<<(1) | carry).toShort
  }

  def INC() { ac = (ac + 1).toShort }

  def SPA() { if(ac >= 0) pc = (pc + 1).toShort }

  def SNA() { if(ac < 0) pc = (pc + 1).toShort }

  def SZA() { if(ac == 0) pc = (pc + 1).toShort }

  def SZE() { if(!e) pc = (pc + 1).toShort }

  def HLT() { s = true }

  def INP() { ac = inpr}

  var outr:Short=0

  def OUT() { outr = ac }

  def SKI() {}

  def SKO() {}

  def ION() {}

  def IOF() {}

  def SIO() {}

  def PIO() {}

  def IMK() {}
}
