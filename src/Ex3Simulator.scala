import Instructions._
import scala.collection.mutable
import scala.util.control.Breaks

class Ex3Simulator(parsedAssembly:List[Line], invMap:mutable.HashMap[Int,String]) extends Ex3Executor {
  val memory:Array[Int]=new Array[Int](4096)
  parsedAssembly.withFilter(_.hasCell).foreach(
  { instruction =>
    val address = instruction.cell.address
    memory.update(address,instruction.cell.toBin)
  })

  var pc:Int=0x10
  var ac:Int=0
  var e:Boolean = false
  var s:Boolean = true
  var r:Boolean=false
  var ir:Int=0
  var runInstructions=0
  val breakpoints:mutable.TreeSet[Int]=new mutable.TreeSet[Int]()

  Ex3IO.interruptHandler = ()=>{
    r = true
  }
  val dataAddress=parsedAssembly.filter(_.hasCell).filter(_.cell.word.isInstanceOf[Data]).map(_.cell.address)

  def bool2Int(b:Boolean):Int = if(b){1}else{0}

  def printCPUStatus()={
    println("%3d:PC[%03x],AC[%08x],E[%d],R[%d],IR[%08x:%s]".format(runInstructions,
      pc,ac,bool2Int(e),bool2Int(r),ir,Instruction(ir).toString(invMap)))
  }

  def step():Boolean = {
    Ex3IO.execute()
    val old_pc=pc
    if(!r){
      ir = memory.apply(pc)
      pc = pc + 1
      runInstructions += 1
      Instruction(ir).execute(this)
      printCPUStatus()
    }else{
      println("pc=1")
      r=false
      memory.update(0, pc)
      pc = 1
      printCPUStatus()
    }
    breakpoints.exists(_ == old_pc)
  }

  def dumpData() = {
    dataAddress.map(addr=>(addr,memory.apply(addr)))
      .map(t=>MemoryCell(t._1, Data(t._2)).toString(invMap)).foreach(println)
  }

  def setBreakpoint()={
    print("Breakpoint: 0x")
    val bp = Integer.parseInt(Ex3Utils.readLineWithEcho(3),16)
    if(!breakpoints.add(bp)){
      breakpoints.remove(bp)
      println("Remove breakpoint(%03x)".format(bp))
    }else{
      println("Append breakpoint(%03x)".format(bp))
    }
  }

  def run()={
    while(s && !step()){}
  }

  def allDump()={
    //parsedAssembly.filter(_.hasCell).map(_.cell.toString(invMap)).foreach(println)
    parsedAssembly.filter(_.hasCell).map(_.cell).map(cell=>{
      val addr = cell.address
      val word = cell.word match {
        case inst:Instruction => Instruction(memory.apply(addr))
        case data:Data => Data(memory.apply(addr))
        case symbol:Symbol => Symbol(memory.apply(addr))
        case _ => throw new Exception("dump failed")
      }
      MemoryCell(addr, word).toString(invMap)
    }).foreach(println)
  }

  def simulate(){
    dumpData()
    val b1=new Breaks
    val console=new tools.jline.console.ConsoleReader()
    b1.breakable{
      while(s){
        print("[r:run, s:step, q:quit, b:breakpoint, a:alldump, m:datadump]:")
        val c=console.readVirtualKey().toChar
        println(c)
        c match{
          case 's' => step()
          case 'r' => run()
          case 'q' => b1.break()
          case 'm' => dumpData()
          case 'a' => allDump()
          case 'b' => setBreakpoint()
          case _ =>
        }
      }
    }
    println("===========Simulation is quitted==========")
    dumpData()
  }

  def addressing(operand:Int,i:Boolean):Int = {
    if(i){
      memory.apply(operand)
    }else{
      operand
    }
  }

  // N1
  def AND(operand: Int, i: Boolean) {
    val addr = addressing(operand,i)
    ac = ac & memory.apply(addr)
  }

  def ADD(operand: Int, i: Boolean) {
    val addr = addressing(operand,i)
    val temp:Long = ac.toLong + memory.apply(addr)
    ac = (temp & 0xffffffff).toInt
    e = (temp & ~0xffffffff) != 0
  }

  def SUB(operand: Int, i: Boolean) {
    val addr = addressing(operand,i)
    val temp:Long = ac.toLong - memory.apply(addr)
    ac = (temp & 0xffffffff).toInt
    e = (temp & ~0xffffffff) != 0
  }

  def OR(operand: Int, i: Boolean) {
    val addr = addressing(operand,i)
    ac = ac | memory.apply(addr)
  }

  def XOR(operand: Int, i: Boolean) {
    val addr = addressing(operand,i)
    ac = ac ^ memory.apply(addr)
  }

  def LDA(operand: Int, i: Boolean) {
    val addr = addressing(operand,i)
    ac = memory.apply(addr)
  }

  def STA(operand: Int, i: Boolean) {
    val addr = addressing(operand,i)
    memory.update(addr,ac)
  }

  def BUN(operand: Int, i: Boolean) {
    val addr = addressing(operand,i)
    pc = addr
  }

  def JPA(operand: Int, i: Boolean) {
    val addr = addressing(operand,i)
    if(ac>=0){
      pc=addr
    }
  }

  def JNA(operand: Int, i: Boolean) {
    val addr = addressing(operand,i)
    if(ac<0){
      pc=addr
    }
  }

  def JZA(operand: Int, i: Boolean) {
    val addr = addressing(operand,i)
    if(ac==0){
      pc=addr
    }
  }

  def JZE(operand: Int, i: Boolean) {
    val addr = addressing(operand,i)
    if(!e){
      pc=addr
    }
  }

  def BSA(operand: Int, i: Boolean) {
    val addr = addressing(operand,i)
    memory.update(addr,pc)
    pc = addr + 1
  }

  def ISZ(operand: Int, i: Boolean) {
    val addr = addressing(operand,i)
    var tmp = memory.apply(addr)
    tmp += 1
    memory.update(addr,tmp)
    if(tmp==0){ pc+=1 }
  }

  // 11 Inst
  def ADDi(operand: Int, literal: Int, i: Boolean) {
    val addr = addressing(operand,i)
    val temp:Long = memory.apply(addr).toLong + literal.toLong
    ac = (temp & 0xffffffff).toInt
    e = (temp & ~0xffffffff) != 0
  }

  def ANDi(operand: Int, literal: Int, i: Boolean) {
    val addr = addressing(operand,i)
    ac = memory.apply(addr) & literal
  }

  def ORi(operand: Int, literal: Int, i: Boolean) {
    val addr = addressing(operand,i)
    ac = memory.apply(addr) | literal
  }

  def STAi(operand: Int, literal: Int, i: Boolean) {
    val addr = addressing(operand,i)
    memory.update(addr,literal)
  }

  // N2 Inst
  def ADD(operand1: Int, operand2: Int, i: Boolean) {
    val addr2 = addressing(operand2,i)
    val addr1 = operand1
    val temp:Long = memory.apply(addr1).toLong + memory.apply(addr2).toLong
    ac = (temp&0xffffffff).toInt
    e = (temp& ~0xffffffff) != 0
  }

  def SUB(operand1: Int, operand2: Int, i: Boolean) {
    val addr2 = addressing(operand2,i)
    val addr1 = operand1
    ac = memory.apply(addr2) - memory.apply(addr1)
  }

  def AND(operand1: Int, operand2: Int, i: Boolean) {
    val addr2 = addressing(operand2, i)
    val addr1 = operand1
    ac = memory.apply(addr1) & memory.apply(addr2)
  }

  def OR(operand1: Int, operand2: Int, i: Boolean) {
    val addr2 = addressing(operand2,i)
    val addr1 = operand1
    ac = memory.apply(addr1) | memory.apply(addr2)
  }

  def XOR(operand1: Int, operand2: Int, i: Boolean) {
    val addr2 = addressing(operand2,i)
    val addr1 = operand1
    ac = memory.apply(addr1) ^ memory.apply(addr2)
  }

  def MOVE(operand1: Int, operand2: Int, i: Boolean) {
    val addr2 = addressing(operand2,i)
    val addr1 = operand1
    memory.update(addr1, memory.apply(addr2))
  }

  // 1N Inst
  def ADDi(literal: Int) {
    val temp:Long = ac.toLong + literal
    ac = (temp&0xffffffff).toInt
    e = (temp& ~0xffffffff) != 0
  }

  def ANDi(literal: Int) {
    ac &= literal
  }

  def ORi(literal: Int) {
    ac |= literal
  }

  def LDAi(literal: Int) {
    ac = literal
  }

  // NN Inst.
  def CLA() {
    ac = 0
  }

  def CLE() {
    e=false
  }

  def CMA() {
    ac = ~ac
  }

  def CME() {
    e = !e
  }

  def CIR() {
    val co:Int=if(e){0x80000000}else 0
    e=(ac & 1)!=0
    ac >>= 1
    ac = ac | co
  }

  def CIL() {
    val co:Int=if(e){1}else 0
    e=(ac & 0x80000000)!=0
    ac <<=1
    ac = ac | co
  }

  def INC() {
    ac += 1
  }

  def SPA() {
    if(ac>=0){
      pc+=1
    }
  }

  def SNA() {
    if(ac<0){
      pc+=1
    }
  }

  def SZA() {
    if(ac==0){
      pc+=1
    }
  }

  def SZE() {
    if(!e){
      pc+=1
    }
  }

  def HLT() {
    s=false
  }

  def INP() {
    ac &= ~0xff
    ac |= Ex3IO.INP()
  }

  def OUT() {
    Ex3IO.OUT((ac&0xff).toByte)
  }

  def SKI() {
    if(Ex3IO.isReadable){pc+=1}
  }

  def SKO() {
    if(Ex3IO.isWritable){pc+=1}
  }

  def ION() {
    Ex3IO.IEN=true
  }

  def IOF() {
    Ex3IO.IEN=false
  }

  def SIO() {
    Ex3IO.selectPort(isSerial = true)
  }

  def PIO() {
    Ex3IO.selectPort(isSerial = false)
  }

  def IMK() {
    Ex3IO.IMSK = (ac&0xf).toByte
  }
}
