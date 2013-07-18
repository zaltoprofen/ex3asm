package Instructions

trait Ex3Executor {
  // N1
  def AND(operand:Int, i:Boolean)
  def ADD(operand:Int, i:Boolean)
  def SUB(operand:Int, i:Boolean)
  def OR(operand:Int, i:Boolean)
  def XOR(operand:Int, i:Boolean)
  def LDA(operand:Int, i:Boolean)
  def STA(operand:Int, i:Boolean)
  def BUN(operand:Int, i:Boolean)
  def JPA(operand:Int, i:Boolean)
  def JNA(operand:Int, i:Boolean)
  def JZA(operand:Int, i:Boolean)
  def JZE(operand:Int, i:Boolean)
  def BSA(operand:Int, i:Boolean)
  def ISZ(operand:Int, i:Boolean)

  // 11 Inst
  def ADDi(operand:Int, literal:Int, i:Boolean)
  def ANDi(operand:Int, literal:Int, i:Boolean)
  def ORi(operand:Int, literal:Int, i:Boolean)
  def STAi(operand:Int, literal:Int, i:Boolean)

  // N2 Inst
  def ADD(operand1:Int,operand2:Int,i:Boolean)
  def SUB(operand1:Int,operand2:Int,i:Boolean)
  def AND(operand1:Int,operand2:Int,i:Boolean)
  def OR(operand1:Int,operand2:Int,i:Boolean)
  def XOR(operand1:Int,operand2:Int,i:Boolean)
  def MOVE(operand1:Int,operand2:Int,i:Boolean)

  // 1N Inst
  def ADDi(literal:Int)
  def ANDi(literal:Int)
  def ORi(literal:Int)
  def LDAi(literal:Int)

  // NN Inst.
  def CLA()
  def CLE()
  def CMA()
  def CME()
  def CIR()
  def CIL()
  def INC()
  def SPA()
  def SNA()
  def SZA()
  def SZE()
  def HLT()
  def INP()
  def OUT()
  def SKI()
  def SKO()
  def ION()
  def IOF()
  def SIO()
  def PIO()
  def IMK()
}
