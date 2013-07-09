package Instructions

trait Ex3Executor {
  // Memory Ref. Inst.
  def AND(operand:Short, i:Boolean)
  def ADD(operand:Short, i:Boolean)
  def LDA(operand:Short, i:Boolean)
  def STA(operand:Short, i:Boolean)
  def BUN(operand:Short, i:Boolean)
  def BSA(operand:Short, i:Boolean)
  def ISZ(operand:Short, i:Boolean)

  // Other Inst.
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
