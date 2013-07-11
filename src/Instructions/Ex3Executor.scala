package Instructions

trait Ex3Executor {
  // Memory Ref. Inst.
  def AND(operand:Int, i:Boolean)
  def ADD(operand:Int, i:Boolean)
  def LDA(operand:Int, i:Boolean)
  def STA(operand:Int, i:Boolean)
  def BUN(operand:Int, i:Boolean)
  def BSA(operand:Int, i:Boolean)
  def ISZ(operand:Int, i:Boolean)

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
