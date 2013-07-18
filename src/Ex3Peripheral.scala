trait PeripheralPort {
  val random = new scala.util.Random
  var interval:Int = -1
  def randomInterval():Int
  val name:String
  var isReady:Boolean
  def interruptProcess()
  def execute()={
    if(interval < 0 && !isReady){
      interval=randomInterval()
      printf("interval[%s]=%d\n",name,interval)
    }
    if(interval == 0){
      isReady=true
      interruptProcess()
    }
    if(interval >= 0){interval -= 1;}
  }

  def isInterrupt:Boolean = interval<=0 && isReady
}


object Ex3IO{
  val console=new tools.jline.console.ConsoleReader()
  object PIN extends PeripheralPort {
    override def randomInterval():Int = random.nextInt(50)+1
    override val name: String = "PIN"
    override var isReady: Boolean = false
    override def interruptProcess()={
      println("Interrupted by PIN!")
      printf("input[%s]=",name)
      val inputData=console.readVirtualKey().toByte
      printf("%c\n",inputData.toChar)
      INPR=inputData
    }
  }

  object POU extends PeripheralPort {
    override def randomInterval():Int = 1
    override val name: String = "POU"
    override var isReady:Boolean = false
    override def interruptProcess()={
      println("Interrupted by POU!")
    }
  }

  object SIN extends PeripheralPort {
    override def randomInterval():Int = random.nextInt(50)+1
    override val name: String = "SIN"
    override var isReady: Boolean = false
    override def interruptProcess()={
      println("Interrupted by SIN!")
      printf("input[%s]=",name)
      val inputData=Console.in.read.toByte
      printf("%c\n",inputData.toChar)
      INPR=inputData
    }
  }

  object SOU extends PeripheralPort {
    override def randomInterval():Int = 1
    override val name: String = "SOU"
    override var isReady:Boolean = false
    override def interruptProcess()={
      println("Interrupted by SOU!")
    }
  }

  var interruptHandler:() => Unit = null

  var IOT:Boolean=false
  var IEN:Boolean=false
  var IMSK:Byte=0

  def execute()={
    if(IEN){
      if((IMSK & 0x2)!=0){PIN.execute()}
      if((IMSK & 0x1)!=0){POU.execute()}
      if((IMSK & 0x8)!=0){SIN.execute()}
      if((IMSK & 0x4)!=0){SOU.execute()}
      if(PIN.isReady && (IMSK & 0x2)!=0 || SIN.isReady && (IMSK&0x8) != 0 ||
        POU.isReady && (IMSK & 0x1)!=0 || SOU.isReady && (IMSK & 0x4) !=0){
        IEN=false
        interruptHandler()
      }
    }
  }

  def selectPort(isSerial:Boolean)={
    IOT=isSerial
  }

  var INPR:Byte = 0

  def INP():Byte={
    if(IOT){
      SIN.isReady=false
    }else{
      PIN.isReady=false
    }
    INPR
  }

  def isReadable ={
    if(IOT){
      SIN.isReady
    }else{
      PIN.isReady
    }
  }

  def isWritable ={
    if(IOT){
      SOU.isReady
    }else{
      POU.isReady
    }
  }

  def OUT(value:Byte)={
    if(IOT){
      SOU.isReady = false
    }else{
      POU.isReady = false
    }
    println("output[%s]='%c'".format(currentPortName, value.toChar))
  }

  def currentPortName:String={
    if(IOT){"SIO"}else{"PIO"}
  }
}