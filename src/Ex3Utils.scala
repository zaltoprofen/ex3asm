import scala.collection.mutable

object Ex3Utils {
  def hex2Int(hex:String):Int=
    if(hex.length>8){
      throw new IllegalArgumentException("8桁以上の16進文字列はparseできません")
    }else if(hex.length == 0){
      throw new IllegalArgumentException("0桁の16進文字列はparseできません")
    }else if(hex.length == 8){
      val top4 = Integer.parseInt(hex(0).toString,16)
      val sign3 = if(top4>=8){
        (-1,top4-8)
      }else{
        (+1,top4)
      }
      sign3._1*sign3._2*0x10000000+Integer.parseInt(hex drop 1,16)
    }else{
      Integer.parseInt(hex,16)
    }

  def inverseMap(map:mutable.HashMap[String,Int]):mutable.HashMap[Int,String]={
    val addresses = map.values
    val expansion = map.toArray
    val invMap= new mutable.HashMap[Int,String]()
    addresses.foreach( addr => {
      val label = expansion.find( tuple => tuple._2 == addr ) match{
        case Some(tuple) => tuple._1
        case None => ""
      }
      invMap += ((addr, label))
    })
    invMap
  }

  def addressResolve(invMap:mutable.HashMap[Int,String], address:Int)={
    invMap.get(address) match {
      case Some(label) => label
      case None => ""
    }
  }

  def readLineWithEcho(count:Int):String={
    var i=0
    var c:Char=' '
    val console=new tools.jline.console.ConsoleReader()
    var buffer:String =""
    while(i<count){
      c = console.readVirtualKey().toChar
      print(c)
      if(c=='\n') return buffer
      buffer += c
      i+=1
    }
    buffer
  }
}
