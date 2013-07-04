import scala.collection.mutable
import util.parsing.combinator._
import scala.io._
import java.io.PrintWriter

class Ex3Parser extends RegexParsers{
  override val whiteSpace="[ \t]*".r
  var labels: mutable.HashMap[String,Int]=mutable.HashMap.empty[String,Int]
  var lineNum:Int =1
  var cur:Int =0

  val memInst="A[ND]D|LDA|STA|BUN|BSA|ISZ".r
  val nonMemInst="C[LM][AE]|CI[LR]|INC|S[PNZ]A|SZE|HLT".r
  val ioInst="INP|OUT|SK[IO]|IO[FN]|[SP]IO|IMK".r
  val label="[_a-zA-Z0-9]+".r
  val dataType="HEX|DEC|CHR|SYM".r
  val addressPrefix="ORG".r

  val inst_memory_ref=memInst ~ label ~ "I".r.? ^^ {
    case op ~ lbl ~ Some("I") =>
      MemoryCell(cur, Instruction(op, lbl, imm = true))
    case op ~ lbl ~ None =>
      MemoryCell(cur, Instruction(op, lbl, imm = false))
  }
  val inst_non_memory_ref=nonMemInst ^^ {
    result=>
      MemoryCell(cur, Instruction(result, null, imm = false))
  }
  val inst_io=ioInst ^^ {
    result=>
      MemoryCell(cur, Instruction(result, null, imm = false))
  }
  val define_label=label <~ ',' ^^ {
    result => {
      if(labels.exists(_._1==result)){
        throw new Exception("the definition of '"+result+"' duplicated at " + lineNum)
      }
      labels += (result -> cur)
      result -> cur
    }
  }
  val data= ("HEX".r ~> "[0-9a-fA-F]+".r ^^ { res => Integer.parseInt(res,16) } |
    "DEC".r ~> "[+-]?[0-9]+".r ^^ { res => Integer.parseInt(res)} |
    "CHR".r ~> "[_0-9a-zA-Z]".r ^^ { res => res.toCharArray.apply(0).toInt }) ^^
    {res => MemoryCell(cur, Data(res))}
  val symbol = "SYM".r ~> label ^^ {res=> MemoryCell(cur, Symbol(res))}
  val address = addressPrefix ~> "[0-9a-fA-F]+".r ^^ { result => cur = Integer.parseInt(result,16) }

  val comment="/.*".r
  val eol= opt('\r') <~ '\n'

  val line=whiteSpace.? ~> address.? ~> define_label.* ~> ((inst_memory_ref | inst_non_memory_ref | inst_io | data | symbol)
    ~ comment.? ^^ {
    result => cur += 1;
      result match {
        case word ~ Some(cmt) => Line(lineNum, word, Comment(cmt))
        case word ~ None => Line(lineNum, word, null)
      }
  } | (comment.?) ^^ {
      case Some(cmt) => Line(lineNum, null, Comment(cmt))
      case None => Line(lineNum, null, null)
  }) ^^ { line => lineNum+=1; line }

  val program = rep(line.? <~ eol ^^ {
    case Some(line) => line
    case None => Line(lineNum, null, null)
  }) <~ "END" <~ eol.?

  def parse(src:String):List[String]={
    labels = mutable.HashMap.empty[String,Int]
    cur = 0
    try{
      parseAll(program,src) match {
        case Success(res,nxt) =>
          val instructions = res.filter(_.hasCell).map(_.cell)
          instructions.map(_.toBinStr(labels))
        case Failure(msg,nxt) =>
          System.err.println(Failure(msg,nxt))
          sys.exit(1)
        case Error(_,_)=>
          System.err.println("Unknown Error")
          sys.exit(1)
      }
    }catch {
      case e:Exception =>
        System.err.println(e.getMessage)
        sys.exit(1)
    }
  }
}

object MainObj{
  val freg="""([/0-9a-zA-Z\._\-]+)\.asm""".r
  def main(args: Array[String]):Unit={
    if(args.length!=1){
      System.err.println("Invalid argument (insufficient)")
      sys.exit(1)
    }
    var filename:String=null
    args(0) match {
      case freg(fn) => filename=fn
      case _ =>
        System.err.println("Invalid argument (Not .asm)")
        sys.exit(1)
    }
    val source=Source.fromFile(args(0))
    var src = source.getLines().toList
    val parser=new Ex3Parser()
    val result=parser.parse(src.mkString("\n"))
    val writer=new PrintWriter(filename+".mem")
    result.foreach(writer.println)
    writer.close()
  }
}
