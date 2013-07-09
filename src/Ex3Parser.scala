import Instructions._
import Instructions.Data
import Instructions.Instruction
import Instructions.Line
import Instructions.MemoryCell
import scala.collection.mutable
import scala.Some
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
  val label= "[_a-zA-Z0-9]+".r
  val dataType="HEX|DEC|CHR|SYM".r
  val addressPrefix="ORG".r

  val operand = label ^^ {
    label_n =>
    labels.get(label_n) match {
      case Some(address_n) => address_n.toShort
      case _ => throw new Exception("Undefined label:" + label_n)
    }
  }

  val inst_memory_ref=memInst ~ operand ~ "I".r.? ^^ {
    case op ~ lbl ~ Some("I") =>
      MemoryCell(cur, MemRefInstruction(op, lbl, imm = true))
    case op ~ lbl ~ None =>
      MemoryCell(cur, MemRefInstruction(op, lbl, imm = false))
  }
  val inst_non_memory_ref=nonMemInst ^^ {
    result=>
      MemoryCell(cur, NonRefInstruction(result))
  }
  val inst_io=ioInst ^^ {
    result=>
      MemoryCell(cur, NonRefInstruction(result))
  }
  val define_label=label <~ ','

  val data= ("HEX".r ~> "[0-9a-fA-F]+".r ^^ { res => Integer.parseInt(res,16) } |
    "DEC".r ~> "[+-]?[0-9]+".r ^^ { res => Integer.parseInt(res)} |
    "CHR".r ~> "[_0-9a-zA-Z]".r ^^ { res => res.toCharArray.apply(0).toInt }) ^^
    {res => MemoryCell(cur, Data(res))}
  val symbol = "SYM".r ~> operand ^^ {res=> MemoryCell(cur, Symbol(res))}
  val address = addressPrefix ~> "[0-9a-fA-F]+".r ^^ { result => cur = Integer.parseInt(result,16) }

  val comment="/.*".r
  val eol= opt('\r') <~ '\n'

  val word = inst_memory_ref | inst_non_memory_ref | inst_io | data | symbol

  val line=whiteSpace.? ~> address.? ~> define_label.* ~> (word ~ comment.? ^^ {
    result => cur += 1
      result match {
        case word_n ~ Some(cmt) => Line(lineNum, word_n, Comment(cmt))
        case word_n ~ None => Line(lineNum, word_n, null)
      }
  } | comment.? ^^ {
      case Some(cmt) => Line(lineNum, null, Comment(cmt))
      case None => Line(lineNum, null, null)
  }) ^^ { line => lineNum+=1; line }

  val program = rep(line.? <~ eol ^^ {
    case Some(line_n) => line_n
    case None => Line(lineNum, null, null)
  }) <~ "END" <~ eol.?

  private class FirstPassParser extends Ex3Parser{
    override val operand=label ^^ {
      labels.get(_) match {
        case Some(address_n) => address_n.toShort
        case _ => 0.toShort
      }
    }

    override val define_label=label <~ ',' ^^ {
      result => {
        if(labels.exists(_._1==result)){
          throw new Exception("the definition of '"+result+"' duplicated at " + lineNum)
        }
        labels += (result -> cur)
        result
      }
    }

    def parse1pass(src:String):mutable.HashMap[String,Int]={
      labels = mutable.HashMap.empty[String,Int]
      cur = 0
      try{
        parseAll(program,src) match {
          case Success(res,nxt) =>
            labels
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

  def parse(src:String):List[Line]={
    val firstParser = new FirstPassParser()
    labels = firstParser.parse1pass(src)
    cur = 0
    try{
      parseAll(program,src) match {
        case Success(res,nxt) =>
          res
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
  val fileNameRegex="""([/0-9a-zA-Z\._\-]+)\.asm""".r
  def main(args: Array[String]) {
    if(args.length!=1){
      System.err.println("Invalid argument (insufficient)")
      sys.exit(1)
    }
    var filename:String=null
    args(0) match {
      case fileNameRegex(fn) => filename=fn
      case _ =>
        System.err.println("Invalid argument (Not .asm)")
        sys.exit(1)
    }
    val source=Source.fromFile(args(0))
    val src = source.getLines().toList
    val parser=new Ex3Parser()
    val instructions=parser.parse(src.mkString("\n")).filter(_.hasCell).map(_.cell)

    val mem_writer=new PrintWriter(filename+".mem")
    instructions.map(_.toBinStr).foreach(mem_writer.println)
    mem_writer.close()

    val prb_writer = new PrintWriter(filename + ".prb")
    instructions.filter(_.word.isInstanceOf[Data]).map({data =>
      val address = data.address
      val binary = data.word.toBin.toInt & 0xffff
      val probe = binary | address <<16
      f"$probe%08x"
    }).foreach(prb_writer.println)
    prb_writer.println("f0000000")
    prb_writer.close()
  }
}
