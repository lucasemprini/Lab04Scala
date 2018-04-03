package u09lab



sealed trait ExprNode
case class Num(value: Double) extends ExprNode
case class Sum(x: ExprNode, y: ExprNode) extends ExprNode
case class Diff(x: ExprNode, y: ExprNode) extends ExprNode
case class Mul(x: ExprNode, y: ExprNode) extends ExprNode
case class Div(x: ExprNode, y: ExprNode) extends ExprNode
case class Exp(x: ExprNode, exp: ExprNode) extends ExprNode
case object Prev extends ExprNode

trait Parser {
  def parse(s: String): ExprNode
}

object RPNParser extends Parser {
  private var stack: List[ExprNode] = List.empty[ExprNode]

  override def parse(s: String): ExprNode = {
    s.split("\\s+").foreach(token =>
      if (isOperand(token)) push(createExprNode(token))
      else if (isOperator(token)) {
        push(createExprNode(token))
      } else throw new Exception(s"Error while parsing: '$token'.")
    )
    pop
  }

  def createExprNode(token: String) = token match {
    case "+" => val (b,a) = (pop, pop); Sum(a, b)
    case "-" => val (b,a) = (pop, pop); Diff(a, b)
    case "*" => val (b,a) = (pop, pop); Mul(a, b)
    case "/" => val (b,a) = (pop, pop); Div(a, b)
    case "^" => val (b,a) = (pop, pop); Exp(a, b)
    case "a" => Prev
    case _ if token matches("(0|[1-9][0-9]*)(\\.[0-9]+)?") => Num(token.toDouble)
  }

  def isOperand(token: String): Boolean = token.matches("([0-9]+|a)")
  def isOperator(token: String): Boolean = Set("+","-","*","/","^").contains(token)

  def push(s: ExprNode): List[ExprNode] = { stack = s :: stack; stack }
  def pop: ExprNode = { val head = stack.head; stack = stack.tail; head }
}

class Calculator(parser: Parser, logger: Logger) {
  private var prev: Double = 0.0

  def enter(s: String): Double = {
    logger.log("Enter: " + s)
    val value = calculate(parser.parse(s))
    logger.log("Result: " + value)
    prev = value
    value
  }

  def calculate(expr: ExprNode): Double = expr match {
    case Num(value) => value
    case Sum(x,y) => calculate(x) + calculate(y)
    case Diff(x,y) => calculate(x) - calculate(y)
    case Mul(x,y) => calculate(x) * calculate(y)
    case Div(x,y) => calculate(x) / calculate(y)
    case Exp(x,e) => Math.pow(calculate(x), calculate(e))
    case Prev => this.prev
  }
}

object TryCalculator extends App {
  val calc = new Calculator(RPNParser, new Logger {
    override def log(msg: String) = println(msg)
  })

  calc.enter("a 2 +") // 2.0
  calc.enter("1 a + 4 *") // 12.0
  calc.enter("a") // 12.0
  calc.enter("2 3 ^") // 8.0
  calc.enter("3 4 2 * 1 5 - 2 3 ^ ^ / +") // 3.0001220703125
}
