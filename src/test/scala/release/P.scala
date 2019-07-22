import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

class ScalaParser extends RegexParsers {
  def expression = {
    number ~ symbol ~ number ^^ { case firstOperand ~ operator ~ secondOperand =>
      validateAndExtractFirstValue(firstOperand) - validateAndExtractSecondValue(secondOperand)
    }
  }

  def symbol: Parser[Any] = "+" | "-" | "*"

  def number: Parser[Int] =
    """(0|[1-9]\d*)""".r ^^ {
      _.toInt
    }

  def validateAndExtractFirstValue(firstOperand: Any): Int = {
    val firstValue: Try[Int] = Try(firstOperand.toString.toInt)
    firstValue match {
      case util.Success(value) => value
      case util.Failure(exception) => throw new Exception("can not convert values to integer")
    }
  }

  def validateAndExtractSecondValue(secondOperand: Any): Int = {
    val secondValue = Try(secondOperand.toString.toInt)
    secondValue match {
      case util.Success(value) => value
      case util.Failure(exception) => throw new Exception("can not convert values to integer")
    }
  }
}

object TestSimpleParser extends ScalaParser {
  def main(args: Array[String]) = {

    class ExprParser extends RegexParsers {
      val number = "[\\-]?[1-9][0-9]*".r
      //def expr: Parser[Any] = number ~ opt(operator ~ expr )

      def expr: Parser[Int] = (number ^^ {
        _.toInt
      }) ~ opt(operator ~ expr) ^^ {
        case a ~ None => a
        case a ~ Some("*" ~ b) => a * b
        case a ~ Some("/" ~ b) => a / b
        case a ~ Some("+" ~ b) => a + b
        case a ~ Some("-" ~ b) => a - b
      }

      def operator: Parser[Any] = "+" | "-" | "*" | "/"
    }
    val parser = new ExprParser
    val result = parser.parseAll(parser.expr, "9 * 7 - 1")
    println(result.get)

    parseAll(expression, "FROM a") match {
      case Success(result, _) => println(result)
      case Failure(msg, _) => println("FAILURE: " + msg)
      case Error(msg, _) => println("ERROR: " + msg)
    }
  }
}
