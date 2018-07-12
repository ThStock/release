package release

import java.io.{BufferedReader, PrintStream}

import org.jline.reader._
import org.jline.terminal.TerminalBuilder
import release.Starter.Opts

import scala.annotation.tailrec

object Term {

  private def readLineWithPromtJline(prompt: String): String = {
    val terminal = TerminalBuilder.builder()
      .system(true)
      .dumb(true)
      .streams(System.in, System.out)
      .build()
    val reader = LineReaderBuilder.builder()
      .terminal(terminal)
      .option(LineReader.Option.INSERT_TAB, true)
      .build()

    try {
      reader.readLine(prompt)
    } catch {
      case _: EndOfFileException ⇒ {
        System.exit(28)
        "EndOfFileException"
      }
      case _: UserInterruptException ⇒ {
        System.exit(28)
        "UserInterruptException"
      }
    }
  }

  private def readLineWithPrompt(in: BufferedReader, out: PrintStream, prompt: String, opts: Opts): String = {
    if (opts.useJlineInput) {
      readLineWithPromtJline(prompt)
    } else {
      out.print(prompt)
      in.readLine()
    }
  }

  @tailrec
  def removeSnapshot(str: String): String = {
    val out = str.replaceFirst("-SNAPSHOT$", "").trim
    if (out.contains("-SNAPSHOT")) {
      removeSnapshot(out)
    } else {
      out
    }
  }

  def readFrom(out: PrintStream, text: String, defaultValue: String, opts: Opts, in: BufferedReader = Console.in): String = {
    val line = readLineWithPrompt(in, out, text + " [%s]: ".format(defaultValue), opts)
    val result = line match {
      case null ⇒ System.exit(14); null
      case "" ⇒ defaultValue
      case any: String ⇒ Option(any.trim).getOrElse(defaultValue)
    }

    result match {
      case in if in.contains("-UNDEF") ⇒ throw new IllegalArgumentException("\"-UNDEF\" is not allowed")
      case in if in.trim.isEmpty ⇒ throw new IllegalArgumentException("blank is not allowed")
      case other ⇒ other
    }
  }

  def readFromOneOfYesNo(out: PrintStream, text: String, opts: Opts, in: BufferedReader = Console.in) = readFromOneOf(out, text, Seq("y", "n"), opts, in)

  @tailrec
  def readFromOneOf(out: PrintStream, text: String, possibleValues: Seq[String], opts: Opts, in: BufferedReader = Console.in): String = {
    val line = readLineWithPrompt(in, out, text + " [%s]: ".format(possibleValues.mkString("/")), opts)
    line match {
      case null ⇒ System.exit(1); null
      case any: String if possibleValues.contains(any.trim) ⇒ any.trim
      case _: String ⇒ readFromOneOf(out, text, possibleValues, opts, in)
    }
  }

  def readFromYes(out: PrintStream, text: String, opts: Opts) = readFrom(out, text, "y", opts)

  def readChooseOneOfOrType(out: PrintStream, text: String, possibleValues: Seq[String], opts: Opts, in: BufferedReader = Console.in): String = {
    possibleValues match {
      case values if values.size == 1 ⇒ readFrom(out, text, possibleValues.head, opts, in)
      case _ ⇒ {
        out.println(text)
        val mapped: Map[String, String] = possibleValues.zip(Stream.from(1))
          .map(in ⇒ (in._2.toString, in._1)).foldLeft(Map.empty[String, String])(_ + _)
        possibleValues.zip(Stream.from(1))
          .map(in ⇒ (in._2, in._1)).foreach(p ⇒ out.println("[" + p._1 + "] " + p._2))
        val line = readLineWithPrompt(in, out, "Enter option or type [" + possibleValues.head + "]: ", opts)
        line match {
          case null ⇒ System.exit(1); null
          case "" ⇒ mapped.head._2
          case any: String if mapped.contains(any) ⇒ mapped(any)
          case other: String ⇒ other.trim
        }
      }
    }
  }
}
