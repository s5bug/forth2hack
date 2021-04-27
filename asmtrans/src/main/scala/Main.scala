package tf.bug.asmtrans

import cats.effect.{ExitCode, IO}
import cats.parse.Parser
import cats.syntax.all._
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import fs2._

import java.nio.file.{Path, StandardOpenOption}

object Main extends CommandIOApp(
  name = "hacktrans",
  header = "Translates a more standard ASM syntax into Hack ASM",
  version = "0.1.0",
) {
  override def main: Opts[IO[ExitCode]] = {
    val inputArgument = Opts.argument[Path]("input.asm").orNone
    val outputOption = Opts.option[Path]("output", "Output file to write to instead of stdout", "o", "output.asm").orNone

    (inputArgument, outputOption).mapN {
      case (in, out) =>
        val inStream = in match {
          case Some(p) => io.file.Files[IO].readAll(p, 4096)
          case None => io.stdin[IO](4096)
        }
        val outPipe = out match {
          case Some(p) => io.file.Files[IO].writeAll(p, List(StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING))
          case None => io.stdout[IO]
        }
        val stream = inStream
          .through(text.utf8Decode)
          .through(text.lines[IO])
          .through(StandardParser.parseLines[IO])
          .flatMap {
            case Left(err) => Stream.raiseError[IO](TranslationException(err))
            case Right(inst) => Stream[IO, String](inst.toHackAsm, "\n")
          }.through(text.utf8Encode[IO])
          .through(outPipe)
        stream.compile.drain.as(ExitCode.Success)
    }
  }
}

case class TranslationException(internal: Parser.Error) extends Exception {
  override def getMessage: String = {
    internal.toString
  }
}
