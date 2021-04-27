package tf.bug.forthhack

import cats.collections.AvlSet
import cats.effect.{ExitCode, IO}
import cats.syntax.all._
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import fs2._
import tf.bug.asmtrans.{HackExpression, HackInstruction, HackJumpType, HackRegister}

import java.nio.file.{Path, StandardOpenOption}

object Main extends CommandIOApp(
  name = "forth2hack",
  header = "Forth to Hack ASM Compiler",
  version = "0.1.0",
) {

  override def main: Opts[IO[ExitCode]] = {
    val input = Opts.argument[Path]("input.fs")
    val output = Opts.argument[Path]("output.asm")
    val libpath = Opts.option[Path]("stdlib", "Path to Standard Library files", "L", "/path/to/lib").orNone
    val betterAsmSyntax = Opts.flag("standard-asm", "Use a better ASM syntax", "S").orNone.map(_.isDefined)

    (input, output, libpath, betterAsmSyntax).mapN((ip, op, l, basm) => program[IO](ip, op, l, basm).compile.drain.as(ExitCode.Success))
  }

  def program[F[_]](in: Path, out: Path, lib: Option[Path], betterAsm: Boolean)(implicit files: io.file.Files[F]): Stream[F, Nothing] = {
    val inputInstructions =
      ForthNormalizer.normalize[F](in, lib)
        .through(HackConverter.convert[F])

    val runtimeInstructions = forthRuntime[F]

    (runtimeInstructions ++ inputInstructions).map { instr =>
        if(betterAsm) instr.toStandardAsm + "\n"
        else instr.toHackAsm + "\n"
      }
      .through(text.utf8Encode[F])
      .through(files.writeAll(out, List(StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)))
  }

  def forthRuntime[F[_]]: Stream[F, HackInstruction] =
    Stream(
      HackInstruction.Address("16383"),
      HackInstruction.Compare(AvlSet(HackRegister.Data), HackExpression.Identity(HackRegister.Address), HackJumpType.Never),
      HackInstruction.Address("SP"),
      HackInstruction.Compare(AvlSet(HackRegister.Memory), HackExpression.Identity(HackRegister.Data), HackJumpType.Never),
      HackInstruction.Address("2047"),
      HackInstruction.Compare(AvlSet(HackRegister.Data), HackExpression.Identity(HackRegister.Address), HackJumpType.Never),
      HackInstruction.Address("LCL"),
      HackInstruction.Compare(AvlSet(HackRegister.Memory), HackExpression.Identity(HackRegister.Data), HackJumpType.Never),
      HackInstruction.Address("_ENTRY"),
      HackInstruction.Compare(AvlSet.empty, HackExpression.Zero, HackJumpType.Jump)
    )

}
