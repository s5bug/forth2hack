package tf.bug.asmtrans

import cats.collections.AvlSet
import cats.parse.{Parser, Parser0}
import fs2.{Pipe, Stream}

object StandardParser {

  val newline: Parser[Unit] = Parser.char('\n')
  val whitespace: Parser[Unit] = Parser.charWhere(_.isWhitespace).void
  val whitespace1: Parser[Unit] = whitespace.rep.void
  val whitespace0: Parser0[Unit] = whitespace.rep0.void

  val addrKw: Parser[Unit] = Parser.string("addr").void

  val moveKw: Parser[HackJumpType] = Parser.string("move").as(HackJumpType.Never)
  val jgtzKw: Parser[HackJumpType] = Parser.string("jgtz").as(HackJumpType.GTZero)
  val jeqzKw: Parser[HackJumpType] = Parser.string("jeqz").as(HackJumpType.EQZero)
  val jltzKw: Parser[HackJumpType] = Parser.string("jltz").as(HackJumpType.LTZero)
  val jgezKw: Parser[HackJumpType] = Parser.string("jgez").as(HackJumpType.GEZero)
  val jnezKw: Parser[HackJumpType] = Parser.string("jnez").as(HackJumpType.NEZero)
  val jlezKw: Parser[HackJumpType] = Parser.string("jlez").as(HackJumpType.LEZero)
  val jumpKw: Parser[HackJumpType] = Parser.string("jump").as(HackJumpType.Jump)

  val jumpType: Parser[HackJumpType] =
    Parser.oneOf(moveKw :: jgtzKw :: jeqzKw :: jltzKw :: jgezKw :: jnezKw :: jlezKw :: jumpKw :: Nil)

  val aReg: Parser[HackRegister] = Parser.char('a').as(HackRegister.Address)
  val mReg: Parser[HackRegister] = Parser.char('m').as(HackRegister.Memory)
  val dReg: Parser[HackRegister] = Parser.char('d').as(HackRegister.Data)

  val register: Parser[HackRegister] =
    Parser.oneOf(aReg :: mReg :: dReg :: Nil)

  val destination: Parser[AvlSet[HackRegister]] =
    register.rep.flatMap { l =>
      val set = AvlSet.fromFoldable(l)
      if (set.size != l.length) {
        Parser.failWith(s"Invalid destination register(s) ${l.map(_.toStandardChar.toString).reduce}")
      } else {
        Parser.pure(set)
      }
    }

  val identifier: Parser[String] =
    Parser.charIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') :+ '-' :+ '_').rep.string

  val addrInstruction: Parser[HackInstruction] =
    (addrKw *> whitespace1 *> identifier).map(HackInstruction.Address)

  val zeroExpression: Parser[HackExpression] =
    Parser.char('0').as(HackExpression.Zero)

  val oneExpression: Parser[HackExpression] =
    Parser.char('1').as(HackExpression.One)

  val negativeOneExpression: Parser[HackExpression] =
    Parser.char('-') *> Parser.char('1').as(HackExpression.NegativeOne)

  val constantExpression: Parser[HackExpression] =
    Parser.oneOf(zeroExpression :: oneExpression :: negativeOneExpression :: Nil)

  val identityExpression: Parser[HackExpression] =
    register.map(HackExpression.Identity)

  val notExpression: Parser[HackExpression] =
    (Parser.char('~') *> register).map(HackExpression.Invert)

  val negateExpression: Parser[HackExpression] =
    (Parser.char('-') *> register).map(HackExpression.Negate)

  val addOneExpression: Parser[HackExpression] =
    (register <* (whitespace0 *> Parser.char('+') *> whitespace0 *> Parser.char('1'))).map(HackExpression.AddOne)

  val subtractOneExpression: Parser[HackExpression] =
    (register <* (whitespace0 *> Parser.char('-') *> whitespace0 *> Parser.char('1'))).map(HackExpression.SubtractOne)

  val additionExpression: Parser[HackExpression] =
    ((register <* (whitespace0 *> Parser.char('+') *> whitespace0)) ~ register).flatMap {
      case (HackRegister.Data, HackRegister.Address) | (HackRegister.Address, HackRegister.Data) =>
        Parser.pure(HackExpression.DPlusA)
      case (HackRegister.Data, HackRegister.Memory) | (HackRegister.Memory, HackRegister.Address) =>
        Parser.pure(HackExpression.DPlusM)
      case (regA, regB) =>
        Parser.failWith(s"Invalid addition operands, ${regA.toStandardChar} + ${regB.toStandardChar}")
    }

  val subtractionExpression: Parser[HackExpression] =
    ((register <* (whitespace0 *> Parser.char('-') *> whitespace0)) ~ register).flatMap {
      case (HackRegister.Data, HackRegister.Address) =>
        Parser.pure(HackExpression.DMinusA)
      case (HackRegister.Address, HackRegister.Data) =>
        Parser.pure(HackExpression.AMinusD)
      case (HackRegister.Data, HackRegister.Memory) =>
        Parser.pure(HackExpression.DMinusM)
      case (HackRegister.Memory, HackRegister.Data) =>
        Parser.pure(HackExpression.MMinusD)
      case (regA, regB) =>
        Parser.failWith(s"Invalid subtraction operands, ${regA.toStandardChar} - ${regB.toStandardChar}")
    }

  val andExpression: Parser[HackExpression] =
    ((register <* (whitespace0 *> Parser.char('&') *> whitespace0)) ~ register).flatMap {
      case (HackRegister.Data, HackRegister.Address) | (HackRegister.Address, HackRegister.Data) =>
        Parser.pure(HackExpression.DAndA)
      case (HackRegister.Data, HackRegister.Memory) | (HackRegister.Memory, HackRegister.Address) =>
        Parser.pure(HackExpression.DAndM)
      case (regA, regB) =>
        Parser.failWith(s"Invalid bitwise and operands, ${regA.toStandardChar} & ${regB.toStandardChar}")
    }

  val orExpression: Parser[HackExpression] =
    ((register <* (whitespace0 *> Parser.char('|') *> whitespace0)) ~ register).flatMap {
      case (HackRegister.Data, HackRegister.Address) | (HackRegister.Address, HackRegister.Data) =>
        Parser.pure(HackExpression.DOrA)
      case (HackRegister.Data, HackRegister.Memory) | (HackRegister.Memory, HackRegister.Address) =>
        Parser.pure(HackExpression.DOrM)
      case (regA, regB) =>
        Parser.failWith(s"Invalid bitwise or operands, ${regA.toStandardChar} | ${regB.toStandardChar}")
    }

  val compExpression: Parser[HackExpression] =
    Parser.oneOf(
      notExpression.backtrack ::
        negateExpression.backtrack ::
        addOneExpression.backtrack ::
        subtractOneExpression.backtrack ::
        additionExpression.backtrack ::
        subtractionExpression.backtrack ::
        andExpression.backtrack ::
        orExpression.backtrack ::
        identityExpression.backtrack ::
        constantExpression ::
        Nil
    )

  val nullaryCompInstruction: Parser[HackInstruction] =
    jumpType.map(hjt => HackInstruction.Compare(AvlSet.empty, HackExpression.Zero, hjt))

  val unaryCompInstruction: Parser[HackInstruction] =
    ((jumpType <* whitespace1) ~ compExpression).map {
      case (hjt, expr) => HackInstruction.Compare(AvlSet.empty, expr, hjt)
    }

  val binaryCompInstruction: Parser[HackInstruction] =
    ((jumpType <* whitespace1) ~ (destination <* whitespace0 <* Parser.char(',')) ~ (whitespace0 *> compExpression)).map {
      case ((hjt, dest), expr) => HackInstruction.Compare(dest, expr, hjt)
    }

  val compInstruction: Parser[HackInstruction] =
    Parser.oneOf(
      binaryCompInstruction.backtrack ::
        unaryCompInstruction.backtrack ::
        nullaryCompInstruction ::
        Nil
    )

  val labelInstruction: Parser[HackInstruction] =
    (identifier <* Parser.char(':')).map(HackInstruction.Label)

  val instruction: Parser[HackInstruction] =
    Parser.oneOf(
      compInstruction.backtrack ::
        addrInstruction.backtrack ::
        labelInstruction ::
        Nil
    )

  // Takes in lines and outputs instructions
  def parseLines[F[_]]: Pipe[F, String, Either[Parser.Error, HackInstruction]] = { lines: Stream[F, String] =>
    lines
      .map { s =>
        val comment = s.indexOf(';')
        if (comment != -1) s.take(comment).trim
        else s.trim
      }.filter(_.nonEmpty)
      .map(instruction.parseAll)
  }

}
