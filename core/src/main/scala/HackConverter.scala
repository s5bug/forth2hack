package tf.bug.forthhack

import cats.collections.AvlSet
import fs2._
import tf.bug.asmtrans.HackInstruction

object HackConverter {

  def convert[F[_]]: Pipe[F, ForthStatement, HackInstruction] = { forthDefinitions: Stream[F, ForthStatement] =>
    forthDefinitions.zipWithIndex.flatMap {
      // Todo: handle : name stmnts ;
      case (ForthStatement.Word(wordName), i) =>
        // TODO generate a label name from source location and a [a-zA-Z0-9] word name
        val labelName = s"STMT_$i"
        Stream(
          HackInstruction.Address(labelName),
          HackInstruction.Compare(AvlSet(HackRegister.Data), HackExpression.Identity(HackRegister.Address), HackJumpType.Never),
          HackInstruction.Address("LCL"),
          HackInstruction.Compare(AvlSet(HackRegister.Memory), HackExpression.SubtractOne(HackRegister.Memory), HackJumpType.Never),
          HackInstruction.Compare(AvlSet(HackRegister.Address), HackExpression.AddOne(HackRegister.Memory), HackJumpType.Never),
          HackInstruction.Compare(AvlSet(HackRegister.Memory), HackExpression.Identity(HackRegister.Data), HackJumpType.Never),
          HackInstruction.Address(wordName),
          HackInstruction.Compare(AvlSet(), HackExpression.Zero, HackJumpType.Jump),
          HackInstruction.Label(labelName)
        )
      case (ForthStatement.Literal(value), i) =>
        val getValueIntoD = value match {
          case -1 =>
            Stream.emit(HackInstruction.Compare(AvlSet(HackRegister.Data), HackExpression.NegativeOne, HackJumpType.Never))
          case 0 =>
            Stream.emit(HackInstruction.Compare(AvlSet(HackRegister.Data), HackExpression.Zero, HackJumpType.Never))
          case 1 =>
            Stream.emit(HackInstruction.Compare(AvlSet(HackRegister.Data), HackExpression.One, HackJumpType.Never))
          case x if x >= 2 =>
            Stream(
              HackInstruction.Address(x.toString),
              HackInstruction.Compare(AvlSet(HackRegister.Data), HackExpression.Identity(HackRegister.Address), HackJumpType.Never)
            )
          case n =>
            Stream(
              HackInstruction.Address((-n).toString),
              HackInstruction.Compare(AvlSet(HackRegister.Data), HackExpression.Negate(HackRegister.Address), HackJumpType.Never)
            )
        }
        getValueIntoD ++ Stream(
          HackInstruction.Address("SP"),
          HackInstruction.Compare(AvlSet(HackRegister.Memory), HackExpression.SubtractOne(HackRegister.Memory), HackJumpType.Never),
          HackInstruction.Compare(AvlSet(HackRegister.Address), HackExpression.AddOne(HackRegister.Memory), HackJumpType.Never),
          HackInstruction.Compare(AvlSet(HackRegister.Memory), HackExpression.Identity(HackRegister.Data), HackJumpType.Never),
        )
      case (ForthStatement.StringOp(opener, contents), i) => ???
    }
  }

}
