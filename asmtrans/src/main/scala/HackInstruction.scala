package tf.bug.asmtrans

import cats.collections.AvlSet

sealed trait HackInstruction {
  def toHackAsm: String
  def toStandardAsm: String
}

case object HackInstruction {
  case class Address(label: String) extends HackInstruction {
    override def toHackAsm: String = s"@$label"
    override def toStandardAsm: String = s"addr $label"
  }
  case class Compare(destination: AvlSet[HackRegister], expression: HackExpression, jumpType: HackJumpType) extends HackInstruction {
    override def toHackAsm: String = {
      val destStr =
        if(destination.isEmpty) ""
        else destination.foldLeft("") { case (acc, reg) => acc + reg.toHackChar } + "="
      val exprStr =
        expression.toHackString
      val jumpStr =
        if(jumpType == HackJumpType.Never) ""
        else ";" + jumpType.toHackString

      destStr + exprStr + jumpStr
    }

    override def toStandardAsm: String = {
      val jumpStr =
        jumpType.toStandardString
      if(destination.isEmpty) {
        if(expression == HackExpression.Zero) {
          jumpStr
        } else {
          s"$jumpStr ${expression.toStandardString}"
        }
      } else {
        val destStr =
          destination.foldLeft("") { case (acc, reg) => acc + reg.toStandardChar }
        s"$jumpStr $destStr, ${expression.toStandardString}"
      }
    }
  }
  case class Label(name: String) extends HackInstruction {
    override def toHackAsm: String = s"($name)"

    override def toStandardAsm: String = s"$name:"
  }
}
