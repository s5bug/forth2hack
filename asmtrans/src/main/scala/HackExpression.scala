package tf.bug.asmtrans

sealed trait HackExpression {
  val toHackString: String
  val toStandardString: String
}

object HackExpression {
  case object Zero extends HackExpression {
    override val toHackString: String = "0"
    override val toStandardString: String = "0"
  }
  case object One extends HackExpression {
    override val toHackString: String = "1"
    override val toStandardString: String = "1"
  }
  case object NegativeOne extends HackExpression {
    override val toHackString: String = "-1"
    override val toStandardString: String = "-1"
  }
  case class Identity(register: HackRegister) extends HackExpression {
    override val toHackString: String = register.toHackChar.toString
    override val toStandardString: String = register.toStandardChar.toString
  }
  case class Invert(register: HackRegister) extends HackExpression {
    override val toHackString: String = s"!${register.toHackChar}"
    override val toStandardString: String = s"~${register.toStandardChar}"
  }
  case class Negate(register: HackRegister) extends HackExpression {
    override val toHackString: String = s"-${register.toHackChar}"
    override val toStandardString: String = s"-${register.toStandardChar}"
  }
  case class AddOne(register: HackRegister) extends HackExpression {
    override val toHackString: String = s"${register.toHackChar}+1"
    override val toStandardString: String = s"${register.toStandardChar}+1"
  }
  case class SubtractOne(register: HackRegister) extends HackExpression {
    override val toHackString: String = s"${register.toHackChar}-1"
    override val toStandardString: String = s"${register.toStandardChar}-1"
  }
  case object DPlusA extends HackExpression {
    override val toHackString: String = s"${HackRegister.Data.toHackChar}+${HackRegister.Address.toHackChar}"
    override val toStandardString: String = s"${HackRegister.Data.toStandardChar}+${HackRegister.Address.toStandardChar}"
  }
  case object DPlusM extends HackExpression {
    override val toHackString: String = s"${HackRegister.Data.toHackChar}+${HackRegister.Memory.toHackChar}"
    override val toStandardString: String = s"${HackRegister.Data.toStandardChar}+${HackRegister.Memory.toStandardChar}"
  }
  case object DMinusA extends HackExpression {
    override val toHackString: String = s"${HackRegister.Data.toHackChar}-${HackRegister.Address.toHackChar}"
    override val toStandardString: String = s"${HackRegister.Data.toStandardChar}-${HackRegister.Address.toStandardChar}"
  }
  case object DMinusM extends HackExpression {
    override val toHackString: String = s"${HackRegister.Data.toHackChar}-${HackRegister.Memory.toHackChar}"
    override val toStandardString: String = s"${HackRegister.Data.toStandardChar}-${HackRegister.Memory.toStandardChar}"
  }
  case object AMinusD extends HackExpression {
    override val toHackString: String = s"${HackRegister.Address.toHackChar}-${HackRegister.Data.toHackChar}"
    override val toStandardString: String = s"${HackRegister.Address.toStandardChar}-${HackRegister.Data.toStandardChar}"
  }
  case object MMinusD extends HackExpression {
    override val toHackString: String = s"${HackRegister.Memory.toHackChar}-${HackRegister.Data.toHackChar}"
    override val toStandardString: String = s"${HackRegister.Memory.toStandardChar}-${HackRegister.Data.toStandardChar}"
  }
  case object DAndA extends HackExpression {
    override val toHackString: String = s"${HackRegister.Data.toHackChar}&${HackRegister.Address.toHackChar}"
    override val toStandardString: String = s"${HackRegister.Data.toStandardChar}&${HackRegister.Address.toStandardChar}"
  }
  case object DAndM extends HackExpression {
    override val toHackString: String = s"${HackRegister.Data.toHackChar}&${HackRegister.Memory.toHackChar}"
    override val toStandardString: String = s"${HackRegister.Data.toStandardChar}&${HackRegister.Memory.toStandardChar}"
  }
  case object DOrA extends HackExpression {
    override val toHackString: String = s"${HackRegister.Data.toHackChar}|${HackRegister.Address.toHackChar}"
    override val toStandardString: String = s"${HackRegister.Data.toStandardChar}|${HackRegister.Address.toStandardChar}"
  }
  case object DOrM extends HackExpression {
    override val toHackString: String = s"${HackRegister.Data.toHackChar}|${HackRegister.Memory.toHackChar}"
    override val toStandardString: String = s"${HackRegister.Data.toStandardChar}|${HackRegister.Memory.toStandardChar}"
  }
}
