package tf.bug.asmtrans

sealed trait HackJumpType {
  val toHackString: String
  val toStandardString: String
}
object HackJumpType {
  case object Never extends HackJumpType {
    override val toHackString: String = "null"
    override val toStandardString: String = "move"
  }
  case object GTZero extends HackJumpType {
    override val toHackString: String = "JGT"
    override val toStandardString: String = "jgtz"
  }
  case object EQZero extends HackJumpType {
    override val toHackString: String = "JEQ"
    override val toStandardString: String = "jeqz"
  }
  case object LTZero extends HackJumpType {
    override val toHackString: String = "JLT"
    override val toStandardString: String = "jltz"
  }
  case object GEZero extends HackJumpType {
    override val toHackString: String = "JGE"
    override val toStandardString: String = "jgez"
  }
  case object NEZero extends HackJumpType {
    override val toHackString: String = "JNE"
    override val toStandardString: String = "jnez"
  }
  case object LEZero extends HackJumpType {
    override val toHackString: String = "JLE"
    override val toStandardString: String = "jlez"
  }
  case object Jump extends HackJumpType {
    override val toHackString: String = "JMP"
    override val toStandardString: String = "jump"
  }
}
