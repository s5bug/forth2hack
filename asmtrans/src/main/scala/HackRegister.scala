package tf.bug.asmtrans

import cats._

sealed trait HackRegister {
  val toHackChar: Char
  val toStandardChar: Char
}
object HackRegister {
  case object Address extends HackRegister {
    override val toHackChar: Char = 'A'
    override val toStandardChar: Char = 'a'
  }
  case object Memory extends HackRegister {
    override val toHackChar: Char = 'M'
    override val toStandardChar: Char = 'm'
  }
  case object Data extends HackRegister {
    override val toHackChar: Char = 'D'
    override val toStandardChar: Char = 'd'
  }

  implicit val hackRegisterOrder: Order[HackRegister] = Order.fromLessThan {
    case (Address, Memory) => true
    case (Address, Data) => true
    case (Memory, Data) => true
    case (_, _) => false
  }
}
