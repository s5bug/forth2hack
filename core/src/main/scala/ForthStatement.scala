package tf.bug.forthhack

sealed trait ForthStatement
object ForthStatement {
  case class Word(name: String) extends ForthStatement
  case class Literal(value: Short) extends ForthStatement
  case class Definition(name: String, body: Vector[ForthStatement]) extends ForthStatement
  case class Include(path: String) extends ForthStatement
  case class StringOp(opener: String, contents: String) extends ForthStatement
}
