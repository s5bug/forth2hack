package tf.bug.forthhack

import fs2._

import java.nio.file.Path

object ForthNormalizer {

  case class Token(value: String, line: Long, column: Int)

  def normalize[F[_]](in: Path, lib: Option[Path])(implicit files: io.file.Files[F]): Stream[F, ForthStatement] = {
    files.readAll(in, 4096)
      .through(text.utf8Decode[F])
      .through(text.lines[F])
      .zipWithIndex
      .flatMap {
        case (line, lineNum) if line.trim.nonEmpty =>
          Stream.emits[F, Token](getTokens(line, lineNum))
        case _ => Stream.empty
      }.mapAccumulate(TokenizationState.empty) {
        case (tokenizationState, token) =>
          tokenizationState.step(token)
      }.collect {
        case (_, Some(stmnt)) =>
          stmnt
      }
  }

  def getTokens(str: String, l: Long): Vector[Token] = ???

  case class TokenizationState() {
    def step(token: Token): (TokenizationState, Option[ForthStatement]) = {
      ???
    }
  }
  object TokenizationState {
    val empty: TokenizationState = TokenizationState()
  }

}
