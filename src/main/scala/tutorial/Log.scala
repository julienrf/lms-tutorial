package tutorial

import scala.virtualization.lms.common.{EffectExp, ScalaGenEffect, Base}

// Logging DSL
trait Log extends Base {

  // Print a message in a log
  def log(message: Rep[String]): Rep[Unit]

}

trait LogExp extends Log with EffectExp {

  case class Log(message: Exp[String]) extends Def[Unit]

  def log(message: Exp[String]) = reflectEffect(Log(message))

}


trait ScalaGenLog extends ScalaGenEffect {
  val IR: LogExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Log(message) =>
      stream.println(s"println(${quote(message)})")
    case _ => super.emitNode(sym, rhs)
  }

}