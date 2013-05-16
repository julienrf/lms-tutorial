package tutorial

import scala.virtualization.lms.common.{ScalaGenEffect, CompileScala, EffectExp}
import java.io.PrintWriter

// Usage
trait Prog extends LinearAlgebra {

  def f(v: Rep[Vector]): Rep[Vector] = v * unit(42.0)

  def g(v: Rep[Vector]): Rep[Vector] = v * unit(1.0)

}


trait LogProg extends LinearAlgebra with Log {

  def double(v: Rep[Vector]): Rep[Vector] = {
    log(unit("hello"))
    v * unit(2.0)
  }

}


object Main extends App {

  val concreteProg = new Prog with LinearAlgebraOpt with EffectExp with CompileScala { self =>
    override val codegen = new ScalaGenEffect with ScalaGenLinearAlgebra { val IR: self.type = self }
    codegen.emitSource(f, "F", new java.io.PrintWriter(System.out))
    codegen.emitSource(g, "G", new java.io.PrintWriter(System.out))
  }
  val f = concreteProg.compile(concreteProg.f)
  println(f(Seq(1.0, 2.0)))

  val interpretedProg = new Prog with LinearAlgebraInterpreter
  println(interpretedProg.f(Seq(1.0, 2.0)))


  val logProg = new LogProg with LinearAlgebraExp with LogExp
  val codegen = new ScalaGenLinearAlgebra with ScalaGenLog { val IR: logProg.type = logProg }
  codegen.emitSource(logProg.double, "Double", new PrintWriter(System.out))

}