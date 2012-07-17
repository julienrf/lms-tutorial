package tutorial

import scala.Tuple2.apply
import scala.virtualization.lms.common.Base
import scala.virtualization.lms.common.BaseExp
import scala.virtualization.lms.common.CompileScala
import scala.virtualization.lms.common.EffectExp
import scala.virtualization.lms.common.LiftNumeric
import scala.virtualization.lms.common.ScalaGenBase
import scala.virtualization.lms.common.ScalaGenEffect
import tutorial.Usage.{ConcreteProg => self}

// Concepts and concrete syntax
trait LinearAlgebra extends Base {

  // Concepts
  type Vector
  def vector_scale(v1: Rep[Vector], k: Rep[Double]): Rep[Vector]

  // Concrete syntax
  def infix_*(v1: Rep[Vector], k: Rep[Double]): Rep[Vector] = vector_scale(v1, k)

  implicit def vectorManifest: Manifest[Vector]
}

// Interpreter
trait LinearAlgebraInterpreter extends LinearAlgebra {

  override type Vector = Seq[Double]
  override type Rep[+A] = A

  override def vector_scale(v: Seq[Double], k: Double): Seq[Double] = v map (_ * k)

  override def unit[T : Manifest](x: T): Rep[T] = x
  override def vectorManifest = manifest[Vector]
}

// Intermediate representation
trait LinearAlgebraExp extends LinearAlgebra with BaseExp {

  override type Vector = Seq[Double]

  case class VectorScale(v1: Exp[Vector], k: Exp[Double]) extends Def[Vector]

  override def vector_scale(v1: Exp[Vector], k: Exp[Double]): Exp[Vector] = VectorScale(v1, k)

  override def vectorManifest = manifest[Vector]
}

// Optimizations working on the intermediate representation
trait LinearAlgebraOpt extends LinearAlgebraExp {

  override def vector_scale(v: Exp[Vector], k: Exp[Double]): Exp[Vector] = k match {
    case Const(1.0) => v
    case _ => super.vector_scale(v, k)
  }

}

// Scala code generator
trait ScalaGenLinearAlgebra extends ScalaGenBase {
  val IR: LinearAlgebraExp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {
    case VectorScale(v, k) => {
      // stream.println("val " + quote(sym) + " = …")
      emitValDef(sym, quote(v) + ".map(x => x * " + quote(k) + ")")
    }
    case _ => super.emitNode(sym, node)
  }

}


// Usage
trait Prog extends LinearAlgebra with LiftNumeric {

  def f(v: Rep[Vector]): Rep[Vector] = v * 42.0

}

object Usage extends App {

  object ConcreteProg extends Prog with EffectExp with LinearAlgebraExp with LinearAlgebraOpt with CompileScala { self =>
    override val codegen = new ScalaGenEffect with ScalaGenLinearAlgebra { val IR: self.type = self }
    lazy val compiledF = {
      println(codegen.emitSource(f, "f", new java.io.PrintWriter(System.out)))
      compile(f)
    }
  }

  val t = System.currentTimeMillis
  println(ConcreteProg.compiledF(Seq(1.0, 2.0)))
  println(System.currentTimeMillis - t)

  object InterpretedProg extends Prog with LinearAlgebraInterpreter

  val t2 = System.currentTimeMillis
  println(InterpretedProg.f(Seq(1.0, 2.0)))
  println(System.currentTimeMillis - t2)

}