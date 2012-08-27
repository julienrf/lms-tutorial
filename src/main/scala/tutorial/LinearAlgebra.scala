package tutorial

import scala.virtualization.lms.common._

// Concepts and concrete syntax
trait LinearAlgebra extends Base {

  // Concepts
  type Vector
  def vector_scale(v: Rep[Vector], k: Rep[Double]): Rep[Vector]

  // Concrete syntax
  final def infix_*(v: Rep[Vector], k: Rep[Double]): Rep[Vector] = vector_scale(v, k)
}

// Interpreter
trait Interpreter extends Base {
  override type Rep[+A] = A
  override def unit[A : Manifest](a: A) = a
}

trait LinearAlgebraInterpreter extends LinearAlgebra { this: Interpreter =>
  override type Vector = Seq[Double]
  override def vector_scale(v: Seq[Double], k: Double) = v map (_ * k)
}

// Intermediate representation
trait LinearAlgebraExp extends LinearAlgebra with BaseExp {

  case class VectorScale(v: Exp[Vector], k: Exp[Double]) extends Def[Vector]

  override def vector_scale(v: Exp[Vector], k: Exp[Double]) = VectorScale(v, k)

  override type Vector = Seq[Double]
}

// Optimizations working on the intermediate representation
trait LinearAlgebraOpt extends LinearAlgebraExp {

  override def vector_scale(v: Exp[Vector], k: Exp[Double]) = k match {
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
trait Prog { this: LinearAlgebra =>

  def f(v: Rep[Vector]): Rep[Vector] = v * unit(42.0)

  def g(v: Rep[Vector]): Rep[Vector] = v * unit(1.0)

}

object Usage extends App {

  val concreteProg = new Prog with EffectExp with LinearAlgebraExp with LinearAlgebraOpt with CompileScala { self =>
    override val codegen = new ScalaGenEffect with ScalaGenLinearAlgebra { val IR: self.type = self }
    codegen.emitSource(f, "F", new java.io.PrintWriter(System.out))
    codegen.emitSource(g, "G", new java.io.PrintWriter(System.out))
  }
  val f = concreteProg.compile(concreteProg.f)
  println(f(Seq(1.0, 2.0)))

  val interpretedProg = new Prog with Interpreter with LinearAlgebraInterpreter
  println(interpretedProg.f(Seq(1.0, 2.0)))

}