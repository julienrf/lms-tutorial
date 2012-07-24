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

trait LinearAlgebraInterpreter extends LinearAlgebra with Interpreter {
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
trait Prog extends LinearAlgebra {

  def f(v: Rep[Vector]): Rep[Vector] = v * unit(42.0)

  def g(v: Rep[Vector]): Rep[Vector] = v * unit(1.0)

}

object Usage extends App {

  object ConcreteProg extends Prog with EffectExp with LinearAlgebraExp with LinearAlgebraOpt with CompileScala { self =>
    override val codegen = new ScalaGenEffect with ScalaGenLinearAlgebra { val IR: self.type = self }
    lazy val compiledF = {
      codegen.emitSource(f, "f", new java.io.PrintWriter(System.out))
      codegen.emitSource(g, "g", new java.io.PrintWriter(System.out))
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