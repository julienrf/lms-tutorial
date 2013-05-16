package tutorial

import scala.virtualization.lms.common._

// Concepts and concrete syntax
trait LinearAlgebra extends Base {

  // Concepts
  type Vector
  def vector_scale(v: Rep[Vector], k: Rep[Double]): Rep[Vector]

  // Concrete syntax
  implicit class VectorOps(v: Rep[Vector]) {
    def * (k: Rep[Double]): Rep[Vector] = vector_scale(v, k)
  }
}


trait LinearAlgebraInterpreter extends LinearAlgebra with ShallowInterpreter {

  override type Vector = Seq[Double]

  override def vector_scale(v: Seq[Double], k: Double) = v map (_ * k)

}

// Deep embedding: use an abstract intermediate representation to model concepts of the DSL
trait LinearAlgebraExp extends LinearAlgebra with BaseExp {

  // Reification of the vector scaling operation into a intermediate representation node
  case class VectorScale(v: Exp[Vector], k: Exp[Double]) extends Def[Vector]

  // The implementation just returns the corresponding intermediate representation node
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

