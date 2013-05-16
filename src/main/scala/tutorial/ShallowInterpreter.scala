package tutorial

import scala.virtualization.lms.common.Base

// Shallow interpreter: does not build an abstract intermediate representation of expressions
trait ShallowInterpreter extends Base {

  // Use a value of type A to represent a value of type A
  override type Rep[+A] = A

  // Lifting a constant to its representation is the identity function
  override def unit[A : Manifest](a: A) = a

}
