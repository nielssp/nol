package dk.nielssp.nol

class TypeClass(name: String = "", parameters: Int = 1) {
  override def toString = name

  def toFunction: Value = {
    def convert(n: Int, parameters: List[Value]): Value =
    // TODO: handle non-monotype parameters
      if (n < 1) Constraint(this, parameters.reverse.map(_.asInstanceOf[Monotype]): _*)
      else LambdaValue {
        case parameter => convert(n - 1, parameter :: parameters)
      }
    convert(parameters, List.empty)
  }
}
