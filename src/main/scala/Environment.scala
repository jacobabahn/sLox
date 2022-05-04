package environment
import runtimeerror._
import token._

class Environment {
    var values: Map[String, Object] = Map()

    def get(name: Token): Object = {
        if (values.contains(name.lexeme)) then
            return values(name.lexeme)

        throw new RuntimeError(name, "Undefined variable '" + name.lexeme + "'.")
    }

    def define(name: String, value: Object): Unit = {
        values += (name -> value)
    }
}