package environment
import runtimeerror._
import token._

class Environment(var enclosing: Any = null) {
    var values: Map[String, Object] = Map()
    var constMap: Map[String, Boolean] = Map()

    def main(enclosing: Any) = {
        this.enclosing = enclosing
    }

    def get(name: Token): Object = {
        if (values.contains(name.lexeme)) then
            return values(name.lexeme)

        if (enclosing != null) then
            return enclosing.asInstanceOf[Environment].get(name)
        // double check above
        throw new RuntimeError(name, "Undefined variable '" + name.lexeme + "'.")
    }

    def assign(name: Token, value: Object): Unit = {
        if (constMap.contains(name.lexeme)) then
            throw new RuntimeError(name, "Cannot reassign const variable '" + name.lexeme + "'.")
        if (values.contains(name.lexeme)) then
            values += (name.lexeme -> value)
        else if (enclosing != null) then
            enclosing.asInstanceOf[Environment].assign(name, value)
        else
            throw new RuntimeError(name, "Undefined variable '" + name.lexeme + "'.")
    }

    def define(name: String, value: Object, isConst: Boolean = false): Unit = {
        if (constMap.contains(name)) then
            throw new RuntimeError(name, "Cannot redefine const variable '" + name + "'.")

        values += (name -> value)
        
        if (isConst) then
            constMap += (name -> true)
    }
}