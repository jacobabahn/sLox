package loxfunction
import loxcallable._
import stmt._
import interpreter._
import environment._
import runtimeerror._

class LoxFunction(var declaration: Funct) extends LoxCallable {
    def main(declaration: Funct) = {
        this.declaration = declaration
    }

    override def call(interpreter: Interpreter, arguments: Array[Object]) = {
        var environment = new Environment(interpreter.environment)
        for (i <- 0 until declaration.params.length) {
            environment.define(declaration.params(i).lexeme, arguments(i))
        }

        try {
            interpreter.executeBlock(declaration.body, environment)
        } catch {
            // Return
            case e: Any => print("Help") // return e.value
        }
        return null
    }

    override def arity(): Int = {
        return declaration.params.length
    }

    override def toString(): String = {
        return "<fn " + declaration.name.lexeme + ">"
    }
}