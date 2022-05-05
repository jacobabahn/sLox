package loxcallable
import interpreter._

trait LoxCallable {
    def arity(): Int
    def call(interpreter: Interpreter, arguments: Array[Object]): Object
}