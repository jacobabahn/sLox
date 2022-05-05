package runtimeerror

import token._

class RuntimeError(var token: Token, var message: String) extends RuntimeException {
        def main(token: Token, message: String): Unit = {
            // super.main(message)
            this.message = message
            this.token = token
        }
}

class ReturnException(var value: Object) {
    def main(value: Object): Unit = {
        this.value = value
    }
}