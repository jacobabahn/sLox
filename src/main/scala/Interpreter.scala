package interpreter

import expr._
import stmt._
import lox.Lox
import tokentype._
import token._
import environment._
import runtimeerror._
import math.Fractional.Implicits.infixFractionalOps
import math.Integral.Implicits.infixIntegralOps
import math.Numeric.Implicits.infixNumericOps


class Interpreter() extends Visitor[Object], sVisitor[Unit] {
    private var environment: Environment = new Environment()

    def interpret(statements: Array[Stmt]) = {
        try {
            for (statement <- statements) {
                execute(statement)
            }
        } catch {
            case e: RuntimeError => {
                val lox = new Lox()
                lox.runtimeError(e)
            }
        }
    }

    override def visitLiteralExpr(expr: Literal): Object = {
        var value = expr.value
        return value.asInstanceOf[Object]
    }

    override def visitLogicalExpr(expr: Logical): Object = {
        val left = evaluate(expr.left)

        if (expr.operator.toktype == TokenType.OR) {
            if (isTruthy(left)) {
                return left
            }
        } else {
            if (!isTruthy(left)) {
                return left
            }
        }
        return evaluate(expr.right)
    }

    override def visitUnaryExpr(expr: Unary): Object = {
        var right = evaluate(expr.right)

        val op = expr.operator.toktype
        op match {
            case TokenType.BANG => return (!isTruthy(right)).asInstanceOf[Object]
            case TokenType.MINUS => {
                checkNumberOperand(expr.operator, right)
                return (-((right).asInstanceOf[Double])).asInstanceOf[Object]
            }
        }

        return null
    }

    override def visitVariableExpr(expr: Variable): Object = {
        return environment.get(expr.name)
    }

    private def checkNumberOperand(operator: Token, operand: Object): Unit = {
        if (operand.isInstanceOf[Double]) return

        throw new RuntimeError(operator, "Operand must be a number.")
    }

    private def checkNumberOperands(operator: Token, left: Any, right: Any): Unit = {
        if (left.isInstanceOf[Double] && right.isInstanceOf[Double]) return

        throw new RuntimeError(operator, "Operands must be numbers.")
    }

    private def isTruthy(obj: Object): Boolean = {
        if (obj == null) return false
        if (obj.isInstanceOf[Boolean]) return obj.asInstanceOf[Boolean]
        return true
    }

    private def isEqual(a: Object, b: Object): Boolean = {
        if (a == null && b == null) return true
        if (a == null) return false

        return a == b
    }

    private def stringify(obj: Object): String = {
        if (obj == null) return "nil"

        if (obj.isInstanceOf[Double]) then
            var text = obj.toString
            if (text.endsWith(".0")) then
                text = text.substring(0, text.length() - 2)
            return text
        
        return obj.toString
    }

    override def visitGroupingExpr(expr: Grouping): Object = {
        return evaluate(expr.expression)
    }

    private def evaluate(expr: Expr): Object = {
        return expr.accept(this)
    }

    private def execute(stmt: Stmt): Unit = {
        stmt.accept(this)
    }

    def executeBlock(statements: Array[Stmt], environment: Environment): Unit = {
        val previous = this.environment
        try {
            this.environment = environment
            for (statement <- statements) {
                execute(statement)
            }
        } finally {
            this.environment = previous
        }
    }

    override def visitBlockStmt(stmt: Block): Unit = {
        executeBlock(stmt.statements, new Environment(environment))
        return null
    }

    override def visitExpressionStmt(stmt: Expression): Unit = {
        evaluate(stmt.expression)
        return null
    }

    override def visitIfStmt(stmt: If): Unit = {
        if (isTruthy(evaluate(stmt.condition))) {
            execute(stmt.thenBranch)
        } else if (stmt.elseBranch != null) {
            execute(stmt.elseBranch)
        }
        return null
    }

    override def visitPrintStmt(stmt: Print): Unit = {
        var value = evaluate(stmt.expression)
        println(stringify(value))
        return null
    }

    override def visitVarStmt(stmt: Var): Unit = {
        var value: Object = new Literal(null)
        if (stmt.initializer != Literal(null)) then value = evaluate(stmt.initializer) else value = Literal(null)

        environment.define(stmt.name.lexeme, value)
        return null
    }

    override def visitWhileStmt(stmt: While): Unit = {
        while (isTruthy(evaluate(stmt.condition))) {
            execute(stmt.body)
        }
        return null
    }

    override def visitAssignExpr(expr: Assign): Object = {
        var value = evaluate(expr.value)
        environment.assign(expr.name, value)
        return value
    }

    override def visitBinaryExpr(expr: Binary): AnyRef = {
        val left = evaluate(expr.left)
        val right = evaluate(expr.right)

        val op = expr.operator.toktype
        op match {
            case TokenType.GREATER => {
                checkNumberOperands(expr.operator, left, right)
                return (left.asInstanceOf[Double] > right.asInstanceOf[Double]).asInstanceOf[Object]
            }
            case TokenType.GREATER_EQUAL => {
                checkNumberOperands(expr.operator, left, right)
                return (left.asInstanceOf[Double] >= right.asInstanceOf[Double]).asInstanceOf[Object]
            }
            case TokenType.LESS => {
                checkNumberOperands(expr.operator, left, right)
                return (left.asInstanceOf[Double] < right.asInstanceOf[Double]).asInstanceOf[Object]
            }
            case TokenType.LESS_EQUAL => {
                checkNumberOperands(expr.operator, left, right)
                return (left.asInstanceOf[Double] <= right.asInstanceOf[Double]).asInstanceOf[Object]
            }
            case TokenType.MINUS => {
                checkNumberOperands(expr.operator, left, right)
                return (left.asInstanceOf[Double] - right.asInstanceOf[Double]).asInstanceOf[Object]
            }
            case TokenType.PLUS => {
                if (left.isInstanceOf[Double] || right.isInstanceOf[Double]) then {
                    return (left.asInstanceOf[Double] + right.asInstanceOf[Double]).asInstanceOf[Object]
                } else if (left.isInstanceOf[String] || right.isInstanceOf[String]) then {
                    return (left.asInstanceOf[String] + right.asInstanceOf[String]).asInstanceOf[Object]
                }

                throw new RuntimeError(expr.operator, "Operands must be two numbers or two strings.")
            }
            case TokenType.SLASH => {
                checkNumberOperands(expr.operator, left, right)
                return (left.asInstanceOf[Double] / right.asInstanceOf[Double]).asInstanceOf[Object]
            }
            case TokenType.STAR => {
                checkNumberOperands(expr.operator, left, right)
                return (left.asInstanceOf[Double] * right.asInstanceOf[Double]).asInstanceOf[Object]
            }
            case TokenType.BANG_EQUAL => return (!isEqual(left, right)).asInstanceOf[Object]
            case TokenType.EQUAL_EQUAL => return isEqual(left, right).asInstanceOf[Object]
        }
    }
}