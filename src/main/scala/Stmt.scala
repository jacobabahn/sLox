package stmt
import token.Token
import expr._

trait sVisitor[R] {
    def visitExpressionStmt(stmt: Expression): R
    def visitPrintStmt(stmt: Print): R
    def visitVarStmt(stmt: Var): R
}

abstract class Stmt {
    def accept[R](visitor: sVisitor[R]): R
}

class Expression(var expression: Expr) extends Stmt {
    override def accept[R](visitor: sVisitor[R]): R = return visitor.visitExpressionStmt(this)
}

class Print(var expression: Expr) extends Stmt {
    override def accept[R](visitor: sVisitor[R]): R = return visitor.visitPrintStmt(this)
}

class Var(var name: Token, var initializer: Expr) extends Stmt {
    override def accept[R](visitor: sVisitor[R]): R = return visitor.visitVarStmt(this)
}