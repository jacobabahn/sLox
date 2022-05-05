package stmt
import token.Token
import expr._

trait sVisitor[R] {
    def visitBlockStmt(stmt: Block): R
    def visitExpressionStmt(stmt: Expression): R
    def visitPrintStmt(stmt: Print): R
    def visitFunctStmt(stmt: Funct): R
    def visitIfStmt(stmt: If): R
    def visitVarStmt(stmt: Var): R
    def visitReturnStmt(stmt: Return): R
    def visitWhileStmt(stmt: While): R
}

abstract class Stmt {
    def accept[R](visitor: sVisitor[R]): R
}

class Block(val statements: Array[Stmt]) extends Stmt {
    override def accept[R](visitor: sVisitor[R]): R = visitor.visitBlockStmt(this)
}

class Expression(var expression: Expr) extends Stmt {
    override def accept[R](visitor: sVisitor[R]): R = return visitor.visitExpressionStmt(this)
}

class Print(var expression: Expr) extends Stmt {
    override def accept[R](visitor: sVisitor[R]): R = return visitor.visitPrintStmt(this)
}

class Funct(val name: Token, val params: Array[Token], val body: Array[Stmt]) extends Stmt {
    override def accept[R](visitor: sVisitor[R]): R = return visitor.visitFunctStmt(this)
}

class If(var condition: Expr, var thenBranch: Stmt, var elseBranch: Stmt) extends Stmt {
    override def accept[R](visitor: sVisitor[R]): R = return visitor.visitIfStmt(this)
}

class Var(var name: Token, var initializer: Expr) extends Stmt {
    override def accept[R](visitor: sVisitor[R]): R = return visitor.visitVarStmt(this)
}

class Return(var keyword: Token, var value: Expr) extends Stmt {
    override def accept[R](visitor: sVisitor[R]): R = return visitor.visitReturnStmt(this)
}

class While(var condition: Expr, var body: Stmt) extends Stmt {
    override def accept[R](visitor: sVisitor[R]): R = return visitor.visitWhileStmt(this)
}