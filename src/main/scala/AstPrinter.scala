package astprinter
import expr._
import token.Token
import tokentype.TokenType

class AstPrinter extends Visitor[String] {
    def printVal(expr: Expr): String = expr.accept(this)

    override def visitBinaryExpr(expr: Binary) = {
        return parenthesize(expr.operator.lexeme, expr.left, expr.right)
    }

    override def visitGroupingExpr(expr: Grouping) = {
        return parenthesize("group", expr.expression)
    }

    override def visitLiteralExpr(expr: Literal) = {
        if (expr.value == null) then
            return "nil"
        
        return expr.value.toString()
    }

    override def visitUnaryExpr(expr: Unary) = {
        return parenthesize(expr.operator.lexeme, expr.right)
    }

    def parenthesize(name: String, exprs: Expr*): String = {
        val builder = new StringBuilder()

        builder.append("(").append(name)
        for (expr <- exprs) {
            builder.append(" ")
            builder.append(expr.accept(this))
        }
        builder.append(")")

        return builder.toString()
    }

    def main() = {
        val expression = new Binary(
            new Unary(
                new Token(TokenType.MINUS, "-", null, 1),
                new Literal(123)),
            new Token(TokenType.STAR, "*", null, 1),
            new Grouping(
                new Literal(45.6)))
        
        println(new AstPrinter().printVal(expression))
    }
}