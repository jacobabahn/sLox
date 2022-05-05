package parser

import expr._
import stmt._
import scala.annotation.switch
import token.Token
import tokentype.TokenType
import lox.Lox
import scala.util.control.Breaks._


class Parser(var tokens: Array[Token]) {
    var current = 0

    def main(tokens: Array[Token]): Unit = {
        this.tokens = tokens
    }

    def parse(): Array[Stmt] = {
        var statements: Array[Stmt] = Array()

        while (!isAtEnd()) {
            statements = statements :+ declaration()
        }
        return statements
    }

    private def expression(): Expr = {
        return assignment()
    }

    private def declaration(): Stmt = {
        try {
            if (matchToken(Array(TokenType.FUN))) {
                return function("function")
            }
            if (matchToken(Array(TokenType.VAR))) {
                return varDeclaration()
            }
            return statement()
        } catch {
            case e: ParseError => {
                synchronize()
                return null
            }
        }
    }

    private def statement(): Stmt = {
        if (matchToken(Array(TokenType.IF))) then
            return ifStatement()
        if (matchToken(Array(TokenType.FOR))) then
            return forStatement()
        if (matchToken(Array(TokenType.PRINT))) then
            return printStatement()
        if (matchToken(Array(TokenType.RETURN))) then
            return returnStatement()
        if (matchToken(Array(TokenType.WHILE))) then
            return whileStatement()
        if (matchToken(Array(TokenType.LEFT_BRACE))) then
            return new Block(block())

        return expressionStatement()
    }

    private def forStatement(): Stmt = {
        consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.")
        var initializer: Stmt = Expression(null)

        if (matchToken(Array(TokenType.SEMICOLON))) then
            initializer = Expression(null)
        else if (matchToken(Array(TokenType.VAR))) then
            initializer = varDeclaration()
        else
            initializer = expressionStatement()
        
        var condition: Expr = Literal(null)
        if (!check(TokenType.SEMICOLON)) then
            condition = expression()
        
        consume(TokenType.SEMICOLON, "Expect ';' after loop condition.")

        var increment: Expr = Literal(null)
        if (!check(TokenType.RIGHT_PAREN)) then
            increment = expression()
        
        consume(TokenType.RIGHT_PAREN, "Expect ')' after for clauses.")
        var body = statement()

        if (increment != null) then
            body = Block(Array(body, Expression(increment)))

        if (condition == null) then
            condition = Literal(true)

        body = While(condition, body)

        if (initializer != null) then
            body = Block(Array(initializer, body))

        return body
    }

    private def ifStatement(): Stmt = {
        consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.")
        var condition = expression()
        consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition.")

        var thenBranch = statement()
        var elseBranch: Stmt = Expression(null)
        if (matchToken(Array(TokenType.ELSE))) then
            elseBranch = statement()
        
        return If(condition, thenBranch, elseBranch)
    }

    private def printStatement(): Stmt = {
        val value = expression()
        consume(TokenType.SEMICOLON, "Expect ';' after value.")
        return new Print(value)
    }

    private def returnStatement(): Stmt = {
        var keyword = previous()
        var value: Expr = Literal(null)
        if (!check(TokenType.SEMICOLON)) then
            value = expression()
        
        consume(TokenType.SEMICOLON, "Expect ';' after return value.")
        return new Return(keyword, value)
    }

    private def varDeclaration(): Stmt = {
        var name = consume(TokenType.IDENTIFIER, "Expect variable name.")
        var initializer: Expr = new Literal(null)
        if (matchToken(Array(TokenType.EQUAL))) then
            initializer = expression()

        consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.")
        return new Var(name, initializer)
    }

    private def whileStatement(): Stmt = {
        consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.")
        var condition = expression()
        consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.")
        var body = statement()

        return While(condition, body)
    }

    private def expressionStatement(): Stmt = {
        val expr = expression()
        consume(TokenType.SEMICOLON, "Expect ';' after expression.")
        return new Expression(expr)
    }

    private def function(kind: String): Funct = {
        var name = consume(TokenType.IDENTIFIER, "Expect " + kind + " name.")
        var parameters: Array[Token] = Array()
        var cond = true

        while (cond || matchToken(Array(TokenType.COMMA))) {
            if (!check(TokenType.RIGHT_PAREN)) then
                if (parameters.length >= 255) then
                    error(peek(), "Cannot have more than 255 parameters.")
                
                parameters = parameters :+ consume(TokenType.IDENTIFIER, "Expect parameter name.")
        }
        consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters.")

        consume(TokenType.LEFT_BRACE, "Expect '{' before " + kind + " body.")
        var body: Array[Stmt] = block()
        return Funct(name, parameters, body)
    }

    private def block(): Array[Stmt] = {
        var statements: Array[Stmt] = Array()

        while (!check(TokenType.RIGHT_BRACE) && !isAtEnd()) {
            statements = statements :+ declaration()
        }

        consume(TokenType.RIGHT_BRACE, "Expect '}' after block.")
        return statements
    }

    private def assignment(): Expr = {
        var expr = or()

        if (matchToken(Array(TokenType.EQUAL))) {
            var equals = previous()
            var value = assignment()

            if (expr.isInstanceOf[Variable]) {
                var name = expr.asInstanceOf[Variable].name
                return new Assign(name, value)
            }

            error(equals, "Invalid assignment target.")
        }
        
        return expr
    }

    private def or(): Expr = {
        var expr = and()

        while (matchToken(Array(TokenType.OR))) {
            var operator = previous()
            var right = and()
            expr = Logical(expr, operator, right)
        }
        
        return expr
    }

    private def and(): Expr = {
        var expr = equality()

        while (matchToken(Array(TokenType.AND))) {
            var operator = previous()
            var right = equality()
            expr = Logical(expr, operator, right)
        }
        
        return expr
    }

    private def equality(): Expr = {
        var expr = comparison()

        while (check(TokenType.BANG_EQUAL) || check(TokenType.EQUAL_EQUAL)) {
            val operator = previous()
            val right = comparison()
            expr = Binary(expr, operator, right)
        }

        return expr
    }

    private def comparison(): Expr = {
        var expr = term()

        while (matchToken(Array(TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL))) {
            val operator = previous()
            val right = term()
            expr = Binary(expr, operator, right)
        }

        return expr
    }

    private def term(): Expr = {
        var expr = factor()

        while (matchToken(Array(TokenType.MINUS, TokenType.PLUS))) {
            val operator = previous()
            val right = factor()
            expr = Binary(expr, operator, right)
        }

        return expr
    }

    private def factor(): Expr = {
        var expr = unary()

        while (matchToken(Array(TokenType.SLASH, TokenType.STAR))) {
            val operator = previous()
            val right = unary()
            expr = Binary(expr, operator, right)
        }

        return expr
    }

    private def unary(): Expr = {
        if (matchToken(Array(TokenType.BANG, TokenType.MINUS))) {
            val operator = previous()
            val right = unary()
            return Unary(operator, right)
        }

        return call()
    }

    private def finishCall(callee: Expr): Expr = {
        var arguments: Array[Expr] = Array()
        var cond = true

        if (!check(TokenType.RIGHT_PAREN)) then
            while (cond || matchToken(Array(TokenType.COMMA))) {
                if (arguments.length >= 255) then
                    error(peek(), "Cannot have more than 255 arguments.")
                arguments = arguments :+ expression()
                cond = false
            }

        var paren = consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments.")
        return Call(callee, paren, arguments)
    }

    private def call(): Expr = {
        var expr = primary()

        breakable {
            while (true) {
                if (matchToken(Array(TokenType.LEFT_PAREN))) then
                    expr = finishCall(expr)
                else
                    break
            }
        }

        return expr
    }

    private def primary(): Expr = {
        if (matchToken(Array(TokenType.FALSE))) then
            return Literal(false)

        if (matchToken(Array(TokenType.TRUE))) then
            return Literal(true)

        if (matchToken(Array(TokenType.NIL))) then
            return Literal(null)

        if (matchToken(Array(TokenType.NUMBER, TokenType.STRING))) then
            return Literal(previous().literal)
        
        if (matchToken(Array(TokenType.IDENTIFIER))) then
            return Variable(previous())

        if (matchToken(Array(TokenType.LEFT_PAREN))) then
            val expr = expression()
            consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")
            return Grouping(expr)
        throw error(peek(), "Expect expression.")
    }

    private def matchToken(types: Array[TokenType]): Boolean = {
        for (tok <- types) {
            if (check(tok)) then
                advance()
                return true
            }

            return false
        }


    private def consume(tok: TokenType, message: String): Token = {
        if (check(tok)) then
            return advance()
        else
            throw error(peek(), message)
    }

    private def check(tokenType: TokenType): Boolean = {
        if (isAtEnd()) {
            return false
        }

        return peek().toktype == tokenType
    }

    private def advance(): Token = {
        if (!isAtEnd()) {
            current += 1
        }

        return previous()
    }

    private def isAtEnd(): Boolean = {
        return peek().toktype == TokenType.EOF
    }

    private def peek(): Token = {
        return tokens(current)
    }

    private def previous(): Token = {
        return tokens(current - 1)
    }

    private def error(token: Token, message: String): Throwable = {
        var lox = new Lox()
        lox.error(token, message)
        return new ParseError()
    }

    private def synchronize(): Unit = {
        advance()

        while (!isAtEnd()) {
            if (previous().toktype == TokenType.SEMICOLON) then
                return

            val test = peek().toktype           
            test match {
                case TokenType.CLASS => 
                case TokenType.FUN =>
                case TokenType.VAR =>
                case TokenType.FOR =>
                case TokenType.IF =>
                case TokenType.WHILE =>
                case TokenType.PRINT =>
                case TokenType.RETURN => return
            }

            advance()
        }
    }


    class ParseError() extends RuntimeException()
}