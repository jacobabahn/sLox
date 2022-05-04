package parser

import expr._
import stmt._
import scala.annotation.switch
import token.Token
import tokentype.TokenType
import lox.Lox


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
        return equality()
    }

    private def declaration(): Stmt = {
        try {
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
        if (matchToken(Array(TokenType.PRINT))) {
            return printStatement()
        }

        return expressionStatement()
    }

    private def printStatement(): Stmt = {
        val value = expression()
        consume(TokenType.SEMICOLON, "Expect ';' after value.")
        return new Print(value)
    }

    private def varDeclaration(): Stmt = {
        var name = consume(TokenType.IDENTIFIER, "Expect variable name.")
        var initializer: Expr = new Literal(null)
        if (matchToken(Array(TokenType.EQUAL))) then
            initializer = expression()

        consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.")
        return new Var(name, initializer)
    }

    private def expressionStatement(): Stmt = {
        val expr = expression()
        consume(TokenType.SEMICOLON, "Expect ';' after expression.")
        return new Expression(expr)
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

        return primary()
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