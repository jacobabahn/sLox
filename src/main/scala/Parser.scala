package com.craftinginterpreters.lox;


class Parser {
    var tokens: Array[Token] = Array()
    var current = 0

    def main(tokens: Array[Token]): Unit = {
        this.tokens = tokens
    }

    private def expression(): Expr = {
        return equality()
    }

    private def equality(): Expr = {
        var expr = comparison()

        while (checkToken(TokenType.EQUAL) || checkToken(TokenType.NOT_EQUAL)) {
            val operator = previous()
            val right = comparison()
            expr = Binary(expr, operator, right)
        }

        return expr
    }

    private def comparison(): Expr = {
        var expr = term()

        while (matchToken(Array(TokenType.GREATER, checkToken(TokenType.GREATER_EQUAL), checkToken(TokenType.LESS), checkToken(TokenType.LESS_EQUAL)) {
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

        if (matchToken(Array(TokenType.LEFT_PAREN))) then
            val expr = expression()
            consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")
            return Grouping(expr)
    }

    private def matchToken(types: Array[TokenType]): Boolean = {
        for (tok <- types) {
            if (check(tok)) then
                advance()
                return true
            }
        }

        return false
    }

    private def consume(tok: TokenType, message: String): Unit = {
        if (check(tok)) then
            advance()
        else
            error(peek(), message)
    }

    private def check(tokenType: TokenType): Boolean = {
        if (isAtEnd()) {
            return false
        }

        return peek().getType() == tokenType
    }

    private def advance(): Token = {
        if (!isAtEnd()) {
            current += 1
        }

        return previous()
    }

    private def isAtEnd(): Boolean = {
        return peek().getType() == TokenType.EOF
    }

    private def peek(): Token = {
        return tokens(current)
    }

    private def previous(): Token = {
        return tokens(current - 1)
    }

    private def error(token: Token, message: String): Nothing = {
        Lox.error(token, message)
        return new ParseError()
    }
}