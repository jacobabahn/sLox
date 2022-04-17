package scanner
import tokentype.TokenType
import token.Token
import lox.Lox

class Scanner(var source: String) {
    var tokens: Array[Token] = Array()
    var start: Int = 0
    var current = 0
    var line = 1

    def main(source: String) = {
        this.source = source
    }

    def scanTokens(): Array[Token] = {
        while (!isAtEnd) {
            start = current
            scanToken()
        }

        tokens :+ (new Token(TokenType.EOF, "", null, line))
        return tokens
    }

    def scanToken(): Any = {
        var c = this.advance()

        c match {
            case '(' => addToken(TokenType.LEFT_PAREN)
            case ')' => addToken(TokenType.RIGHT_PAREN)
            case '{' => addToken(TokenType.LEFT_BRACE)
            case '}' => addToken(TokenType.RIGHT_BRACE)
            case ',' => addToken(TokenType.COMMA)
            case '.' => addToken(TokenType.DOT)
            case '-' => addToken(TokenType.MINUS)
            case '+' => addToken(TokenType.PLUS)
            case ';' => addToken(TokenType.SEMICOLON)
            case '*' => addToken(TokenType.STAR)
            case '!' => addToken(if checkToken('=') then (TokenType.BANG_EQUAL) else (TokenType.BANG))
            case '=' => addToken(if checkToken('=') then (TokenType.EQUAL_EQUAL) else (TokenType.EQUAL))
            case '<' => addToken(if checkToken('=') then (TokenType.LESS_EQUAL) else (TokenType.LESS))
            case '>' => addToken(if checkToken('=') then (TokenType.GREATER_EQUAL) else (TokenType.GREATER))
            case '/' => {
                if checkToken('/') then {
                    while (peek() != '\n' && !isAtEnd) {
                        advance()
                    }
                } else {
                    addToken(TokenType.SLASH)
                }
            }
            case ' ' | '\r' | '\t' => {}
            case '\n' => line += 1
            case '"' => string()
            case 'o' => {
                if checkToken('r') then
                    addToken(TokenType.OR)
            }
            case default => {
                if isDigit(c) then
                    number()
                else if isAlpha(c) then
                    identifier()
                else
                    var lox = new Lox()
                    lox.error(line, "Unexpected character.")
            }
        }
    }

    private def identifier() = {
        while (isAlphaNumeric(peek())) {
            advance()
        }

        val text = source.substring(start, current)
        val token = keywords.get(text)
        if (token == null) {
            addToken(TokenType.IDENTIFIER)
        } else {
            addToken(token)
        }
    }

    private def number() = {
        while (isDigit(peek())) {
            advance()
        }

        if (peek() == '.' && isDigit(peekNext())) {
            advance()
            while (isDigit(peek())) {
                advance()
            }
            
            addToken(TokenType.NUMBER, (source.substring(start, current)).toDouble)
        }
    }

    private def string(): Any = {
        while (peek() != '"' && !isAtEnd) {
            if (peek() == '\n') line += 1
            advance()
        }

        if (isAtEnd) {
            var lox = new Lox()
            lox.error(line, "Unterminated string.")
            return
        }

        advance()

        val value = source.substring(start + 1, current - 1)
        addToken(TokenType.STRING, value)
    }

    private def checkToken(expected: Char): Boolean = {
        if (isAtEnd) return false
        if (source.charAt(current) != expected) return false

        current += 1
        return true
    }

    private def peek(): Char = {
        if (isAtEnd) return '\u0000'
        return source.charAt(current)
    }

    private def peekNext(): Char = {
        if (current + 1 >= source.length) return '\u0000'
        return source.charAt(current + 1)
    }

    private def isAlpha(c: Char): Boolean = {
        return (c >= 'a' && c <= 'z') ||
            (c >= 'A' && c <= 'Z') ||
            c == '_'
    }

    private def isAlphaNumeric(c: Char): Boolean = {
        return isAlpha(c) || isDigit(c)
    }

    private def isDigit(c: Char): Boolean = {
        return c >= '0' && c <= '9'
    }

    private def isAtEnd = current >= source.length

    private def advance(): Char = {
        current += 1
        return source.charAt(current - 1)
    }

    private def addToken(tokenType: Any) = {
        val text = source.substring(start, current)
        tokens :+ (new Token(tokenType, text, null, line))
    }

    val keywords = Map(
        "and" -> TokenType.AND,
        "class" -> TokenType.CLASS,
        "else" -> TokenType.ELSE,
        "false" -> TokenType.FALSE,
        "for" -> TokenType.FOR,
        "fun" -> TokenType.FUN,
        "if" -> TokenType.IF,
        "nil" -> TokenType.NIL,
        "or" -> TokenType.OR,
        "print" -> TokenType.PRINT,
        "return" -> TokenType.RETURN,
        "super" -> TokenType.SUPER,
        "this" -> TokenType.THIS,
        "true" -> TokenType.TRUE,
        "var" -> TokenType.VAR,
        "while" -> TokenType.WHILE
    )
}
