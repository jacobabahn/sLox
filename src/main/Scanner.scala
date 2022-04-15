class Scanner {
    var source: String
    var tokens: Array
    var start = 0
    var current = 0
    var line = 1

    def Scanner(source: String) = {
        this.source = source
    }

    def scanTokens() = {
        while (!isAtEnd) {
            start = current
            scanToken()
        }

        tokens.push(new Token(EOF, "", null, line))
        return tokens
    }

    def scanToken() = {
        c = advance()
        c match {
            case '(' => addToken(LEFT_PAREN)
            case ')' => addToken(RIGHT_PAREN)
            case '{' => addToken(LEFT_BRACE)
            case '}' => addToken(RIGHT_BRACE)
            case ',' => addToken(COMMA)
            case '.' => addToken(DOT)
            case '-' => addToken(MINUS)
            case '+' => addToken(PLUS)
            case ';' => addToken(SEMICOLON)
            case '*' => addToken(STAR)
            case '!' => addToken(if match('=') then (BANG_EQUAL) else (BANG))
            case '=' => addToken(if match('=') then (EQUAL_EQUAL) else (EQUAL))
            case '<' => addToken(if match('=') then (LESS_EQUAL) else (LESS))
            case '>' => addToken(if match('=') then (GREATER_EQUAL) else (GREATER))
            case '/' => {
                if match('/') then {
                    while (peek() != '\n' && !isAtEnd) {
                        advance()
                    }
                } else {
                    addToken(SLASH)
                }
            }
            case ' ' | '\r' | '\t' => {}
            case '\n' => line += 1
            case '"' => string()
            case default => {
                if isDigit(c) {
                    number()
                } else
                Lox.error(line, "Unexpected character.")
            }
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
            
            // left off here
        }
    }

    private def string() = {
        while (peek() != '"' && !isAtEnd) {
            if (peek() == '\n') line += 1
            advance()
        }

        if (isAtEnd) {
            Lox.error(line, "Unterminated string.")
            return
        }

        advance()

        val value = source.substring(start + 1, current - 1)
        addToken(STRING, value)
    }

    private def match(expected) = {
        if (isAtEnd) return false
        if (source[current] != expected) return false
        current += 1
        return true
    }

    private def peek() = {
        if (isAtEnd) return '\0'
        return source[current]
    }

    private def isDigit(c) = {
        return c >= '0' && c <= '9'
    }

    private def isAtEnd = current >= source.length

    private def advance = {
        current += 1
        source(current - 1)
    }

    private def addToken(tokenType: TokenType) = {
        val text = source.substring(start, current)
        tokens.push(new Token(tokenType, text, null, line))
    }


}