package token
import tokentype.TokenType

class Token(var toktype: Any , var lexeme: String, var literal: Any, var line: Int) {
    def main(toktype: TokenType, lexeme: String, literal: Any, line: Int) = {
        this.toktype = toktype
        this.lexeme = lexeme
        this.literal = literal
        this.line = line
    }

    override def toString() = {
        return s"$toktype $lexeme $literal $line"
    }
}