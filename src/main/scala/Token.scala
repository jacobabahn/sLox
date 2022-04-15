class Token {
    var toktype: TokenType
    var lexeme: String
    var literal: Any
    var line: Int
    
    def Token(toktype: TokenType, lexeme: String, literal: Any, line: Int) = {
        this.toktype = toktype
        this.lexeme = lexeme
        this.literal = literal
        this.line = line
    }

    def toString() = {
        return s"$toktype $lexeme $literal $line"
    }
}