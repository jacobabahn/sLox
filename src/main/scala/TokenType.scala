enum TokenType {
    case LEFT_PAREN extends TokenType
    case RIGHT_PAREN extends TokenType
    case LEFT_BRACE extends TokenType
    case RIGHT_BRACE extends TokenType
    case COMMA extends TokenType
    case DOT extends TokenType
    case MINUS extends TokenType
    case PLUS extends TokenType
    case SEMICOLON extends TokenType
    case SLASH extends TokenType
    case STAR extends TokenType

    case BANG extends TokenType
    case BANG_EQUAL extends TokenType
    case EQUAL extends TokenType
    case EQUAL_EQUAL extends TokenType
    case GREATER extends TokenType
    case GREATER_EQUAL extends TokenType
    case LESS extends TokenType
    case LESS_EQUAL extends TokenType

    case IDENTIFIER extends TokenType
    case STRING extends TokenType
    case NUMBER extends TokenType

    case AND extends TokenType
    case CLASS extends TokenType
    case ELSE extends TokenType
    case FALSE extends TokenType
    case FUN extends TokenType
    case FOR extends TokenType
    case IF extends TokenType
    case NIL extends TokenType
    case OR extends TokenType
    case PRINT extends TokenType
    case RETURN extends TokenType
    case SUPER extends TokenType
    case THIS extends TokenType
    case TRUE extends TokenType
    case VAR extends TokenType
    case WHILE extends TokenType
}