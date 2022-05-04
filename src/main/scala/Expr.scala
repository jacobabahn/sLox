// package com.craftinginterpreters.lox
package expr
import token.Token


trait Visitor[R] {
	def visitAssignExpr(expr: Assign): R
	def visitBinaryExpr(expr: Binary): R
	def visitGroupingExpr(expr: Grouping): R
	def visitLiteralExpr(expr: Literal): R
	def visitLogicalExpr(expr: Logical): R
	def visitUnaryExpr(expr: Unary): R
	def visitVariableExpr(expr: Variable): R
}

abstract class Expr {
	def accept[R](visitor: Visitor[R]): R
}

class Assign(val name: Token, val value: Expr) extends Expr {
	def accept[R](visitor: Visitor[R]): R = visitor.visitAssignExpr(this)
}

class Binary(val left: Expr, val operator: Token, val right: Expr) extends Expr {
	override def accept[R](visitor: Visitor[R]): R = return visitor.visitBinaryExpr(this)
}

class Grouping(val expression: Expr) extends Expr {
	override def accept[R](visitor: Visitor[R]): R = return visitor.visitGroupingExpr(this)
}

class Literal(val value: Any) extends Expr {
	override def accept[R](visitor: Visitor[R]): R = return visitor.visitLiteralExpr(this)
}

class Logical(val left: Expr, val operator: Token, val right: Expr) extends Expr {
	override def accept[R](visitor: Visitor[R]): R = return visitor.visitLogicalExpr(this)
}

class Unary(val operator: Token, val right: Expr) extends Expr {
	override def accept[R](visitor: Visitor[R]): R = return visitor.visitUnaryExpr(this)
}

class Variable(val name: Token) extends Expr {
	override def accept[R](visitor: Visitor[R]): R = return visitor.visitVariableExpr(this)
}