// package com.craftinginterpreters.lox
package expr
import token.Token


trait Visitor[R] {
	def visitBinaryExpr(expr: Binary): R
	def visitGroupingExpr(expr: Grouping): R
	def visitLiteralExpr(expr: Literal): R
	def visitUnaryExpr(expr: Unary): R
}

abstract class Expr {
	def accept[R](visitor: Visitor[R]): R
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

class Unary(val operator: Token, val right: Expr) extends Expr {
	override def accept[R](visitor: Visitor[R]): R = return visitor.visitUnaryExpr(this)
}