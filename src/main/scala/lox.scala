package lox
import scanner.Scanner

object sLox {
	def main(args: Array[String]): Unit = {
		var lox = Lox()
		lox.main(args)
	}
}

class Lox:
	var hadError = false

	def main(args: Array[String]) = {
		if (args.length > 1) then
			println("Usage: lox [script]")
			sys.exit(64)
		else if (args.length == 1) then
			runFile(args(0))
		else
			runPrompt()
	}

	private def runFile(path: String) = {
		run(io.Source.fromFile(path).mkString)
		if (hadError) sys.exit(65)
	}
	
	private def runPrompt() = {
		while (true) {
			print("> ")
			val line = io.StdIn.readLine()
			run(line)
			hadError = false
		}
	}

	private def run(source: String) = {
		val scanner = new Scanner(source)
		val tokens = scanner.scanTokens()

		for (token <- tokens) {
			println(token)
		}
	}

	def error(line: Int, message: String) = {
		report(line, "", message)
	}

	def error(token: Token, message: String) = {
		if (token.toktype == TokenType.EOF) then
			report(token.line, " at end", message)
		else
			report(token.line, " at '" + token.lexeme + "'", message)
	}

	def report(line: Int, where: String, message: String) = {
		println("[line " + line + "] Error" + where + ": " + message)
		hadError = true
	}
