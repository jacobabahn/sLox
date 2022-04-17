package lox
import scanner.Scanner

@main def main() = {
	var lox = Lox()
	lox.main()
}

class Lox:
	var hadError = false

	def main(args: Array[String]) = {
		println("main")
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

	def report(line: Int, where: String, message: String) = {
		println("[line " + line + "] Error" + where + ": " + message)
		hadError = true
	}
