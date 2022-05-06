# sLox

This project showcases a lox interpreter written in Scala. It was created by following the java implementation from [Crafting Interpreters](https://github.com/munificent/craftinginterpreters). 

## Implementation
This implementation goes up to chapter 10.2 in the crafting interpreters book and includes the following modifications:

### Const Keyword
The const keyword I implemented works similarly to const in JavaScript or val in typescript. If a variable is declared using the const keyword, it cannot be reassigned.

### Input
Input is a native function that works by calling input()

### Random
Random is a native function that works by calling random(int), where int is the upper bound of the random value being generated.
