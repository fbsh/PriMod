# mod: A Lightweight Module System Compiler

mod is a simplified version of the mod compiler, implementing a basic module system with type checking and inference. This document outlines the structure, workflow, and usage of the compiler.

## Components

1. **Syntax (syntax.sml)**
   - Defines the abstract syntax tree (AST) for the source language
   - Includes structures for modules, bindings, expressions, types, and signatures

2. **Tokens (token.sml)**
   - Defines the tokens used in lexical analysis

3. **Lexer (lexer.sml)**
   - Converts source code into a stream of tokens
   - Handles basic lexical errors

4. **Parser (parser.sml)**
   - Converts the token stream into an AST
   - Implements a recursive descent parser

5. **Internal Representation (internal.sml)**
   - Defines the internal representation of types, modules, and signatures
   - Includes utilities for type manipulation and unification

6. **Environment (env.sml)**
   - Manages the compilation environment
   - Keeps track of modules, values, types, and type variables in scope

7. **Elaboration (elaboration.sml)**
   - Converts the AST into the internal representation
   - Performs type checking and inference
   - Handles module and signature elaboration

8. **mod Interface (mod.sml)**
   - Provides high-level functions for parsing, elaboration, and compilation
   - Handles error reporting and pretty printing

9. **Main (main.sml)**
   - Implements the command-line interface for the compiler

## Workflow

1. **Lexical Analysis**: The source file is read and converted into a stream of tokens by the lexer.
2. **Parsing**: The token stream is parsed into an AST.
3. **Elaboration**: The AST is elaborated into the internal representation, including type checking, inference, and module system elaboration. The result is an abstract signature (asig) and purity information.
4. **Output**: The abstract signature and purity are pretty-printed.

## Key Features

- **Module System**: Supports basic modules and signatures
- **Type Inference**: Performs Hindley-Milner style type inference
- **Polymorphism**: Supports polymorphic types
- **Error Handling**: Provides error messages for lexical, parsing, and elaboration errors


## Example

Create a file `example.sml` with the following contents:

```sml
module Example = struct
  type t = int
  val x = 42
  module Inner = struct
    val y = x + 1
  end
end
```

Compile and run it:

```
./mod example.sml
```

This should output the elaborated abstract signature and purity of the module.

## Extending to PriML

To extend mod to support PriML features:

1. **Syntax Extension**: 
   - Add priority declarations and orderings to `syntax.sml`
   - Extend the parser in `parser.sml` to handle new syntax

2. **Internal Representation**: 
   - Modify `internal.sml` to include priorities and priority-aware commands

3. **Elaboration**: 
   - Update `elaboration.sml` to handle priority checking
   - Implement type checking for priority-aware commands (spawn, sync)

4. **Environment**: 
   - Extend `env.sml` to keep track of priorities and their ordering

5. **Scheduler**: 
   - Implement a new phase for scheduling or code generation that respects priorities

These extensions would allow mod to support priority-based concurrent programming while maintaining its module system and type checking capabilities.

## Contributing

Contributions to mod are welcome! Please submit pull requests or open issues on the project's GitHub repository.

## License

mod is released under the MIT License. See the LICENSE file for details.