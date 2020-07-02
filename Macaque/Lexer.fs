namespace Macaque

open Macaque.Tokens

type Lexer = 
    struct 
       val input: string
       val mutable position: int
       val mutable readPosition: int
       val mutable ch: char
       new(input) = { input = input; position = 0; readPosition = 0; ch = (char)0 }
       
       member x.readChar(): Lexer = 
        if x.readPosition >= x.input.Length then
           x.ch <- (char)0
        else 
           x.ch <- x.input.Chars(x.readPosition)
        x.position <- x.readPosition
        x.readPosition <- x.readPosition + 1
        x
    end

module LexerOp =
    type Lexer with
        member x.NextToken(): Token =
            new Token(ASSIGN, "=")

        static member New(input: string) = Lexer(input).readChar()
            

        



            

                

     



