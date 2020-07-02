namespace Macaque

open Macaque.Tokens

type Lexer (input: string) as self = 

   let mutable position: int = 0   
   let mutable readPosition: int = 0
   let mutable ch: char = '\000'        
   
   do
    self.ReadChar()

   member private this.ReadChar() =             
        if readPosition >= input.Length then
            ch <- '\000'
        else 
            ch <-  input.Chars(readPosition)
            
        position <-  readPosition
        readPosition <-  readPosition + 1            

    override this.ToString() = sprintf "Lexer[Pos: %i, Read: %i, Ch: %O]"  position  readPosition ch
               
    member this.NextToken() = 
        let token = 
            match ch with
            | '=' -> Token(ASSIGN, "=")
            | ';' -> Token(SEMICOLON, ";")
            | '+' -> Token(PLUS, "+")
            | '(' -> Token(LPAREN, "(")
            | ')' -> Token(RPAREN, ")")
            | ',' -> Token(COMMA, ",")
            | '\000' -> Token(EOF, "END OF FILE")
            | _   -> Token(ILLEGAL, "ILLEGAL")  
        this.ReadChar()
        token   
    


        



            

                

     



