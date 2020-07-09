namespace Macaque

open Macaque.Tokens
open System

type Lexer (input: string) as self = 

   let mutable _position: int = 0   
   let mutable _readPosition: int = 0
   let mutable _ch: char = '\000'        
   
   let IsLetter (ch:char) = Char.IsLetter(ch) || ch = '_'
   
   let ReadIndentifier(): string = 
        let position = _position
        while IsLetter _ch do self.ReadChar()
        input.[position.._position - 1]
   
   let SkepWhiteSpece() = while Char.IsWhiteSpace _ch do self.ReadChar()            

   let ReadNumber() = 
        let position = _position
        while Char.IsDigit _ch do self.ReadChar()
        input.[position.._position - 1]

   let Read (tokenType:TokenType) (literal:string) = 
        let token = Token(tokenType, literal)
        for _ in 1 .. literal.Length do self.ReadChar()
        token
      
   let PeakChar() = if _readPosition >= input.Length then '\000' else input.[_readPosition]

   let NextIs (nextChar:char) = if _readPosition >= input.Length then '\000' = nextChar else input.[_readPosition] = nextChar

   do
    self.ReadChar()

   member private this.ReadChar() =             
        if _readPosition >= input.Length then
            _ch <- '\000'
        else 
            _ch <-  input.[_readPosition]
            
        _position <-  _readPosition
        _readPosition <-  _readPosition + 1            

    override this.ToString() = sprintf "Lexer[Pos: %i, Read: %i, Ch: %O]"  _position  _readPosition _ch        
         
    member this.NextToken() = 
        SkepWhiteSpece()
        match _ch with 
            | ';' -> Read SEMICOLON ";"
            | '+' -> Read PLUS "+"
            | '(' -> Read LPAREN "("
            | ')' -> Read RPAREN ")"
            | ',' -> Read COMMA ","
            | '{' -> Read LBRACE "{"
            | '}' -> Read RBRACE "}"
            | '-' -> Read MINUS "-"
            | '/' -> Read SLASH "/"
            | '*' -> Read ASTRISK "*"
            | '<' -> Read LT "<"
            | '>' -> Read GT ">"
            | '=' -> if NextIs '=' then Read EQ "==" else Read ASSIGN "="  
            | '!' -> if NextIs '=' then Read NOT_EQ "!=" else Read BANG "!"
            | '\000' -> Read EOF ""

            | c when IsLetter(c) -> let ident = ReadIndentifier()                        
                                    Token(lookupIdent(ident), ident)

            | c when Char.IsDigit(c) -> Token(INT, ReadNumber())

            | _ -> Read ILLEGAL (_ch.ToString())
                
        
    