namespace Macaque

open Macaque.Tokens
open System

type Lexer (input: string) as self   = 

   let EOF_CHAR = '\000' 
   let mutable _position: int = 0   
   let mutable _readPosition: int = 0
   let mutable _ch: char = EOF_CHAR        
   
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

   let ReadString() =
        self.ReadChar()
        let position = _position
        while _ch <> '"' && _ch <> EOF_CHAR do self.ReadChar()
        self.ReadChar()
        input.[position.._position - 2]

   let Read (tokenType:TokenType) (literal:string) = 
        let token = Token(tokenType, literal)
        for _ in 1 .. literal.Length do self.ReadChar()
        token
      
   let PeakChar() = if _readPosition >= input.Length then EOF_CHAR else input.[_readPosition]

   let NextIs (nextChar:char) = if _readPosition >= input.Length then EOF_CHAR = nextChar else input.[_readPosition] = nextChar

   let rec Iterate(lex: Lexer) = seq {
        match lex.NextToken():Token with
        | tok when tok.Type = EOF -> yield tok
        | tok -> yield tok
                 yield! (Iterate lex)
   }

   do
    self.ReadChar()

   member private this.ReadChar() =             
        _ch <- if _readPosition >= input.Length then EOF_CHAR else  input.[_readPosition]            
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
            | '"' -> Token(STRING, ReadString())
            | '\000' -> Read EOF ""
            | '[' -> Read LBRACKET "["
            | ']' -> Read RBRACKET "]"
            | c when IsLetter(c) -> let ident = ReadIndentifier()                        
                                    Token(lookupIdent(ident), ident)

            | c when Char.IsDigit(c) -> Token(INT, ReadNumber())

            | _ -> Read ILLEGAL (_ch.ToString())
            
        
    member this.IterateOver() = Iterate this
    

    
        
    
    