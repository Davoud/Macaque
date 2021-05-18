namespace Macaque

module Tokens = 
             
    type TokenType =
        | ILLEGAL 
        | EOF       // End of File
        | IDENT     // add, fooBar, x, y, ...
        | INT       // 12323
        | ASSIGN    // =
        | PLUS      // +
        | MINUS     // -
        | BANG      // !
        | ASTRISK   // *
        | SLASH     // /    
        | LT        // <
        | GT        // >
        | EQ        // ==   
        | NOT_EQ    // !=                      
        | COMMA     // ,            
        | SEMICOLON // ;
        | LPAREN    // (
        | RPAREN    // )
        | LBRACE    // {
        | RBRACE    // }
        | LET       // Let
        | FUNCTION  // fn
        | TRUE      // true
        | FALSE     // false
        | IF        // if
        | ELSE      // else           
        | RETURN    // return
        | STRING    // "... "
        | LBRACKET  // "["
        | RBRACKET  // "]"

    type Token =
           struct
               val Type:TokenType
               val Literal:string
               new(type', literal) = { Type = type'; Literal = literal }
               override x.ToString() = sprintf "Token %A('%s')" x.Type x.Literal
               member x.IsNotEOF(): bool = x.Type <> EOF
           end  

    let keywords = Map [
        ("fn", FUNCTION);    
        ("let", LET);
        ("true", TRUE);
        ("false", FALSE);
        ("if", IF);
        ("else", ELSE);
        ("return", RETURN)]

    let lookupIdent identifier =
        match keywords.TryFind identifier with
        | Some(token) -> token
        | None -> IDENT
        
        
  