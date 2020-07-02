namespace Macaque

module Tokens = 

    type TokenType = string
    
    type Token =
        struct
            val Type:TokenType
            val Literal:string
            new(type', literal) = { Type = type'; Literal = literal }
        end   

    [<Literal>] 
    let ILLEGAL = "ILLEGAL"

    [<Literal>]
    let EOF = "EOF"

    [<Literal>]
    let IDENT = "IDENT" // add, foobar, x, y, ...

    [<Literal>]
    let INT = "INT"   // 1343456

    // Operators
    [<Literal>]
    let ASSIGN = "="

    [<Literal>]
    let PLUS = "+"

    [<Literal>]
    let MINUS = "-"

    [<Literal>]
    let BANG = "!"

    [<Literal>]
    let ASTERISK = "*"

    [<Literal>]
    let SLASH = "/"

    [<Literal>]
    let LT = "<"

    [<Literal>]
    let GT = ">"

    [<Literal>]
    let EQ = "=="

    [<Literal>]
    let NOT_EQ = "!="

    // Delimiters
    [<Literal>]
    let COMMA = ","

    [<Literal>]
    let SEMICOLON = ";"

    [<Literal>]
    let LPAREN = "("

    [<Literal>]
    let RPAREN = ")"

    [<Literal>]
    let LBRACE = "{"

    [<Literal>]
    let RBRACE = "}"

    // Keywords
    [<Literal>]
    let FUNCTION = "FUNCTION"

    [<Literal>]
    let LET = "LET"

    [<Literal>]
    let TRUE = "TRUE"

    [<Literal>]
    let FALSE = "FALSE"

    [<Literal>]
    let IF = "IF"

    [<Literal>]
    let ELSE = "ELSE"

    [<Literal>]
    let RETURN = "RETURN"

    let keywords = Map [
        ("fn", FUNCTION);    
        ("let", LET);
        ("true", TRUE);
        ("false", FALSE);
        ("if", IF);
        ("else", ELSE);
        ("return", RETURN)]

    let lookupIdent ident =
        match keywords.TryFind ident with
        | Some(tok) -> tok
        | None -> IDENT
        
        
  