namespace FSharpCalculator.Core

open System.Text.RegularExpressions

module Calculator =
    
    type Token =
        | TNumber of double
        | TOp of string
        | TLeftParen
        | TRightParen
    
    let tokenize1 (token : string) : Token =
        try TNumber (double token) with | _ ->
                                           if token = "(" then TLeftParen
                                           elif token = ")" then TRightParen
                                           else TOp token

    let tokenize (input : string) : array<Token> =
        Regex.Split(input, "\\s+")
        |> Array.map tokenize1
    
    type Expression =
        | Number of double
        | Op of string
    
    type Associativity = Left | Right
    
    type OperatorProperties =
        { precedence: uint
          associativity: Associativity }
    
    let precedenceTable : Map<string, OperatorProperties> =
        Map [ "^", { precedence = 4u; associativity = Right }
              "*", { precedence = 3u; associativity = Left }
              "/", { precedence = 3u; associativity = Left }
              "+", { precedence = 2u; associativity = Left }
              "-", { precedence = 2u; associativity = Left } ]
    
    type ShuntingState =
        { yard: list<Token>
          output: list<Expression> }
    
    let initialShuntingState : ShuntingState =
         { yard = []; output = [] }
    
    let stepShuntingState (current : ShuntingState) (token: Token) : ShuntingState =
        match token with
        | TNumber d -> { yard = current.yard; output = Number d :: current.output }
        | TOp op ->
            let ourPrecedenceEntry = precedenceTable.[op]
            let ourPrecedence = ourPrecedenceEntry.precedence
            let ourAssociativity = ourPrecedenceEntry.associativity
            let rec popOperators (current : ShuntingState) : ShuntingState =
                match List.tryHead current.yard with
                | Some (TOp otherOp) ->
                    let theirPrecedence = precedenceTable.[otherOp].precedence
                    
                    if theirPrecedence > ourPrecedence || (theirPrecedence >= ourPrecedence && ourAssociativity = Left) then
                        { yard = current.yard.Tail; output = Op otherOp :: current.output }
                    else
                        current
                | _ -> current
            
            let popped = popOperators current
            { yard = token :: popped.yard; output = popped.output }
        | TLeftParen -> { yard = TLeftParen :: current.yard; output = current.output }
        | TRightParen ->
            let rec popOperators (current : ShuntingState) : ShuntingState =
                match List.tryHead current.yard with
                | Some (TOp otherOp) ->
                    { yard = current.yard.Tail; output = Op otherOp :: current.output }
                | _ -> current
            
            let popUntilParen = popOperators current
            match List.tryHead popUntilParen.yard with
            | Some TLeftParen -> { yard = popUntilParen.yard.Tail; output = popUntilParen.output }
    
    let parse (tokens : array<Token>) : list<Expression> =
        let rec popAll (current : ShuntingState) : ShuntingState =
            match List.tryHead current.yard with
            | Some (TOp operator) ->
                let moveToken = { yard = current.yard.Tail; output = Op operator :: current.output }
                popAll moveToken
            | None -> current
        
        let consumeAll = (Array.fold stepShuntingState initialShuntingState tokens)
        let finish = consumeAll |> popAll
        List.rev finish.output

    let operate (stack : list<double>) (op : string) : option<list<double>> =
        match op with
        | "+" ->
            match stack with
            | a :: b :: tail -> Some ((a + b) :: tail)
            | _ -> None
        | "-" ->
            match stack with
            | a :: b :: tail -> Some ((a - b) :: tail)
            | _ -> None
        | "*" ->
            match stack with
            | a :: b :: tail -> Some ((a * b) :: tail)
            | _ -> None
        | "/" ->
            match stack with
            | a :: b :: tail -> Some ((a / b) :: tail)
            | _ -> None
        | "^" ->
            match stack with
            | a :: b :: tail -> Some ((a ** b) :: tail)
            | _ -> None
    
    let evaluate (exprs : list<Expression>) : option<double> =
        let rec go (exprs : list<Expression>) (stack : list<double>) : option<list<double>> =
            match List.tryHead exprs with
            | Some (Op s) ->
                match operate stack s with
                | Some ns -> go exprs.Tail ns
                | None -> None
            | Some (Number d) ->
                go exprs.Tail (d :: stack)
            | None -> Some stack
        
        go exprs []
        |> Option.bind List.tryHead 
