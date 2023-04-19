module Eval

    open StateMonad
    open System

    (* Code for testing *)

    let hello = ('H', 4)::('E', 1)::('L', 1)::('L', 1)::('O', 1)::[] 
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add a b = a >>= (fun x -> b >>= (fun y -> ret (x+y)))
    let div a b = a >>= (fun x -> b >>= (fun y -> 
                                                                match y with
                                                                | y when y = 0 -> fail DivisionByZero 
                                                                | _ -> ret(x/y)))      

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let binop f a b =
        a >>= fun x ->
        b >>= fun y ->
        ret (f x y)    

    let rec arithEval a : SM<int> = 
        match a with
        | N n -> ret n
        | V x -> lookup x
        | WL -> wordLength
        | PV a1 -> arithEval a1 >>= (fun r -> pointValue r)
        | Add (a1, a2) -> binop ( + ) (arithEval a1) (arithEval a2)
        | Sub (a1, a2) -> binop ( - ) (arithEval a1) (arithEval a2)
        | Mul (a1, a2) -> binop ( * ) (arithEval a1) (arithEval a2)
        | Div (a1, a2) -> div (arithEval a1) (arithEval a2)
        | Mod (a1,a2) -> arithEval a2 >>= fun r2 -> if r2 <> 0 then arithEval a1 >>= (fun r1 -> ret (r1%r2)) else fail DivisionByZero
        | CharToInt c -> charEval c >>= (fun r -> ret (int r))

    and charEval c : SM<char> = 
        match c with
        | C(c) -> ret c
        | ToUpper(c) -> charEval c >>= (fun r -> ret (Char.ToUpper r))
        | ToLower(c) -> charEval c >>= (fun r -> ret (Char.ToLower r))
        | CV(a) -> arithEval a >>= (fun r -> characterValue r)
        | IntToChar a -> arithEval a >>= (fun r -> ret (char (r + int '0'))) 

    let rec boolEval b : SM<bool> = 
        match b with
        | TT -> ret true
        | FF -> ret false

        | AEq(a1,a2) -> binop ( = ) (arithEval a1) (arithEval a2)
        | ALt(a1,a2) -> binop ( < ) (arithEval a1) (arithEval a2)

        | Not(b1) -> boolEval b1 >>= (fun r -> ret (not r))
        | Conj(b1, b2) -> binop ( && ) (boolEval b1) (boolEval b2)

        | IsDigit(c) -> charEval c >>= (fun r -> ret (Char.IsDigit r))
        | IsLetter(c) -> charEval c >>= (fun r -> ret (Char.IsLetter r))
        | IsVowel(c) -> charEval c >>= (fun r -> ret ("aeiouæøåAEIOUÆØÅ ".Contains r)) 



    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = 
        match stmnt with
        | Declare x -> declare x
        | Ass(x, a) ->
            let v = arithEval a
            v >>= (fun a1 -> update x a1)
        | Skip -> ret ()
        | Seq(stmnt1, stmnt2) -> stmntEval stmnt1 >>>= stmntEval stmnt2
        | ITE(guard, stmnt1, stmnt2) -> push >>>= (boolEval guard >>= (fun b -> if b then stmntEval stmnt1 else stmntEval stmnt2)) >>>= pop
        | While (guard, stmnt) -> push >>>= (boolEval guard >>= (fun b -> if b then stmntEval stmnt >>>= stmntEval (While(guard, stmnt)) else ret ())) >>>= pop


(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"

    type coord = int * int

    type boardFun = coord -> squareFun option

    let stmntToBoardFun stm m = failwith "Not implemented"