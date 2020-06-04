(* well formed instructions *)
type command = PushI of int 
             | PushS of string 
             | PushN of string 
             | PushB of bool
             | PushU
             | Add | Sub | Mul | Div | Rem | Neg
             | Pop
             | Swap
             | Concat
             | And | Or | Not
             | Equal | LessThan
             | Bind
             | If
             | Block of command list
             | Call
             | Def of (string * string * command list*bool)
             | Return
             | InOutDef of (string * string * command list*bool)

(* things that go on a stack *)
type stackVal = 
  I of int 
| S of string 
| N of string
| B of bool 
| U (* unit *)
| E (* error *)
| Func of (string * string * (command list) * env*bool) (*func name, param name, commands, environment  *)
| InOutFunc of (string * string * (command list) * env*bool)
and env = (string * stackVal) list

(* If we limit our interactions with the environment to these helper functions, we can safely change how we encode the environment without changing much other code *)
(* type value = SV of stackVal
          | Func of (string * string * (command list) * env) (*func name, param name, commands, environment  *)
          | InOutFunc of (string * string * (command list) * env)
and env = (string * value) list *)

let insert (s: string)  (v : stackVal) (env: env) : env = (s, v)::env

let rec fetch (name: string)  (env: env) : stackVal option = 
    match env with
      | (name' , v) :: rest -> if name = name' then Some v else fetch name rest
      | []                  -> None
      
let empEnv = []


let rec run (commands: command list) (stack: stackVal list) (env: env) : stackVal list * env = 
  (* if stackVal is a variable, what does it resolve to in the current environment? *)
  let res (sv : stackVal) : stackVal = 
    match sv with 
      | N n -> (match fetch n env with  
                  | Some n' -> n'
                  | None    -> N n)
      | sv -> sv
  in let bad rest : stackVal list*env = (run rest (E :: stack) env) (* everything fails in the same way*)
  in match (commands , stack)  with
  | ([]              , _         ) -> stack, env
  
  | (PushI i  :: rest, _         ) -> run rest (I i :: stack) env
  
  | (PushS s  :: rest, _         ) -> run rest (S s :: stack) env
  
  | (PushN n  :: rest, _         ) -> run rest (N n :: stack) env
  
  | (PushB b  :: rest, _         ) -> run rest (B b :: stack) env
   
  | (PushU    :: rest, _         ) -> run rest (U :: stack) env
  
  | (Add      :: rest, x ::y ::s') -> (match (res x, res y) with 
                                       | (I i, I j) -> run rest (I (i+j) :: s') env 
                                       | _          -> bad rest)
  
  | (Sub      :: rest, x ::y ::s') -> (match (res x, res y) with 
                                       | (I i, I j) -> run rest (I (i-j) :: s') env 
                                       | _          -> bad rest)
  
  | (Mul      :: rest, x ::y ::s') -> (match (res x, res y) with 
                                       | (I i, I j) -> run rest (I (i*j) :: s') env 
                                       | _          -> bad rest)
  
  | (Div      :: rest, x ::y ::s') -> (match (res x, res y) with 
                                       | (I i, I 0) -> bad rest
                                       | (I i, I j) -> run rest (I (i/j) :: s') env 
                                       | _          -> bad rest)
  
  | (Rem      :: rest, x ::y ::s') -> (match (res x, res y) with 
                                       | (I i, I 0) -> bad rest
                                       | (I i, I j) -> run rest (I (i mod j) :: s') env 
                                       | _          -> bad rest)
  
  | (Neg      :: rest, x :: s'   ) -> (match (res x) with 
                                       | (I i) -> run rest (I (-i) :: s') env 
                                       | _     -> bad rest)

  | (Concat   :: rest, x ::y ::s') -> (match (res x, res y) with 
                                       | (S i, S j) -> run rest (S (i ^ j) :: s') env 
                                       | _          -> bad rest)
  
  | (And      :: rest, x ::y ::s') -> (match (res x, res y) with 
                                       | (B i, B j) -> run rest (B (i && j) :: s') env 
                                       | _          -> bad rest)
  
  | (Or       :: rest, x ::y ::s') -> (match (res x, res y) with 
                                       | (B i, B j) -> run rest (B (i || j) :: s') env 
                                       | _          -> bad rest)
  
  | (Not      :: rest, x :: s'   ) -> (match (res x) with 
                                       | (B i) -> run rest (B (not i) :: s') env 
                                       | _     -> bad rest)
  
  | (Equal    :: rest, x ::y ::s') -> (match (res x, res y) with 
                                       | (I i, I j) -> run rest (B (i = j) :: s') env 
                                       | _          -> bad rest)
  | (LessThan :: rest, x ::y ::s') -> (match (res x, res y) with 
                                       | (I i, I j) -> run rest (B (i < j) :: s') env 
                                       | _          -> bad rest)
                                             
  | (If       :: rest,x::y::z::s') -> (match res z with 
                                       | B true  -> run rest (y :: s') env 
                                       | B false -> run rest (x :: s') env 
                                       | _       -> bad rest)
  
  | (Pop      :: rest, _ :: s'   ) -> run rest s' env
  
  | (Swap     :: rest, x ::y ::s') -> run rest (y::x::s') env
  
  | (Bind     :: rest, N n::x::s') -> (match res x with
                                       | E   -> bad rest
                                       | N _ -> bad rest (* if the variable is unbound we get an error *)
                                       | sv  -> run rest (U :: s') (insert n sv env))
  
  | (Block ls :: rest, s'        ) -> let (top :: _),env' = run ls [] env (* an exception will be thown if the resulting stack is empty *)
                                      in  run rest (top :: s') env

  | (Def (name, param, commands,b) :: rest, s') -> (match (res (N name), res (N param)) with
                                                | (_, E) -> bad rest
                                                | (E, _) -> bad rest
                                                | (x, y) -> run rest (U::s') (insert name (Func(name, param, commands, env,b)) env))

  | (Call ::rest, sv :: func :: s') -> (match (res func, res sv) with 
                                                | (_, E) -> bad rest
                                                | (InOutFunc (name, param, commands, env',b'), sv') -> let (ret::_,env'') = run commands stack (insert param (sv') env') in
                                                      let N (n) = sv in (match (res ret) with 
                                                      | U -> let Some param_val = (fetch param env'') in run rest (s') (insert n param_val env)
                                                      | E -> let Some param_val = (fetch param env'') in run rest (s') (insert n param_val env)
                                                      | _ -> let Some param_val = (fetch param env'') in if b' then run rest (ret::s') (insert n param_val env) else run rest (s') (insert n param_val env))
                                                | ((Func (name, param, commands, env',b')), sv') -> let (rel::_,env'') = run commands stack (insert param (sv') env') in 
                                                                        if b' then run rest (rel :: s') env else run rest (s') env
                                                | _ -> bad rest)

  | (InOutDef (name, param, commands,b) :: rest, s') -> (match res (N name), res (N param) with
                                                      | (_, E) -> bad rest
                                                      | (E, _) -> bad rest
                                                      | (x, y) -> run rest (U::s') (insert name (InOutFunc (name, param, commands, env, b)) env))

  | (Return :: rest, x :: s') -> run rest (res x :: s') env

  | (_        :: rest, _         ) -> bad rest

  
(* remember to test! *)
(* let e1 = run [PushI 1; PushI 2; PushI 3] [] empEnv
let e2 = run [PushB true; PushB false] [] empEnv
let e3 = run [PushI 3; PushI 8; Neg; Add; PushI 6; Sub; Div; Pop; PushI 7; Mul; PushI 100; Rem; Pop; PushN "is_this_ok"] [] empEnv
let e4 = run [PushI 1; PushI 1; Add] [] empEnv
let e5 = run [PushN "x"; PushI 1; Add] [] (insert "x" (SV(I 7)) empEnv)
let e6 = run [PushN "x"; PushI 1; Add] [] empEnv
let e7 = run [PushI 500; PushI 2; Mul; PushI 2;  Div] [] empEnv
let e8 = run [PushS "world!"; PushS "hello "; Concat] [] empEnv
let e9 = run [PushI 7; PushI 8; LessThan] [] empEnv
let e10 = run [PushI 7; PushI 7; Equal] [] empEnv
let e11 = run [PushI 13; PushN "a"; Bind; PushI 3; PushN "name1"; Bind;  PushN "a"; PushN "name1"; Add] [] empEnv
let e12 = run [PushB true; PushS "o"; PushS "j"; If] [] empEnv
let e13 = run [Def ("identity", "x",[PushN "x";Return]); PushN "identity"; PushI 1; Call] [] empEnv
let e14 = run [Def ("identity", "x",[PushN "x";Return]); PushI 1;PushN "x"; Bind; PushN "identity";PushN "x";Call] [] empEnv
let e15 = run [PushI 3;PushN "x";Bind;Def ("addX","arg",[PushN "x";PushN "arg";Add;Return]);PushI 5;PushN "x";Bind;PushI 3;PushN "a";Bind;PushN "addX";PushN "a";Call] [] empEnv
let e16 = run [Block [Def ("identity", "x",[PushN "x";Return])]; PushN "identity"; PushI 1; Call] [] empEnv
let e17 = run [Def ("identity", "x",[Block [PushN "x"];Return]);PushN "identity"; PushI 1; Call] [] empEnv
let e18 = run [InOutDef ("addOne","x",[PushN "x";PushI 1;Add;PushN "x";Bind;PushN "x";Return]);PushI 1;PushN "a";Bind;PushN "addOne";PushN "a";Call;PushN "a";PushI 1;Add] [] empEnv *)
let test1 = run [Def ("fun2","a",[PushN "a";PushN "a";Sub;Return],true);Def ("fun3","a",[PushI 20;PushN "d";Bind;PushI 23;PushN "a";Sub;PushN "d";Add;Return],true);PushN "fun3";PushI 7;Call;PushN "fun2";PushI 5;Call;Sub] [] empEnv
(* ... *)


(* writing *)
let to_string (s: stackVal) : string = 
  match s with
  | I i -> string_of_int i 
  | S s  -> s
  | N n -> n
  | B b -> "<" ^ string_of_bool b ^ ">"
  | U   -> "<unit>"
  | E   -> "<error>"

  
(* parsing *)
(* helper functions *)
let explode (s: string) : char list =
  let rec expl i l =
    if i < 0 
    then l 
    else expl (i - 1) (String.get s i :: l)
  in expl (String.length s - 1) []

let implode (cl: char list) : string = 
  String.concat "" (List.map (String.make 1) cl)

 
let is_alpha (c: char): bool = 
  (Char.code 'a' <= Char.code c && Char.code c <= Char.code 'z')
  || (Char.code 'A' <= Char.code c && Char.code c <= Char.code 'Z')

let not_space (c: char) : bool = 
  if c = ' ' then false else true

let is_digit (c: char): bool = 
 Char.code '0' <= Char.code c && Char.code c <= Char.code '9'

let rec take_while' (p: 'a -> bool) (es: 'a list) : ('a list) * ('a list) = 
  match es with
  | []      -> ([],[])
  | x :: xs -> if p x then let (chars, rest) = take_while' p xs in  (x :: chars, rest) else ([],es)

let take_while (p: char -> bool) (s: string) : string * string = 
  let (echars, erest) = take_while' p (explode s) 
  in (implode echars, implode erest)

let parse_int (s: string) : int option = 
    match int_of_string s with    
    | n           -> Some n
    | exception _ -> None

let parse_string (s: string) : string option = 
    if String.length s > 1 && String.get s 0 = '"' && String.get s (String.length s - 1) = '"'
    then Some (String.sub s 1 (String.length s - 2)) (* this is less restrictive then the spec *)
    else None


let parse_name (s: string) : string option = 
    if String.length s > 0 && ( let c = (String.get s 0) in is_alpha c || c = '_')
    then Some s (* this is less restrictive then the spec *)
    else None

let rec parse_command (ls: string list) : command  * (string list)  = 
    let s :: rest = ls in
    match take_while is_alpha (String.trim s) with
    | ("PushI"   , p)  -> let Some i = parse_int (String.trim p)    in (PushI i, rest)
    | ("PushS"   , p)  -> let Some s = parse_string (String.trim p) in (PushS s, rest)
    | ("PushN"   , p)  -> let Some n = parse_name (String.trim p)   in (PushN n, rest)
    | ("PushB"   , p)  -> (match String.trim p with "<true>" -> (PushB true, rest) | "<false>" -> (PushB false, rest))
    | ("Push"    , p)  -> (match String.trim p with "<unit>" -> (PushU, rest) )  
    | ("Add"     , "") -> (Add, rest)
    | ("Sub"     , "") -> (Sub, rest)
    | ("Mul"     , "") -> (Mul, rest)
    | ("Div"     , "") -> (Div, rest)
    | ("Rem"     , "") -> (Rem, rest)
    | ("Neg"     , "") -> (Neg, rest)
    | ("Pop"     , "") -> (Pop, rest)
    | ("Swap"    , "") -> (Swap, rest)
    | ("Concat"  , "") -> (Concat, rest)
    | ("And"     , "") -> (And, rest)
    | ("Or"      , "") -> (Or, rest)
    | ("Not"     , "") -> (Not, rest)
    | ("LessThan", "") -> (LessThan, rest)
    | ("Equal"   , "") -> (Equal, rest)
    | ("If"      , "") -> (If, rest)
    | ("Bind"    , "") -> (Bind, rest)
    | ("Begin"   , "") -> 
      let (cmds, rest') = parse_until_end rest
      in (Block cmds, rest')
    | ("Fun", p) -> (match take_while not_space (String.trim p) with
                    | (name1, name2) -> let (cmds, rest',b) = parse_until_FunEnd rest in (Def (String.trim name1, String.trim name2, cmds,b), rest'))
    | ("Call", "") -> (Call, rest)
    | ("Return", "") -> (Return, rest)
    | ("InOutFun", p) -> (match take_while not_space (String.trim p) with
                        | (name1, name2) -> let (cmds, rest',b) = parse_until_FunEnd rest in (InOutDef (String.trim name1, String.trim name2, cmds,b), rest'))
    (* any incorrect commands will result in an exception *)
and parse_until_end (ls: string list) :  (command list) * (string list)  =  
    match ls with 
    | "End"    :: tl -> ([], tl)
    | ls             -> 
      let    (cmd, rest')   = parse_command ls 
      in let (cmds, rest'') = parse_until_end rest'
      in (cmd :: cmds, rest'')

and parse_until_FunEnd (ls : string list) : (command list) * (string list) * bool = 
    match ls with
    | "Return" :: tl -> let s:: rest = tl in ([Return], rest, true)
    | "FunEnd" :: tl -> ([], tl, false)
    | ls -> let (cmd, rest') = parse_command ls
    in let (cmds, rest'',b) = parse_until_FunEnd rest'
    in (cmd :: cmds, rest'', b)
	
(* remember to test! *)
(* let pe0 = parse_command  ["PushB <true>"]
let pe1 = parse_command  ["PushB <false>"]
let pe2 = parse_command  ["PushN Name"]
(* ... *)
let pe3 = parse_until_end ["Pop";"Pop";"End";"Pop";"Pop"]
let pe4 = parse_until_end ["Begin"; "Pop";"Pop";"End";"Pop";"End"; "Neg"; "Neg"]
let pe5 = parse_until_end ["Begin"; "Pop";"End";"Begin"; "Add";"End";"Neg";"End"]
let pe6 = parse_until_end ["Begin"; "Begin"; "Begin"; "Begin"; "Pop";"End";"End";"End";"End";"End"] *)
(* ... *)


let rec parser (ls : string list) : command list =
  match ls with
  | []  -> []
  | ls -> let (cmd, rest') = parse_command ls in cmd :: parser rest' 

(* remember to test! *)
(* let pe7 = take_while' (fun s -> (String.trim s) <> "Quit") ["PushN Name"; "Quit"] *)
(* ... *)


(* file IO *)

(* from lab 2 *)
let read_lines (path: string) : string list = 
  let ch = open_in path in
  let ls = (let rec loop () : string list = 
                 match input_line ch with
                   | s -> s :: loop ()
                   | exception  End_of_file -> [] (* input_line throws an exception at the end of the file *)
               in loop () )
  in let () = close_in ch
  in ls


(* from lab 2 *)
let rec write_lines (path : string) (ls : string list ) : unit =
  let rec loop ch (ls : string list) : unit =
    match ls with
      | [] -> ()
      | x :: xs -> let _ = Printf.fprintf ch "%s\n" x in loop ch xs
  in 
  let ch = open_out path in
  let () = loop ch ls in
  let () = close_out ch in
  ()


(* run the interperter on the commands in inputFile and write the resulting stack in outputFile *)
let interpreter (inputFile : string) (outputFile : string) : unit =
  let (lines_in, _) = take_while' (fun s -> (String.trim s) <> "Quit") (read_lines inputFile) in (* keeps only syntax before Quit *)
  
  let commands = parser lines_in in
  let stack,env = run commands [] empEnv in
  let lines_out = List.map to_string stack in
  
  let _ = write_lines outputFile lines_out in ()
