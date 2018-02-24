(* $Id: maindc.ml,v 1.5 2017-04-07 13:24:41-07 - - $ *)

include Scanner
include Bigint

open Bigint
open Printf
open Scanner

type stack_t = Bigint.bigint Stack.t
let push = Stack.push
let pop = Stack.pop

let aget = Array.get
let aset = Array.set
let amake = Array.make

type command = Load of char | Store of char * Bigint.bigint

let symbol_table = amake 256 (false, Bigint.zero)

let ord thechar = int_of_char thechar
type binop_t = bigint -> bigint -> bigint

let aget = Array.get
let aset = Array.set
let amake = Array.make

(*
let do_command = function
    | Load sym -> (
         let entry = aget symbol_table (ord sym)
         in match entry with
            | false, _ -> printf "register '%c' is empty\n%!" sym
            | true, value ->  push value stack_t
      )
    | Store (sym, value) -> (
         aset symbol_table (ord sym) (true, value);
         printf "%c := %g\n%!" sym value
      )
*)

let rec print_number number = 
   let s = string_of_bigint number in
   if (String.length s) < 70 
       then printf "%s\n%!" s
   else
       (printf "%s\\\n%!" (String.sub s 0 69);
       print_number (bigint_of_string (String.sub s 69 
                                          ((String.length s) - 69))) )

let print_stackempty () = printf "dc: stack empty\n%!"

let executereg (thestack: stack_t) (oper: char) (reg: int) =
    try match oper with
        | 'l' -> (
            let entry = aget symbol_table reg
            in match entry with
            | false, _ -> printf "register '%c' is empty\n%!" 
                           (char_of_int reg)
            | true, value -> push value thestack
        )
        | 's' -> 
            aset symbol_table reg (true,  (pop thestack));
        | _   -> printf "%c %c is unimplemented\n%!" 
                oper (char_of_int reg)
    with Stack.Empty -> print_stackempty()

let executebinop (thestack: stack_t) (oper: binop_t) =
    try let right = pop thestack
        in  try let left = pop thestack
                in  push (oper left right) thestack
            with Stack.Empty -> (print_stackempty ();
                                 push right thestack)
    with Stack.Empty -> print_stackempty ()

let execute (thestack: stack_t) (oper: char) =
    try match oper with
        | '+'  -> executebinop thestack Bigint.add
        | '-'  -> executebinop thestack Bigint.sub
        | '*'  -> executebinop thestack Bigint.mul
        | '/'  -> executebinop thestack Bigint.div
        | '%'  -> executebinop thestack Bigint.rem
        | '^'  -> executebinop thestack Bigint.pow
        | 'c'  -> Stack.clear thestack
        | 'd'  -> push (Stack.top thestack) thestack
        | 'f'  -> Stack.iter print_number thestack
        | 'l'  -> failwith "operator l scanned with no register"
        | 'p'  -> print_number (Stack.top thestack)
        | 'q'  -> raise End_of_file
        | 's'  -> failwith "operator s scanned with no register"
        | '\n' -> ()
        | ' '  -> ()
        | _    -> printf "0%o is unimplemented\n%!" (ord oper)
    with Stack.Empty -> print_stackempty()

let toploop (thestack: stack_t) inputchannel =
    let scanbuf = Lexing.from_channel inputchannel in
    let rec toploop () = 
        try  let nexttoken = Scanner.scanner scanbuf
             in  (match nexttoken with
                 | Number number       -> push number thestack
                 | Regoper (oper, reg) -> executereg thestack oper reg
                 | Operator oper       -> execute thestack oper
                 );
             toploop ()
        with End_of_file -> printf "";
    in  toploop ()

let readfiles () =
    let thestack : bigint Stack.t = Stack.create ()
    in  ((if Array.length Sys.argv > 1 
         then try  let thefile = open_in Sys.argv.(1)
                   in  toploop thestack thefile
              with Sys_error message -> (
                   printf "%s: %s\n%!" Sys.argv.(0) message;
                   exit 1));
        toploop thestack stdin)

let interact () =
    let thestack : bigint Stack.t = Stack.create ()
    in  toploop thestack stdin

let _ = if not !Sys.interactive then readfiles ()

