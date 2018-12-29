let output = ref [];;
let count = ref 0;;
let ex_bool = ref false;;
let term_bool = ref false;;
let w_count = ref 0;;
let if_count = ref 0;;
let else_count = ref 0;;
let else_lab = ref "";;
let local_count = ref 0;;
let param_list = ref [];;
let param_count = ref 0;;
let label_count = ref 0;;
let notjust_main = ref false;;
let state_list = ref [];;
open Printf;;

let symbtable = Hashtbl.create 20000;;
let para_tab = Hashtbl.create 20000;;
type token = Identifier of string
           | Void | Int
           | LeftParen | RightParen
           | Comma | SemiColon
           | Number of string
           | AddOp | SubOp
           | Assign | MulOp
           | DivOp | If | Else | While
           | Main | Read | Write
           | Continue | Printf 
           | Meta of string
           | Str of string
           | Scanf | Return 
           | Break | LeftBracket
           | RightBracket | Equals
           | DoubleEquals | GreaterThan
           | LessThan | GreaterEqual
           | LessEqual | NotEquals | Not
           | And | DoubleAnd | Or | DoubleOr
           | LeftBrace | RightBrace | Period | EndofFile


exception Er of string

let read_char file = try Some (input_char file) with End_of_file -> None 

let file_lines filename = 
  let rec file_line_ext file lines = match (read_char file) with 
    | None -> List.rev lines
    | Some s -> file_line_ext file (s :: lines) in 
  file_line_ext (open_in filename) [];;

let rec combine_str (l, tok) = match (l, tok) with
| ([], t) -> ([], t)
| ('"'::h, t) -> (h, t^Char.escaped '"')
| ('\\'::h, t) -> (match h with
		|'n'::rest -> combine_str (rest, t^(Char.escaped '\n'))
		|'r'::rest -> combine_str (rest, t^(Char.escaped '\r'))
		|_ -> raise (Er "Error in scanning"))
| (x::h, t) -> begin 
		combine_str (h, t^Char.escaped x)
	end;;

let is_letter c =  let code = Char.code c in
                 (code >= Char.code('A') && code <= Char.code('Z')) ||
                 (code >= Char.code('a') && code <= Char.code('z'))
let is_digit c = let code = Char.code c in
                 code >= Char.code('0') && code <= Char.code('9');;

let rec combine_chars (l, st) = match (l, st) with
| ([], t) -> (l, t)
| ('_'::h, t) -> combine_chars (h, t^"_")
| (x::h, t) -> if (is_letter x) = true then combine_chars (h, t^Char.escaped x)
		else if (is_digit x) = true then combine_chars (h, t^Char.escaped x)
		else (x::h, t)
|_ -> (l, st);;

let rec combine_digs (l, st) = match (l, st) with
|([], t) -> (l, t)
|(x::h, t) -> if (is_digit x) = true then combine_digs (h, t^Char.escaped x)
		else (x::h, t)
|_ -> (l, "");;

let rec get_meta (l, st) = match (l, st) with
| ([], t) -> ([], t)
| ('\\'::h, t) -> (match h with
		|'n'::rest -> get_meta (rest, t^(Char.escaped '\n'))
		|'r'::rest -> get_meta (rest, t^(Char.escaped '\r'))
		|_ -> raise (Er "Error in scanning"))
| ('\n'::h, t) -> (h, t^"\n")
| ('\r'::h, t) -> get_meta (h,t)
| (x::h, t) -> get_meta (h, t^Char.escaped x);;

let rec scan lines tokens = match lines with
	| [] -> List.rev tokens
	| ' '::rest -> scan rest tokens
	| '\t'::rest -> scan rest tokens
	| '#'::rest -> (match (get_meta (rest, "#")) with
					|(body, "") -> scan body tokens
					|(body, tok) -> scan body ((Meta tok)::tokens)
					| (_,_)-> scan rest tokens
					)
	| '/'::'/'::rest -> (match (get_meta (rest, "//")) with
					|(body, "") -> scan body tokens
					|(body, tok) -> scan body ((Meta tok)::tokens)
					| (_,_)-> scan rest tokens
					)
	| '\r'::rest -> scan rest tokens
	| '\n'::rest -> scan rest tokens
	| '('::rest -> scan rest (LeftParen::tokens)
	| '='::rest -> (match rest with
				| '='::h -> scan h (DoubleEquals::tokens)
				|_ -> scan rest (Equals::tokens))
	| '!'::rest -> (match rest with
					| '='::h -> scan h (NotEquals::tokens)
					| _ -> scan rest (Not::tokens) 
					)
	| '>'::rest -> (match rest with
					| '='::h -> scan h (GreaterEqual::tokens)
					| _ -> scan rest (GreaterThan::tokens))
	| '<'::rest -> (match rest with
					| '='::h -> scan h (LessEqual::tokens)
					| _ -> scan rest (LessThan::tokens))
	| '-'::rest -> scan rest (SubOp::tokens)
	| '+'::rest -> scan rest (AddOp::tokens)
	| '/'::rest -> scan rest (DivOp::tokens)
	| ')'::rest -> scan rest (RightParen::tokens)
	| '{'::rest -> scan rest (LeftBrace::tokens)
	| '}'::rest -> scan rest (RightBrace::tokens)
	| '*'::rest -> scan rest (MulOp::tokens)
	| ','::rest -> scan rest (Comma::tokens)
	| '.'::rest -> scan rest (Period::tokens)
	| ';'::rest -> scan rest (SemiColon::tokens)
	| '|'::rest -> scan rest (Or::tokens)
	| '|'::'|'::rest -> scan rest (DoubleOr::tokens)
	| '&'::rest -> scan rest (And::tokens)
	| '&'::'&'::rest -> scan rest (DoubleAnd::tokens)
	| 'a'::rest -> (match (combine_chars (rest, "a")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'b'::rest -> (match (combine_chars (rest, "b")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> if tok = "break" then scan body (Break::tokens) 
						else scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'c'::rest -> (match (combine_chars (rest, "c")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> if tok = "continue" then scan body (Continue::tokens)
						else scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'd'::rest -> (match (combine_chars (rest, "d")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'e'::rest -> (match (combine_chars (rest, "e")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> if tok = "else" then scan body (Else::tokens)
						else scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'f'::rest -> (match (combine_chars (rest, "f")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'g'::rest -> (match (combine_chars (rest, "g")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'h'::rest -> (match (combine_chars (rest, "h")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'i'::rest -> (match (combine_chars (rest, "i")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> if tok = "if" then scan body (If::tokens)
						else if tok = "int" then scan body (Int::tokens)
						else scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'j'::rest -> (match (combine_chars (rest, "j")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'k'::rest -> (match (combine_chars (rest, "k")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'l'::rest -> (match (combine_chars (rest, "l")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'm'::rest -> (match (combine_chars (rest, "m")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> if tok = "main" then scan body (Main::tokens) 
						else scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'n'::rest -> (match (combine_chars (rest, "n")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'o'::rest -> (match (combine_chars (rest, "o")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'p'::rest -> (match (combine_chars (rest, "p")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> if tok = "printf" then scan body (Printf::tokens) 
						else scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'q'::rest -> (match (combine_chars (rest, "q")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'r'::rest -> (match (combine_chars (rest, "r")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> if tok = "read" then scan body (Read::tokens)
						else if tok = "return" then scan body (Return::tokens) 
						else scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 's'::rest -> (match (combine_chars (rest, "s")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 't'::rest -> (match (combine_chars (rest, "t")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> if tok = "scanf" then scan body (Scanf::tokens) 
						else scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'u'::rest -> (match (combine_chars (rest, "u")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'v'::rest -> (match (combine_chars (rest, "v")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> if tok = "void" then scan body (Void::tokens) 
						else scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'w'::rest -> (match (combine_chars (rest, "w")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> if tok = "while" then scan body (While::tokens)
						else if tok = "write" then scan body (Write::tokens)
						else scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'x'::rest -> (match (combine_chars (rest, "x")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'y'::rest -> (match (combine_chars (rest, "y")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'z'::rest -> (match (combine_chars (rest, "z")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'A'::rest -> (match (combine_chars (rest, "A")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'B'::rest -> (match (combine_chars (rest, "B")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'C'::rest -> (match (combine_chars (rest, "C")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'D'::rest -> (match (combine_chars (rest, "D")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'E'::rest -> (match (combine_chars (rest, "E")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'F'::rest -> (match (combine_chars (rest, "F")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'G'::rest -> (match (combine_chars (rest, "G")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'H'::rest -> (match (combine_chars (rest, "H")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'I'::rest -> (match (combine_chars (rest, "I")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'J'::rest -> (match (combine_chars (rest, "J")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'K'::rest -> (match (combine_chars (rest, "K")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'L'::rest -> (match (combine_chars (rest, "L")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'M'::rest -> (match (combine_chars (rest, "M")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'N'::rest -> (match (combine_chars (rest, "N")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'O'::rest -> (match (combine_chars (rest, "O")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'P'::rest -> (match (combine_chars (rest, "P")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'R'::rest -> (match (combine_chars (rest, "Q")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'S'::rest -> (match (combine_chars (rest, "R")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'T'::rest -> (match (combine_chars (rest, "S")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'U'::rest -> (match (combine_chars (rest, "T")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'V'::rest -> (match (combine_chars (rest, "U")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'W'::rest -> (match (combine_chars (rest, "V")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'X'::rest -> (match (combine_chars (rest, "W")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'Y'::rest -> (match (combine_chars (rest, "X")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| 'Z'::rest -> (match (combine_chars (rest, "")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Identifier tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| '0'::rest -> (match (combine_digs (rest, "0")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Number tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| '1'::rest -> (match (combine_digs (rest, "1")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Number tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| '2'::rest -> (match (combine_digs (rest, "2")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Number tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| '3'::rest -> (match (combine_digs (rest, "3")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Number tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| '4'::rest -> (match (combine_digs (rest, "4")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Number tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| '5'::rest -> (match (combine_digs (rest, "5")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Number tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| '6'::rest -> (match (combine_digs (rest, "6")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Number tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| '7'::rest -> (match (combine_digs (rest, "7")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Number tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| '8'::rest -> (match (combine_digs (rest, "8")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Number tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| '9'::rest -> (match (combine_digs (rest, "9")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body ((Number tok)::tokens)
						| (_,_)-> scan rest tokens
						)
	| '"'::rest -> (match (combine_str (rest, Char.escaped '"')) with
				|(body, "") -> scan body tokens
				|(body, tok) -> scan body ((Str tok)::tokens) 
				| (_,_)-> scan rest tokens)
	| '\n'::[] -> scan [] tokens
	| ' '::[] -> scan [] tokens
	| x::rest ->  scan rest tokens
	| x -> tokens
	| _::rest -> tokens
	| _ -> tokens
	| _::[] -> tokens
	;;

let start_scan filename = 
	scan (file_lines filename) [];;

let rec print_list l = match l with
|[] -> print_endline "end"
|(Identifier x)::rest -> begin print_endline x;
				print_list rest
			end
|Void::rest -> begin print_endline "void";
				print_list rest
			end
|Int::rest -> begin print_endline "int";
				print_list rest
			end
|LeftParen::rest -> begin print_endline "(";
				print_list rest
			end
|RightParen::rest -> begin print_endline ")";
				print_list rest
			end
|Comma::rest -> begin print_endline ",";
				print_list rest
			end
|SemiColon::rest -> begin print_endline ";";
				print_list rest
			end
|(Number x)::rest -> begin print_endline x;
				print_list rest
			end
|AddOp::rest -> begin print_endline "+";
				print_list rest
			end
|SubOp::rest -> begin print_endline "-";
				print_list rest
			end
|MulOp::rest -> begin print_endline "*";
				print_list rest
			end
|DivOp::rest -> begin print_endline "/";
				print_list rest
			end
|If::rest -> begin print_endline "if";
				print_list rest
			end
|Else::rest -> begin print_endline "else";
				print_list rest
			end
|While::rest -> begin print_endline "while";
				print_list rest
			end
|Main::rest -> begin print_endline "main";
				print_list rest
			end
|Read::rest -> begin print_endline "read";
				print_list rest
			end
|Write::rest -> begin print_endline "write";
				print_list rest
			end
|Continue::rest -> begin print_endline "continue";
				print_list rest
			end
|(Meta x)::rest -> begin print_endline x;
				print_list rest
			end
|(Str x)::rest -> begin print_endline x;
				print_list rest
			end
|Scanf::rest -> begin print_endline "scanf";
				print_list rest
			end
|Printf::rest -> begin print_endline "printf";
				print_list rest
			end
|Return::rest -> begin print_endline "return";
				print_list rest
			end
|Break::rest -> begin print_endline "break";
				print_list rest
			end
|LeftBrace::rest -> begin print_endline "{";
				print_list rest
			end
|RightBrace::rest -> begin print_endline "}";
				print_list rest
			end
|LeftBracket::rest -> begin print_endline "[";
				print_list rest
			end
|RightBracket::rest -> begin print_endline "]";
				print_list rest
			end
|Equals::rest -> begin print_endline "=";
				print_list rest
			end
|DoubleEquals::rest -> begin print_endline "==";
				print_list rest
			end
|GreaterThan::rest -> begin print_endline ">";
				print_list rest
			end
|GreaterEqual::rest -> begin print_endline ">=";
				print_list rest
			end
|LessThan::rest -> begin print_endline "<";
				print_list rest
			end
|LessEqual::rest -> begin print_endline "<=";
				print_list rest
			end
|NotEquals::rest -> begin print_endline "!=";
				print_list rest
			end
|Not::rest -> begin print_endline "!";
				print_list rest
			end
|And::rest -> begin print_endline "&";
				print_list rest
			end
|DoubleAnd::rest -> begin print_endline "&&";
				print_list rest
			end
|Or::rest -> begin print_endline "|";
				print_list rest
			end
|DoubleOr::rest -> begin print_endline "||";
				print_list rest
			end
|Period::rest -> begin print_endline ".";
				print_list rest
			end
;;


(* (match (get_meta (rest, "#")) with
						| (body, "") -> scan body tokens
						| (body, tok) -> scan body (::tokens)
						)*)

let tokens = Queue.create () ;;

let peekNext toks = if Queue.is_empty toks
			then EndofFile
			else
				Queue.peek toks;;

let next_tok toks = if Queue.is_empty toks
			then EndofFile
			else begin
				Queue.pop toks;
			end;;

let match_tok tok toks = if tok = (peekNext toks)
				then (next_tok toks)
			else if Queue.is_empty toks
			then EndofFile
			else
				raise (Er "Error wrong match")
;;

let print_label w = begin
	label_count := !label_count+1;
	state_list := ("label_"^(string_of_int !label_count)^":\n")::!state_list;
end;;

let rec params_output num lis = match lis with
|[] ->()
|x::h -> if num > 0 then begin
		output := ((Hashtbl.find symbtable x)^" = mem[base-3-"^(string_of_int num)^"];\n")::!output;
		params_output (num-1) h;
	end
|_ -> ()
;;

let rec do_cases num = if num < (!label_count+2) then
begin
	output := ("case "^(string_of_int num)^":\n")::!output;
	output := ("\tgoto label_"^(string_of_int num)^"; break;\n")::!output;
	do_cases (num+1)
end
;;

let create_jump w = begin
	output := "jumptable:\n"::!output;
	output := "switch(jumpreg) {\n"::!output;
	do_cases 1;
	output := "}\n"::!output;
end;;

let create_prologue "" = begin
	output := "base = top;\n"::!output;
	output :=( "top = base+3;\n")::!output;
end

let mem_Name word = begin
	count := !count + 1;
	local_count := !local_count+1;
	"mem["^"base+"^(string_of_int (!local_count-1))^"]"
end
;;

let def_base_top word = begin
		count := !count+1;
		"mem["^(string_of_int (!count-1))^"]"
end
;;

let assignment mem t1 op t2 = 
	let t = String.concat ""[mem; " = "; t1; op; t2; ";\n"] in
	begin
	state_list := t::!state_list;
end
;;

let rec caller_pro name num lis = match lis with
|[] ->()
|x::h -> if num < (Hashtbl.find para_tab name) then begin
		state_list := ("mem[top+"^(string_of_int num)^"] = "^"mem[base+"^(string_of_int num)^"];\n")::!state_list;
		caller_pro name (num+1) h;
	end
|_ -> ()
;;

let caller_prejump name = begin
	caller_pro name 0 !param_list;
	state_list := ("top = top + "^(string_of_int (Hashtbl.find para_tab name))^";\n")::!state_list;
	state_list := "mem[top] = base;\n"::!state_list;
	state_list := "mem[top+1] = 1;\n"::!state_list;
	state_list := "top = top + 3;\n"::!state_list;
end;;

let caller_postjump name = begin
		state_list := "base = mem[top-3];\n"::!state_list;
		state_list := "mem[base+2] = mem[top-1];\n"::!state_list;
		state_list := ("top = base + "^(string_of_int !local_count)^";\n")::!state_list;
end
;;

let rec expression toks = 
	let t = term toks in
	let e = expression_tail toks t in
	if e = "" then t 
	else e;
and expression_tail toks name = if (peekNext toks) = SubOp 
then begin
	match_tok SubOp toks;
	let t = term toks in
	let mem = (mem_Name "") in
	begin
		assignment mem name "-"  t;
		ex_bool := true;
		expression_tail toks mem
end
end
else if (peekNext toks) = AddOp
then begin
	match_tok AddOp toks;
	let t = term toks in
	let mem = (mem_Name "") in
	begin
		(assignment mem name "+"  t);
		ex_bool := true;
		expression_tail toks mem
end
end
else if !ex_bool = true then
	begin
		ex_bool := false;
		name
	end
else 
	"";

and term toks = begin
	let f = factor toks in
	let t = term_tail toks f in
	if t = "" then f
	else t;
end

and term_tail toks name = if (peekNext toks) = DivOp
then begin
	match_tok DivOp toks;
	let f = factor toks in
	let mem = (mem_Name "") in
	begin
		assignment mem name "/" f;
		term_bool := true;
		term_tail toks mem;
end
end
else if (peekNext toks) = MulOp 
then begin
	match_tok MulOp toks;
	let f = factor toks in
	let mem = (mem_Name "") in
	begin
		assignment mem name "*" f;
		term_bool := true;
		term_tail toks mem;
end
end 
else if !term_bool = true then
	begin
		term_bool := false;
		name
	end
else "";

and factor toks = match (next_tok toks) with
|Identifier x -> if (peekNext toks) = LeftParen then
	begin match_tok LeftParen toks;
	let ex = expr_list toks in
	match_tok RightParen toks;
	caller_prejump x;
	state_list := (Hashtbl.find symbtable x)::!state_list;
	print_label "";
	caller_postjump x;
	"mem[base+2]"
end
else if (peekNext toks) = LeftBracket 
then begin
	match_tok LeftBracket toks;
	expression toks;
	match_tok RightBracket toks;
	"";
end
else if (Hashtbl.find_opt symbtable x) = None
 then let mem = (mem_Name "") in 
 	begin
		Hashtbl.add symbtable x mem;
		mem
	end
else 
	Hashtbl.find symbtable x;
|(Number x) -> 	x
|SubOp -> match (next_tok toks) with 
		|(Number x) -> "-"^x;
		|_ -> raise (Er "Error need number for negation of number")
|LeftParen -> let mem = (mem_Name "") in
			let ex = expression toks in
			begin
				match_tok RightParen toks;
				String.concat ""[mem; " = "; ex; "\n"];
			end
|EndofFile -> ""
|_ -> raise (Er "Error in factor")

and expr_list toks = match (peekNext toks) with
|Identifier x -> non_empty_expr_list toks
|(Number x) -> non_empty_expr_list toks
|_ -> ""
and non_empty_expr_list toks = 
	let e = expression toks in
	let tail = expr_list_tail toks in
	String.concat ""[e; tail]
and expr_list_tail toks = if (peekNext toks) = Comma
then begin
	match_tok Comma toks;
	let e = expression toks in
	let tail = expr_list_tail toks in
	String.concat ""[","; e; tail]
end
else ""
;;

 let identifier_tail toks = match (peekNext toks) with
 |LeftBracket -> begin
 	match_tok LeftBracket toks;
 	expression toks;
 	match_tok RightBracket toks
 end
 |_ -> peekNext toks
;;

let comparison_op toks = match (peekNext toks) with
|DoubleEquals -> begin
	match_tok DoubleEquals toks;
	"=="
end
|NotEquals -> begin
	match_tok NotEquals toks;
	"!="
end
|GreaterThan -> begin
	match_tok GreaterThan toks;
	">"
end 
|GreaterEqual -> begin
	match_tok GreaterEqual toks;
	">="
end
|LessThan -> begin
	match_tok LessThan toks;
	"<"
end
|LessEqual -> begin
	match_tok LessEqual toks;
	"<="
end
|_ -> raise (Er "Error in comparison_op")
;;

let condition toks = 
	let e1 = expression toks in
	let op = comparison_op toks in
	let e2 = expression toks in
	e1^op^e2
;;


let rec condition_expression_tail toks = match (peekNext toks) with
|DoubleAnd -> begin
	match_tok DoubleAnd toks;
	let c = condition toks in
	let tail = condition_expression_tail toks in
	" && "^c^tail
end
|DoubleOr -> begin
	match_tok DoubleOr toks;
	let c = condition toks in
	let tail = condition_expression_tail toks in
	" || "^c^tail
end
|_ -> ""
;;


let condition_expression toks = begin
	let con = condition toks in
	let tail = condition_expression_tail toks in
	con^tail
end
;;

let rec write_statements lis = match lis with
|[] -> ()
|x::h -> begin
	output := x::!output;
	write_statements h;
end
|_ -> ();;

let rec statements toks = match (peekNext toks) with
|Printf -> begin
	print_func toks;
	statements toks
end
|Scanf -> begin
	scan_func toks;
	statements toks
end
|If -> begin
	if_statement toks;
	if (peekNext toks) = Else then
		statements toks
	else begin 
			state_list := (!else_lab^":;\n")::!state_list;
			else_lab := "";
			statements toks
		end
end
|While -> begin
	while_statement toks;
	statements toks
end
|(Identifier x) -> begin
	exp_statement toks;
	statements toks
end
|Read -> begin
	exp_statement toks;
	statements toks
end
|Write -> begin
	exp_statement toks;
	statements toks
end
|Return -> begin
	return_statement toks;
	statements toks
end
|Break -> begin
	break_statement toks;
	statements toks
end
|Else -> begin
	else_statement toks;
	statements toks
end
|Continue -> begin
	continue_statement toks;
	statements toks
end
|_ -> ()
and block_statments toks = match (next_tok toks) with
|LeftBrace -> begin
	statements toks;
	match_tok RightBrace toks;
end
|_ -> raise (Er "Error block statements")

and if_statement toks = match (next_tok toks) with
|If -> let i = "l_"^(string_of_int !if_count) in
	begin
	state_list := "if("::!state_list;
	match_tok LeftParen toks;
	let c = condition_expression toks in
	match_tok RightParen toks;
	state_list := (c^")"^"goto "^i^";\n")::!state_list;
	else_lab := "E_"^(string_of_int !else_count);
	else_count := !else_count+1;
	state_list := (" goto "^(!else_lab)^";\n")::!state_list;
	state_list := (i^":;\n")::!state_list;
	block_statments toks
end
|_ -> raise (Er "Error in if statement")


and else_statement toks = match (next_tok toks) with
|Else -> begin 
	state_list := (!else_lab^":;\n")::!state_list;
	else_lab := "";
	block_statments toks;
end
|_ -> raise (Er "Error in else statements")


and while_statement toks = match (next_tok toks) with
|While -> begin
	match_tok LeftParen toks;
	let c = condition_expression toks in
	let w = "w_"^(string_of_int !w_count) in
	w_count := !w_count + 1;
	match_tok RightParen toks;
	state_list := (w^":;\n")::!state_list;
	block_statments toks;
	state_list := ("if ("^c^")"^" goto "^w^";\n")::!state_list
end
|_ -> raise (Er "Error while statement")

and return_statement toks = match (next_tok toks) with
|Return -> return_tail toks;
|_ -> raise (Er "Error in return statement")

and return_tail toks = if (peekNext toks) = SemiColon
then begin 
		match_tok SemiColon toks;
		state_list := "return "::!state_list;
		state_list := ";"::!state_list;
	end
else let e = expression toks in
	begin
		match_tok SemiColon toks;
		state_list := "mem[base-1] ="::!state_list;
		state_list := e::!state_list;
		state_list := ";\n"::!state_list;
		state_list := "top = base;\n"::!state_list;
		if !notjust_main = true then
		begin
			state_list := "jumpreg = mem[base-2]; goto jumptable;\n"::!state_list;
		end
end

and break_statement toks = match (next_tok toks) with
|Break -> match_tok SemiColon toks
|_ -> raise (Er "Error break statement")


and continue_statement toks = match (next_tok toks) with
|Continue -> match_tok SemiColon toks
|_ -> raise (Er "error in continue statement")


and print_func_tail toks = match (peekNext toks) with
|RightParen -> begin 
	match_tok RightParen toks;
	state_list := ")"::!state_list;
	match_tok SemiColon toks;
	state_list := ";\n"::!state_list;
end
|Comma -> begin
	match_tok Comma toks;
	state_list := ","::!state_list;
	state_list := (expression toks)::!state_list;
	match_tok RightParen toks;
	match_tok SemiColon toks;
	state_list := ");\n"::!state_list;
end
|_ -> raise (Er "error in printf tail")


and print_func toks = match (next_tok toks) with
|Printf -> begin
	state_list := "printf("::!state_list;
	match_tok LeftParen toks;
	match (next_tok toks) with
	|(Str x) -> begin
		state_list := x::!state_list;
		print_func_tail toks;
		end
	|_ -> raise (Er "Error no string")
end
|_ -> raise (Er "Error in printf")

and scan_func toks = begin
	match_tok Scanf toks;
	match_tok LeftParen toks;
	match (next_tok toks) with
	|(Str x) -> begin
		state_list := ","::!state_list;
		match_tok Comma toks;
		match_tok And toks;
		state_list := "&"::!state_list;
		expression toks;
		match_tok SemiColon toks;
		state_list := ";"::!state_list;
	end
	|_ -> raise (Er "Error in scanf")
end

and assign_or_func toks = match (next_tok toks) with
|Equals -> let e = expression toks in
	begin
	match_tok SemiColon toks;
	e^";\n"
end
|LeftParen -> let e = expr_list toks in
	begin
	match_tok RightParen toks;
	match_tok SemiColon toks;
	"("^e^");\n";
end
|_ -> raise (Er "Error in assign_or_func")


and exp_statement toks = match (next_tok toks) with
|(Identifier x) -> 	if (Hashtbl.find_opt symbtable x) = None
	then let a = assign_or_func toks in
		let mem = (mem_Name "") in
	begin
		Hashtbl.add symbtable x mem;
		state_list := (String.concat ""[mem; " = "]; a)::!state_list;
	end
	else if (peekNext toks) = LeftParen then
	let a = assign_or_func toks in
		state_list := (Hashtbl.find symbtable x)::!state_list;
	else let a = assign_or_func toks in 
	begin
		state_list := (String.concat ""[(Hashtbl.find symbtable x); " = "; a])::!state_list;
	end
|Write -> let a = assign_or_func toks in
	begin
		state_list := "write"::!state_list;
		state_list := a::!state_list;
	end
|Read -> let a = assign_or_func toks in
	begin
		state_list := "read"::!state_list;
		state_list := a::!state_list;
	end
|_ -> raise (Er "Error in exp statement")
;;

let rec id_list_tail toks = if (peekNext toks) = Comma
		then begin
			match_tok Comma toks;
			match (next_tok toks) with
			|(Identifier x) -> let mems = (mem_Name "") in
					begin
						Hashtbl.add symbtable x mems;
						id_list_tail toks;
					end
			|_ -> raise (Er "error in id list tail")
		end
;;

let id_list toks = match (next_tok toks) with
|(Identifier x) -> let mem = (mem_Name "") in
	begin
		Hashtbl.add symbtable x mem;
		id_list_tail toks
	end
|_ -> raise (Er "Error in id list")
;;

let rec data_decls toks = match (peekNext toks) with
|Int -> begin
	match_tok Int toks;
	id_list toks;
	match_tok SemiColon toks;
	data_decls toks
end
|Void -> begin
	match_tok Void toks;
	id_list toks;
	match_tok SemiColon toks;
	data_decls toks
end
|_ -> ()
;;

let rec non_empty_list toks = if (peekNext toks) = Comma
then begin
	match_tok Comma toks;
	match (peekNext toks) with
	|Int -> begin
		match_tok Int toks;
		match (next_tok toks) with
		|(Identifier x) -> begin 
				Hashtbl.add symbtable x ("mem[base+"^(string_of_int !param_count)^"]");
				param_list := x::!param_list;
				non_empty_list toks
			end
		|_ -> raise (Er "Error")
		end
	|Void -> begin
		match_tok Void toks;
		match (next_tok toks) with
		|(Identifier x) -> non_empty_list toks
		|_ -> raise (Er "Error need identifier")
	end
	|_ -> raise (Er "Error in non_empty_list")
end
;;

let parameter_list_tail toks = match (peekNext toks) with
|(Identifier x) -> begin
	Hashtbl.add symbtable x ("mem[base+"^(string_of_int !local_count)^"]");
	local_count := !local_count+1;
	param_count := !param_count+1;
	param_list := x::!param_list;
	(next_tok toks);
	non_empty_list toks
end
|_ -> ()
;;

let parameter_list toks = if (peekNext toks) = Int
	then begin
	match_tok Int toks;
	parameter_list_tail toks
end
else if (peekNext toks) = Void
	then begin
	match_tok Void toks;
	parameter_list_tail toks
end
;;

let func_decl toks = match (next_tok toks) with
|Int -> (match (next_tok toks) with
	|(Identifier x) -> begin
		Hashtbl.add symbtable x ("goto "^x^"func;\n");
		output := (x^"func:\n")::!output;
		match_tok LeftParen toks;
		parameter_list toks;
		match_tok RightParen toks;
	end
	|Main -> begin
		Hashtbl.add symbtable "main" "goto mainfunc;\n";
		output := "mainfunc:\n"::!output;
		match_tok LeftParen toks;
		parameter_list toks;
		match_tok RightParen toks;
	end
	|_ -> raise (Er "Error not an identifier in func_decl");)
|Void->  (match (next_tok toks) with
	|(Identifier x) -> begin
		Hashtbl.add symbtable x ("goto "^x^"func;\n");
		output := (x^"func:\n")::!output;
		match_tok LeftParen toks;
		parameter_list toks;
		match_tok RightParen toks;
	end
	|Main -> begin
		Hashtbl.add symbtable "main" "goto mainfunc;\n";
		output := "mainfunc:\n"::!output;
		match_tok LeftParen toks;
		parameter_list toks;
		match_tok RightParen toks;
	end
	|_ -> raise (Er "Error not an identifier in func_decl"))
|_-> raise (Er "Error in func_decl")
;;

let func_follow toks = match (peekNext toks) with
|SemiColon -> begin match_tok SemiColon toks;
			output := ";\n"::!output;
end
|LeftBrace -> begin
	match_tok LeftBrace toks;
	data_decls toks;
	statements toks;
	create_prologue "";
	params_output !param_count (List.rev !param_list);
	write_statements (List.rev !state_list);
	state_list := [];
	match_tok RightBrace toks;
	()
end 
|_ -> raise (Er "Error in func_follow")
;;


let func toks = begin
	local_count := 0;
	param_count := 0;
	func_decl toks;
	func_follow toks	
end
;;

let rec func_list toks = match (peekNext toks) with
|Int -> begin
	func toks;
	func_list toks
end
|Void -> begin
	func toks;
	func_list toks
end
|_ -> ()
;;

let start_func toks name =
	if name = "main" then
	begin
		local_count := 0;
		param_count := 0;
		output := (name^"func"^":\n")::!output;
		Hashtbl.add symbtable name ("goto "^name^"func;\n");
		match_tok LeftParen toks;
		parameter_list toks;
		match_tok RightParen toks;
		func_follow toks;
		func_list toks
	end
else begin
	output := (name^"func"^":\n")::!output;
	Hashtbl.add symbtable name ("goto "^name^"func;\n");
	notjust_main := true;
	match_tok LeftParen toks;
	parameter_list toks;
	match_tok RightParen toks;
	Hashtbl.add para_tab name !param_count;
	func_follow toks;
	create_jump "";
	func_list toks
end;;

let beginning_data_decls toks name = match (next_tok toks) with
|Comma-> begin
	id_list toks
	end 
|SemiColon ->  let mem = (mem_Name "") in begin
	Hashtbl.add symbtable name mem;
	()
end
|_ -> raise (Er "Error in beginning data")
;;


let data_or_func toks name = match (peekNext toks) with
|Comma -> beginning_data_decls toks name
|SemiColon -> beginning_data_decls toks name
|LeftParen -> start_func toks name
|_ -> raise (Er "Error in data_or_func")
;;

let rec program_tail toks = match (peekNext toks) with
|Int -> begin
			match_tok Int toks;
			match (next_tok toks) with
			|Identifier x -> begin
				data_or_func toks x;
				program_tail toks
			end
			|Main -> begin
				data_or_func toks "main";
				program_tail toks 
			end
			|_ -> raise (Er "Need Identifier next")
		end
|Void -> begin
			output := "void "::!output;
			match_tok Void toks;
			match (next_tok toks) with
			|(Identifier x) -> begin
				data_or_func toks x;
				program_tail toks
			end
			|Main -> begin
				data_or_func toks "main";
				program_tail toks
			end
			|_ -> raise (Er "Need Identifier next")
		end
|_-> begin
	output := "};\n"::!output;
end
;;

let rec program toks = match (peekNext toks) with
|(Meta x) -> begin
	output := x::!output;
	next_tok toks;
	program toks
end
|Int ->	begin 
		output := ("#define jumpreg "^(def_base_top "")^"\n")::!output;
		output := "int mem["::!output;
		output := "];\n"::!output;
		output := "int base = 3;\n"::!output;
		output := "int top = 3;\n"::!output;
		output := "int main(){\n"::!output;
		output := "goto mainfunc;\n"::!output;
		program_tail toks
	end
|Void -> program_tail toks
|_ -> raise (Er "Error in program")
;;

let rec print_parse_list l = match l with
|[] -> ()
|"int mem["::rest -> begin
	Printf.printf "%s" "int mem[";
	Printf.printf "%s" (string_of_int (!count+3));
	print_parse_list rest;
end
|x::rest -> begin
	Printf.printf "%s" x;
	print_parse_list rest
end
|_ -> raise (Er "Error in printing")
;;

let addtoken a = Queue.add a tokens;;
List.map addtoken (start_scan Sys.argv.(1));;
program tokens;;
print_parse_list (List.rev !output);;



