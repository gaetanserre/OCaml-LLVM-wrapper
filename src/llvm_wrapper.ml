module Llvm = struct
  type register = string

  type llvm_program = {
      f_name: string;
      global_strings: string list;
      mutable funcs: llvm_function list
    }

  and llvm_function = {
    name:         string;
    r_type:       llvm_type;
    params:       (llvm_type * string) list;
    mutable code: llvm_statement list
  }

  and llvm_statement =
    | Assign of register * llvm_expression
    | Store  of llvm_type * register * llvm_type * llvm_expression
    | Return of llvm_type * llvm_expression
    | SetArray of llvm_type * string * int * llvm_expression * llvm_type

  and binop = Add | Mul | Sub | Eq | Gt | Ge | Lt | Le | Or | And

  and unop = Neg | Not
  
  and llvm_expression =
    | Cst   of int
    | Var   of register
    | Param of int
    | Alloc of llvm_type
    | Load  of llvm_type * llvm_type * register
    | Unop  of unop * llvm_expression
    | Binop of binop * llvm_type * llvm_expression * llvm_expression
    | GetArray of llvm_type * register * int
    | Call  of string * llvm_type * (llvm_type * llvm_expression) list
    | CallPointer of register * llvm_type * (llvm_type * llvm_expression) list
  
  and llvm_type =
    | Void
    | I1
    | I8
    | I32
    | Array of int * llvm_type
    | Pointer of llvm_type
    | FPointer of llvm_type * llvm_type list

  let create_program f_name: llvm_program =
    {f_name; global_strings = []; funcs = []}
  
  let add_function llvm_prog llvm_fun =
    llvm_prog.funcs <- llvm_fun :: llvm_prog.funcs
  
  let create_function name r_type params: llvm_function =
    {name; r_type; params; code = []}

  let create_main_function (): llvm_function =
    {name = "@main"; r_type = I32; params = []; code = []}
  
  let add_code llvm_fun llvm_stmt =
    llvm_fun.code <- llvm_stmt :: llvm_fun.code


  (**
    Builds the LLVM IR program and stores it
    it in the file given at its creation
  *)
  let build_llvm_program llvm_prog =
    let rec tr_type t =
      let rec tr_type_params acc tp =
        match tp with
          | [] -> acc
          | [t] -> tr_type t
          | t::tp ->
            let tp = tr_type_params acc tp in
            (tr_type t) ^ " " ^ tp
      in

      match t with
        | Void -> "void"
        | I1 -> "i1"
        | I8 -> "i8"
        | I32 -> "i32"
        | Array (size, t) -> Printf.sprintf "[%d x %s]" size (tr_type t)
        | Pointer t -> Printf.sprintf "%s*" (tr_type t)
        | FPointer (ret_type, p_types) ->
          Printf.sprintf "%s (%s)*" (tr_type ret_type) (tr_type_params "" p_types)
    in

    let tr_unop u =
      match u with
        | Neg -> "sub i32 0"
        | Not -> "xor i1 true"
    in
    
    let tr_binop b =
      match b with
        | Add -> "add"
        | Sub -> "sub"
        | Mul -> "mul"
        (* TODO *)
        | _ -> assert false
    in

    let rec tr_params acc f el =
      match el with
        | [] -> ""
        | [(t, e)] -> (tr_type t) ^ " " ^ (f e)
        | (t, e)::el ->
          let p = tr_params acc f el in
            (tr_type t) ^ " " ^ (f e) ^ ", " ^ p
    in

    let rec tr_expr llvm_expr =
      let tr_call t fname tel =
        let params = tr_params "" tr_expr tel in
        Printf.sprintf "call %s %s(%s)" (tr_type t) fname params
      in

      match llvm_expr with
        | Cst i -> Printf.sprintf "%d" i
        | Var s -> Printf.sprintf "%s" s
        | Param i -> Printf.sprintf "%d" i
        | Alloc t -> Printf.sprintf "alloca %s" (tr_type t)
        | Load (t1, t2, register) ->
          Printf.sprintf "load %s, %s %s" (tr_type t1) (tr_type t2) register
        | Unop (u, e) ->
          Printf.sprintf "%s, %s" (tr_unop u) (tr_expr e)
        | Binop (b, t, e1, e2) ->
          Printf.sprintf "%s %s %s, %s" (tr_binop b) (tr_type t) (tr_expr e1) (tr_expr e2)
        | GetArray (t, s, idx) ->
          Printf.sprintf "getelementptr inbounds %s, %s %s, i64 0, i64 %d"
          (tr_type t) (tr_type (Pointer t)) s idx
        | Call (fname, t, tel) -> tr_call t fname tel
        | CallPointer (register, t, tel) -> tr_call t register tel
    in

    let rec tr_instr llvm_instr =
      match llvm_instr with
        | Assign (dest, value) ->
          Printf.sprintf "%s = %s\n" dest (tr_expr value)
        | Store (t_dest, dest, t_value, value) ->
          Printf.sprintf "store %s %s, %s %s\n"
          (tr_type t_value) (tr_expr value)
          (tr_type t_dest) dest
        | Return (t, e) -> Printf.sprintf "ret %s %s\n" (tr_type t) (tr_expr e)
        | SetArray (t, s, idx, e, te) ->
          (* FIXME *)
          Printf.sprintf "%s%s"
          (tr_instr (Assign("%tmp", GetArray (t, s, idx))))
          (tr_instr (Store(Pointer te, "%tmp", te, e)))
    in
    
    let tr_function llvm_fun =
      let code_str =
        List.fold_right (fun i acc -> acc ^ (tr_instr i)) llvm_fun.code "" in
      let params_type = tr_params "" (fun s -> Printf.sprintf "%s" s) llvm_fun.params in

      Printf.sprintf "define %s %s(%s) {\n%s}"
        (tr_type llvm_fun.r_type) llvm_fun.name params_type code_str
    in

  (* TODO *)
  let tr_gloabl_string s =
    let res = ref [] in
    String.iter (fun c ->
      let c = String.make 1 c in
      let c = if c = "\n" then "\\0A" else c in
      res := c :: !res)
    s;
    List.fold_right (fun s acc -> acc ^ s) !res "" ^ "\\00"
  in

    let code = List.fold_right
                (fun f acc -> acc ^ (tr_function f) ^ "\n") 
                llvm_prog.funcs "" in
    
    let oc = open_out llvm_prog.f_name in
    Printf.fprintf oc "%s" code;
    close_out oc;

end