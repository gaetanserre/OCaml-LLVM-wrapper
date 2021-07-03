open Llvm_utils

let create_add_function llmod = 
  let addty = Function.get_function_type Type.i32 [|Type.i32; Type.i32|] in
  let add_fun = Function.define_function addty "add" llmod in
  let add_builer = Function.get_function_builder add_fun in
  let res = Expr.add (Function.get_params add_fun 0) (Function.get_params add_fun 1) add_builer in
  let _ = Stmt.return res add_builer in
  add_fun

let create_main_function llmod = 
  let mainty = Function.get_function_type Type.i32 [||] in
  let main = Function.define_function mainty "main" llmod in
  Function.get_function_builder main

let () =
  let llmod = Utils.get_module "" in

  let add_fun = create_add_function llmod in
  let main_builder = create_main_function llmod in

  let a = Expr.var ~name:"a" Type.i32 main_builder in
  let b = Expr.var ~name:"b" Type.i32 main_builder in

  let s = Expr.glob_str ~name:"s" "Type number 1:\n" main_builder in
  let _ = Stmt.call (Primitives.printf llmod) [|s|] main_builder in
  let s = Expr.glob_str ~name:"s" "%d" main_builder in
  let _ = Stmt.call (Primitives.scanf llmod) [|s; a|] main_builder in

  let s = Expr.glob_str ~name:"s" "Type number 2:\n" main_builder in
  let _ = Stmt.call (Primitives.printf llmod) [|s|] main_builder in
  let s = Expr.glob_str ~name:"s" "%d" main_builder in
  let _ = Stmt.call (Primitives.scanf llmod) [|s; b|] main_builder in

  let s = Expr.glob_str ~name:"s" "Result: %d\n" main_builder in
  let res = Stmt.call add_fun [|Expr.load_pointer a main_builder; Expr.load_pointer b main_builder|] main_builder in
  let _ = Stmt.call (Primitives.printf llmod) [|s; res|] main_builder in

  let _ = Stmt.return (Expr.zero) main_builder in
  
  try
    Writer.module_to_llm llmod (Sys.argv.(1))
  with Invalid_argument _s -> Printf.printf "%s\n" (Reader.module_to_string llmod)
