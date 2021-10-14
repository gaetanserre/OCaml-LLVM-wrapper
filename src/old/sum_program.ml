open Llvm_utils

let create_sum_function llmod =
  let sum_ty = Function.get_function_type Type.void [|Type.i32; Type.i32; Type.pointer Type.i32|] in
  let sum_fun = Function.define_function sum_ty "sum" llmod in
  let sum_builer = Function.get_function_builder sum_fun in
  let res = Expr.add (Function.get_params sum_fun 0) (Function.get_params sum_fun 1) sum_builer in
  let _ = Stmt.assign res (Function.get_params sum_fun 2) sum_builer in
  let _ = Stmt.return_void sum_builer in
  sum_fun

let create_main_function llmod =
  let mainty = Function.get_function_type Type.i32 [||] in
  let main = Function.define_function mainty "main" llmod in
  Function.get_function_builder main

let get_sum_program () =
  let llmod = Utils.get_module "" in

  let sum_fun = create_sum_function llmod in
  let main_builder = create_main_function llmod in

  let a = Expr.var ~name:"a" Type.i32 main_builder in
  let b = Expr.var ~name:"b" Type.i32 main_builder in
  let res = Expr.var ~name:"res" Type.i32 main_builder in

  let s = Expr.glob_str ~name:"s" "Type number 1: " main_builder in
  let _ = Stmt.call (Primitives.printf llmod) [|s|] main_builder in
  let s = Expr.glob_str ~name:"s" "%d" main_builder in
  let _ = Stmt.call (Primitives.scanf llmod) [|s; a|] main_builder in

  let s = Expr.glob_str ~name:"s" "Type number 2: " main_builder in
  let _ = Stmt.call (Primitives.printf llmod) [|s|] main_builder in
  let s = Expr.glob_str ~name:"s" "%d" main_builder in
  let _ = Stmt.call (Primitives.scanf llmod) [|s; b|] main_builder in

  let s = Expr.glob_str ~name:"s" "Result: %d\n" main_builder in
  let _ = Stmt.call sum_fun [|Expr.load_pointer a main_builder; Expr.load_pointer b main_builder; res|] main_builder in
  let _ = Stmt.call (Primitives.printf llmod) [|s; Expr.load_pointer res main_builder|] main_builder in

  let _ = Stmt.return (Expr.zero) main_builder in
  llmod
