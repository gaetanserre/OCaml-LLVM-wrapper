open Llvm_utils

let create_comps_function llmod =
  let comps_ty = Function.get_function_type Type.bool [|Type.i32; Type.i32;|] in
  let comps_fun = Function.define_function comps_ty "comps" llmod in
  let comps_builer = Function.get_function_builder comps_fun in
  let left = Expr.gt (Function.get_params comps_fun 0) (Function.get_params comps_fun 1) comps_builer in
  let right = Expr.eq (Function.get_params comps_fun 0) (Function.get_params comps_fun 1) comps_builer in
  let _ = Stmt.return (Expr.ll_or left right comps_builer) comps_builer in
  comps_fun

let create_main_function llmod =
  let mainty = Function.get_function_type Type.i32 [||] in
  let main = Function.define_function mainty "main" llmod in
  Function.get_function_builder main

let get_comps_program () =
  let llmod = Utils.get_module "" in

  let comps_fun = create_comps_function llmod in
  let main_builder = create_main_function llmod in

  let a = Expr.var ~name:"a" Type.i32 main_builder in
  let b = Expr.var ~name:"b" Type.i32 main_builder in

  let s = Expr.glob_str ~name:"s" "Type number 1: " main_builder in
  let _ = Stmt.call (Primitives.printf llmod) [|s|] main_builder in
  let s = Expr.glob_str ~name:"s" "%d" main_builder in
  let _ = Stmt.call (Primitives.scanf llmod) [|s; a|] main_builder in

  let s = Expr.glob_str ~name:"s" "Type number 2: " main_builder in
  let _ = Stmt.call (Primitives.printf llmod) [|s|] main_builder in
  let s = Expr.glob_str ~name:"s" "%d" main_builder in
  let _ = Stmt.call (Primitives.scanf llmod) [|s; b|] main_builder in

  let s = Expr.glob_str ~name:"s" "Result: %d\n" main_builder in
  let res = Expr.var ~name:"res" Type.bool main_builder in
  let ret = Stmt.call comps_fun [|Expr.load_pointer a main_builder; Expr.load_pointer b main_builder|] main_builder in
  let _ = Stmt.assign ret res main_builder in
  let _ = Stmt.call (Primitives.printf llmod) [|s; Expr.load_pointer res main_builder|] main_builder in

  let _ = Stmt.return (Expr.zero) main_builder in
  llmod