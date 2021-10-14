open Llvm_wrapper
let _ =
  let prog = Llvm.create_program "test_ml.ll" in

  let sum = Llvm.create_function "sum" I32 [I32, "x"; I32, "y"] in
  let res = Llvm.Assign ("res", Binop (Add, I32, Var "x", Var "y")) in
  Llvm.add_code sum res;
  Llvm.add_code sum (Return (I32, Var "res"));

  let main = Llvm.create_main_function () in

  let res = Llvm.Assign ("res", Llvm.Call ("sum", I32, [I32, Cst 105; I32, Cst 2015])) in
  Llvm.add_code main res;
  Llvm.add_code main (Return (I32, Cst 0));

  Llvm.add_function prog sum;
  Llvm.add_function prog main;

  Llvm.compile_llvm_program prog