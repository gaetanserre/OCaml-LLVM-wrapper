open Llvm_wrapper
let _ =
  let prog = Llvm.create_program "test_ml.ll" in

  let apply = Llvm.create_function "@apply" I32 [I32, "%a"; FPointer (I32, [I32]), "%func"] in
  let res = Llvm.Assign ("%res", Llvm.CallPointer (Var "%func", I32, [I32, Var "%a"])) in
  Llvm.add_code apply res;
  Llvm.add_code apply (Llvm.Return (I32, Var "%res"));
  Llvm.add_function prog apply;

  let by2 = Llvm.create_function "@by2" I32 [I32, "%a"] in
  let res = Llvm.Assign ("%res", Llvm.Binop (Mul, I32, Cst 2, Var "%a")) in
  Llvm.add_code by2 res;
  Llvm.add_code by2 (Llvm.Return (I32, Var "%res"));
  Llvm.add_function prog by2;

  let main = Llvm.create_main_function () in

  let res = Llvm.Assign ("%res", Llvm.Call ("@apply", I32, [I32, Cst 8; FPointer (I32, [I32]), Var "@by2"])) in
  Llvm.add_code main res;
  Llvm.add_code main (Return (I32, Cst 0));
  Llvm.add_function prog main;

  Llvm.build_llvm_program prog