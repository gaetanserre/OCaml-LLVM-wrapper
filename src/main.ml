let () =
  let ll_sum = Sum_program.get_sum_program () in
  Llvm_utils.Writer.module_to_llm ll_sum "sum.ll";

  let ll_comps = Comps_program.get_comps_program () in
  Llvm_utils.Writer.module_to_llm ll_comps "comps.ll"