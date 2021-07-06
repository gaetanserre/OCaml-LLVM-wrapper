module Utils = struct
  let llctx = Llvm.global_context ()
  let get_module name = Llvm.create_module llctx name
end

module Reader = struct
  let bytecode_to_module path =
    let llmem = Llvm.MemoryBuffer.of_file path in
    Llvm_bitreader.parse_bitcode Utils.llctx llmem
  
  let module_to_string llmod =
    Llvm.string_of_llmodule llmod

  let rec print_type llty =
    let ty = Llvm.classify_type llty in
    match ty with
    | Llvm.TypeKind.Integer  -> Printf.printf "integer\n"
    | Llvm.TypeKind.Function -> Printf.printf "function\n"
    | Llvm.TypeKind.Array    -> Printf.printf "array of" ; print_type (Llvm.element_type llty)
    | Llvm.TypeKind.Pointer  -> Printf.printf "pointer to" ; print_type (Llvm.element_type llty)
    | Llvm.TypeKind.Vector   -> Printf.printf "vector of" ; print_type (Llvm.element_type llty)
    | _                      -> Printf.printf "other type\n"
  
  let print_val lv =
    Printf.printf "Value\n";
    Printf.printf "name %s\n" (Llvm.value_name lv);
    let llty = Llvm.type_of lv in
    Printf.printf "type %s" (Llvm.string_of_lltype llty);
    print_type llty
  
  let print_func llmod =
    Llvm.iter_functions print_val llmod
  
  let print_globals llmod =
    Llvm.iter_globals print_val llmod ;
end

module Writer = struct
  let module_to_bytecode llmod path =
    Llvm_analysis.assert_valid_module llmod;
    Llvm_bitwriter.write_bitcode_file llmod path |> ignore
  
  let module_to_llm llmod path =
    Llvm_analysis.assert_valid_module llmod;
    let file_out = open_out path in
    Printf.fprintf file_out "%s" (Reader.module_to_string llmod);
    close_out file_out
  
end

module Type = struct
  let void = Llvm.void_type Utils.llctx
  let bool = Llvm.i1_type Utils.llctx
  let i8 = Llvm.i8_type Utils.llctx
  let i32 = Llvm.i32_type Utils.llctx
  let i64 = Llvm.i64_type Utils.llctx
  let pointer lltype = Llvm.pointer_type lltype
end

module Function = struct
  let get_function_type typ args = Llvm.var_arg_function_type typ args
  let declare_function typ name llmod = Llvm.declare_function name typ llmod
  let define_function typ name llmod = Llvm.define_function name typ llmod
  let get_function_builder func = Llvm.builder_at_end Utils.llctx (Llvm.entry_block func)
  let get_params func nb = Llvm.param func nb
end

module Primitives = struct
  let printf llmod =
    let printf_ty = Function.get_function_type (Type.i32) [|Type.pointer (Type.i8)|] in
    Function.declare_function printf_ty "printf" llmod
  
  let scanf llmod =
    let printf_ty = Function.get_function_type (Type.i32) [|Type.pointer (Type.i8)|] in
    Function.declare_function printf_ty "scanf" llmod
end

module Expr = struct
  let zero = Llvm.const_int Type.i32 0
  let one = Llvm.const_int Type.i32 1

  let const i = Llvm.const_int Type.i32 i

  let var ?(name = "") typ builder =
    Llvm.build_alloca typ name builder
  
  let load_pointer ?(name = "") v builder = 
    Llvm.build_load v name builder

  let glob_str ?(name = "") s builder =
    Llvm.build_global_stringptr s name builder
    
  let add ?(name = "") v1 v2 builder = 
    Llvm.build_add v1 v2 name builder

  let sub ?(name = "") v1 v2 builder = 
    Llvm.build_sub v1 v2 name builder
  
  let mul ?(name = "") v1 v2 builder = 
    Llvm.build_mul v1 v2 name builder

  let div ?(name = "") v1 v2 builder = 
    Llvm.build_sdiv v1 v2 name builder
  
  let neg ?(name = "") v builder =
    Llvm.build_fneg v name builder

  let not ?(name = "") v builder =
    Llvm.build_not v name builder
  
  let eq ?(name = "") v1 v2 builder =
    Llvm.build_icmp Llvm.Icmp.Eq v1 v2 name builder

  let gt ?(name = "") v1 v2 builder =
    Llvm.build_icmp Llvm.Icmp.Sgt v1 v2 name builder
  
  let lt ?(name = "") v1 v2 builder =
    Llvm.build_icmp Llvm.Icmp.Slt v1 v2 name builder

  let ge ?(name = "") v1 v2 builder =
    Llvm.build_icmp Llvm.Icmp.Sge v1 v2 name builder
  
  let le ?(name = "") v1 v2 builder =
    Llvm.build_icmp Llvm.Icmp.Sle v1 v2 name builder
  
  let ll_or ?(name = "") v1 v2 builder =
    Llvm.build_or v1 v2 name builder
  
  let ll_and ?(name = "") v1 v2 builder =
    Llvm.build_and v1 v2 name builder
  
end

module Stmt = struct
  let assign value dest builder = Llvm.build_store value dest builder
  let call func args builder = Llvm.build_call func args "" builder
  let return v builder = Llvm.build_ret v builder
  let return_void builder = Llvm.build_ret_void builder
end