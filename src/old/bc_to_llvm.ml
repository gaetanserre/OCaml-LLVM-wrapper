exception No_Input_File

let () = 
  try 
    let llmod = Llvm_utils.Reader.bytecode_to_module Sys.argv.(1) in
    let s = Llvm_utils.Reader.module_to_string llmod in
    Llvm_utils.Reader.print_func llmod;
    Llvm_utils.Reader.print_globals llmod;

    try 
      let out_file = open_out Sys.argv.(2) in
      Printf.fprintf out_file "%s\n" s;
      close_out out_file; 
    with Invalid_argument _s -> Printf.printf "%s\n" s

  with Invalid_argument _s -> raise No_Input_File