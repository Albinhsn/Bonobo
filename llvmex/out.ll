; ModuleID = 'Bonobo'
source_filename = "Bonobo"

@0 = private constant [16 x i8] c"Hello, LLVM IR!\00"

define i32 @main() {
entry:
  ret i32 0
}
