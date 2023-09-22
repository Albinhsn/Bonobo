; ModuleID = 'Bonobo'
source_filename = "Bonobo"

define i32 @main() {
entry:
  %b = alloca i32, i32 5, align 4
  %c = alloca i32, align 4
  %d = alloca double, double 1.234000e+01, align 8
  ret i32 0
}
