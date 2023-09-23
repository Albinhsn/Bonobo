; ModuleID = 'Bonobo'
source_filename = "Bonobo"

define i32 @main() {
entry:
  %0 = call i32 @fib()
  %a = alloca i32, align 4
  store i32 %0, ptr %a, align 4
  %1 = load i32, ptr %a, align 4
  ret i32 %1
  ret i32 0
}

define i32 @fib(i32 %0) {
entry:
  %1 = icmp ult i32 %0, 2
  br i1 %1, label %then, label %else

then:                                             ; preds = %entry
  ret i32 1

else:                                             ; preds = %entry
  ret i32 2

merge:                                            ; No predecessors!
  ret i32 0
}
