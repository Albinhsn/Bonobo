; ModuleID = 'Bonobo'
source_filename = "Bonobo"

@0 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare i32 @printf(ptr, ...)

define i32 @fib(i32 %0) {
fib:
  %1 = icmp sle i32 %0, 2
  br i1 %1, label %then, label %else

then:                                             ; preds = %fib
  br label %merge

else:                                             ; preds = %fib
  %2 = sub i32 %0, 2
  %3 = sub i32 %0, 1
  %4 = call i32 @fib(i32 %2)
  %5 = call i32 @fib(i32 %3)
  %6 = add i32 %4, %5
  br label %merge

merge:                                            ; preds = %else, %then
  %7 = phi i32 [ 1, %then ], [ %6, %else ]
  ret i32 %7
}

define i32 @main() {
entry:
  %0 = call i32 @fib(i32 35)
  %1 = call i32 (ptr, ...) @printf(ptr @0, i32 %0)
  ret i32 0
}
