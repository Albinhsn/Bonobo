; ModuleID = 'example'
source_filename = "example"

@0 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

define i32 @main() {
entry:
  br label %loop.header

loop.header:                                      ; preds = %loop.body, %entry
  %0 = phi i32 [ 0, %entry ], [ %3, %loop.body ]
  %1 = icmp slt i32 %0, 10
  br i1 %1, label %loop.body, label %loop.exit

loop.body:                                        ; preds = %loop.header
  %2 = call i32 (ptr, ...) @printf(ptr @0, i32 %0)
  %3 = add i32 %0, 1
  br label %loop.header

loop.exit:                                        ; preds = %loop.header
  ret i32 0
}

declare i32 @printf(ptr, ...)
