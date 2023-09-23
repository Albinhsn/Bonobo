; ModuleID = 'Bonobo'
source_filename = "Bonobo"

define i32 @main() {
entry:

entry1:                                           ; No predecessors!
  %a = alloca i32, align 4
  store i32 42, ptr %a, align 4
  %0 = alloca [0 x i32], align 4
  %1 = load [0 x i32], ptr %0, align 4
  %b = alloca [0 x i32], align 4
  store [0 x i32] %1, ptr %b, align 4
  %2 = alloca [3 x i32], align 4
  store i32 1, i32 0, align 4
  store i32 2, i32 getelementptr (i32, i32 1, i32 1), align 4
  store i32 3, i32 getelementptr (i32, i32 2, i32 2), align 4
  %3 = load [3 x i32], ptr %2, align 4
  %c = alloca [0 x i32], align 4
  store [3 x i32] %3, ptr %c, align 4
  %d = alloca i32, align 4
  store i1 false, ptr %d, align 1
  %i = alloca i32, align 4
  store i32 0, ptr %i, align 4
  br label %loop.header

loop.header:                                      ; preds = %loop.body, %entry1
  %4 = load i32, ptr %i, align 4
  %5 = icmp ult i32 %4, 5
  br i1 %5, label %loop.body, label %loop.exit

loop.body:                                        ; preds = %loop.header
  %a2 = alloca i32, align 4
  store i32 5, ptr %a2, align 4
  %6 = load i32, ptr %i, align 4
  %7 = add i32 %6, 1
  store i32 %7, ptr %i, align 4
  br label %loop.header

loop.exit:                                        ; preds = %loop.header
  %8 = load [0 x i32], ptr %b, align 4
  %9 = add i32 %0, [0 x i32] %8
  ret i32 0
  %10 = call i32 @foo(i32 1, i32 2)
  %a3 = alloca i32, align 4
  store i1 true, ptr %a3, align 1
  %i4 = alloca i32, align 4
  store i32 0, ptr %i4, align 4
  br label %loop.header5

loop.header5:                                     ; preds = %loop.body6, %loop.exit
  %11 = load i32, ptr %i, align 4
  %12 = icmp ult i32 %11, 5
  br i1 %12, label %loop.body6, label %loop.exit7

loop.body6:                                       ; preds = %loop.header5
  %13 = load i32, ptr %i, align 4
  %14 = add i32 %13, 1
  store i32 %14, ptr %i, align 4
  br label %loop.header5

loop.exit7:                                       ; preds = %loop.header5
  ret i32 0
}

define i32 @foo(i32 %0, i32 %1) {
entry:
}
