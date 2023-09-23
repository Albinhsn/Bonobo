; ModuleID = 'Bonobo'
source_filename = "Bonobo"

define i32 @main() {
entry:
  %0 = call i32 @fib(i32 0)
  ret i32 %0
  ret i32 0
}

define i32 @fib(i32 %0) {
entry:
  %1 = icmp ult i32 %0, 2
  br i1 %1, label %then, label %else

then:                                             ; preds = %entry
  %2 = icmp ult i32 %0, 2
  br i1 %2, label %then1, label %else2

else:                                             ; preds = %entry
  br label %merge


then1:                                            ; preds = %then
  %3 = icmp ult i32 %0, 2
  br i1 %3, label %then4, label %else5

merge:                                            ; preds = %else, %merge3
  ret i32 1
  ret i32 0

else2:                                            ; preds = %then
  br label %merge3

merge3:                                           ; preds = %else2, %merge6
  br label %merge

then4:                                            ; preds = %then1
  br label %merge6

else5:                                            ; preds = %then1
  br label %merge6

merge6:                                           ; preds = %else5, %then4
  br label %merge3
}
