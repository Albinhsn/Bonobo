; ModuleID = 'Bonobo'
source_filename = "Bonobo"

define i32 @main() {
entry:
  ret i32 0
  br label %ifcond
}

define i32 @fib() {
fib:
  br i1 false, label %then, label %else

then:                                             ; preds = %fib

else:                                             ; preds = %fib
  br label %ifcond

ifcond:                                           ; preds = %else, %entry
  %iftmp = phi double [ 0.000000e+00, %entry ], [ 0.000000e+00, %entry ]
  ret i32 0
}
