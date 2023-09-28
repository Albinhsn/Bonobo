; ModuleID = 'Bonobo'
source_filename = "Bonobo"

%array = type { [3 x i32], i32 }

@0 = private constant [3 x i32] [i32 1, i32 2, i32 3]

define i32 @main() {
entry:
  %array = alloca %array, align 8
  %0 = getelementptr inbounds %array, ptr %array, i32 0, i32 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %0, ptr align 4 @0, i64 3, i1 false)
  %1 = getelementptr inbounds %array, ptr %array, i32 0, i32 1
  store i32 3, ptr %1, align 4
  %2 = load %array, ptr %array, align 4
  %a = alloca %array, align 8
  store %array %2, ptr %a, align 4
  %3 = getelementptr inbounds %array, ptr %a, i32 0, i32 0
  %4 = getelementptr %array, ptr %3, i32 0, i32 1
  %5 = load i32, ptr %4, align 4
  %b = alloca i32, align 8
  store i32  %5, ptr %b, align 4
  ret i32 %5
}

declare i32 @printf(ptr, ...)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
