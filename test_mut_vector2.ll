; ModuleID = 'test_mut_vector2'
source_filename = "test_mut_vector2"

declare ptr @malloc(i64)

define i64 @mv-default-cap(ptr %0) {
entry:
  ret i64 4
}

define i64 @mv-growth-factor(ptr %0) {
entry:
  ret i64 2
}

define i64 @mv-get-len(ptr %0, ptr %1) {
entry:
  %arrayidx = getelementptr i64, ptr %1, i64 0
  %arrayget = load i64, ptr %arrayidx, align 4
  ret i64 %arrayget
}

define i64 @mv-get-cap(ptr %0, ptr %1) {
entry:
  %arrayidx = getelementptr i64, ptr %1, i64 1
  %arrayget = load i64, ptr %arrayidx, align 4
  ret i64 %arrayget
}

define i64 @"mv-set-len!"(ptr %0, ptr %1, i64 %2) {
entry:
  %arrayidx = getelementptr i64, ptr %1, i64 0
  store i64 %2, ptr %arrayidx, align 4
  ret i32 0
}

define i64 @mv-data-idx(ptr %0, i64 %1) {
entry:
  %add = add i64 %1, 2
  ret i64 %add
}

define ptr @mv-copy-loop(ptr %0, ptr %1, ptr %2, i64 %3, i64 %4) {
entry:
  %icmp = icmp sge i64 %3, %4
  br i1 %icmp, label %then_1, label %else_2

then_1:                                           ; preds = %entry
  br label %merge_3

else_2:                                           ; preds = %entry
  %call = call i64 @mv-data-idx(ptr null, i64 %3)
  %call1 = call i64 @mv-data-idx(ptr null, i64 %3)
  %arrayidx = getelementptr i64, ptr %1, i64 %call1
  %arrayget = load i64, ptr %arrayidx, align 4
  %arrayidx2 = getelementptr i64, ptr %2, i64 %call
  store i64 %arrayget, ptr %arrayidx2, align 4
  %add = add i64 %3, 1
  %call3 = call ptr @mv-copy-loop(ptr null, ptr %1, ptr %2, i64 %add, i64 %4)
  br label %merge_3

merge_3:                                          ; preds = %else_2, %then_1
  %_if_result_3 = phi ptr [ %2, %then_1 ], [ %call3, %else_2 ]
  ret ptr %_if_result_3
}

define ptr @mv-copy-data(ptr %0, ptr %1, ptr %2, i64 %3) {
entry:
  %tailcall = tail call ptr @mv-copy-loop(ptr null, ptr %1, ptr %2, i64 0, i64 %3)
  ret ptr %tailcall
}

define ptr @mv-grow(ptr %0, ptr %1) {
entry:
  %gep = getelementptr inbounds { i64, ptr }, ptr %1, i32 0, i32 1
  %load = load ptr, ptr %gep, align 8
  %call = call i64 @mv-get-len(ptr null, ptr %load)
  %call1 = call i64 @mv-get-cap(ptr null, ptr %load)
  %call2 = call i64 @mv-growth-factor(ptr null)
  %mul = mul i64 %call1, %call2
  %add = add i64 %mul, 2
  %mul3 = mul i64 %add, 8
  %call4 = call ptr @malloc(i64 %mul3)
  %arrayidx = getelementptr i64, ptr %call4, i64 0
  store i64 %call, ptr %arrayidx, align 4
  %arrayidx5 = getelementptr i64, ptr %call4, i64 1
  store i64 %mul, ptr %arrayidx5, align 4
  %call6 = call ptr @mv-copy-data(ptr null, ptr %load, ptr %call4, i64 %call)
  %heap_struct = call ptr @malloc(i64 16)
  %field_0 = getelementptr { i64, ptr }, ptr %heap_struct, i32 0, i32 0
  store i64 1, ptr %field_0, align 4
  %field_1 = getelementptr { i64, ptr }, ptr %heap_struct, i32 0, i32 1
  store ptr %call4, ptr %field_1, align 8
  ret ptr %heap_struct
}

define ptr @mut-vector(ptr %0) {
entry:
  %call = call i64 @mv-default-cap(ptr null)
  %add = add i64 %call, 2
  %mul = mul i64 %add, 8
  %call1 = call ptr @malloc(i64 %mul)
  %arrayidx = getelementptr i64, ptr %call1, i64 0
  store i64 0, ptr %arrayidx, align 4
  %arrayidx2 = getelementptr i64, ptr %call1, i64 1
  store i64 %call, ptr %arrayidx2, align 4
  %heap_struct = call ptr @malloc(i64 16)
  %field_0 = getelementptr { i64, ptr }, ptr %heap_struct, i32 0, i32 0
  store i64 1, ptr %field_0, align 4
  %field_1 = getelementptr { i64, ptr }, ptr %heap_struct, i32 0, i32 1
  store ptr %call1, ptr %field_1, align 8
  ret ptr %heap_struct
}

define i64 @__MutIndexed_MutVector__mv-nth(ptr %0, i64 %1) {
entry:
  %gep = getelementptr inbounds { i64, ptr }, ptr %0, i32 0, i32 1
  %load = load ptr, ptr %gep, align 8
  %call = call i64 @mv-data-idx(ptr null, i64 %1)
  %arrayidx = getelementptr i64, ptr %load, i64 %call
  %arrayget = load i64, ptr %arrayidx, align 4
  ret i64 %arrayget
}

define ptr @"__MutIndexed_MutVector__mv-set!"(ptr %0, i64 %1, i64 %2) {
entry:
  %gep = getelementptr inbounds { i64, ptr }, ptr %0, i32 0, i32 1
  %load = load ptr, ptr %gep, align 8
  %call = call i64 @mv-data-idx(ptr null, i64 %1)
  %arrayidx = getelementptr i64, ptr %load, i64 %call
  store i64 %2, ptr %arrayidx, align 4
  ret ptr %0
}

define i64 @__MutCounted_MutVector__mv-count(ptr %0) {
entry:
  %gep = getelementptr inbounds { i64, ptr }, ptr %0, i32 0, i32 1
  %load = load ptr, ptr %gep, align 8
  %call = call i64 @mv-get-len(ptr null, ptr %load)
  ret i64 %call
}

define i1 @"__MutCounted_MutVector__mv-empty?"(ptr %0) {
entry:
  %gep = getelementptr inbounds { i64, ptr }, ptr %0, i32 0, i32 1
  %load = load ptr, ptr %gep, align 8
  %call = call i64 @mv-get-len(ptr null, ptr %load)
  %icmp = icmp eq i64 %call, 0
  ret i1 %icmp
}

define ptr @"__MutGrowable_MutVector__mv-push!"(ptr %0, i64 %1) {
entry:
  %gep = getelementptr inbounds { i64, ptr }, ptr %0, i32 0, i32 1
  %load = load ptr, ptr %gep, align 8
  %call = call i64 @mv-get-len(ptr null, ptr %load)
  %call1 = call i64 @mv-get-cap(ptr null, ptr %load)
  %icmp = icmp slt i64 %call, %call1
  br i1 %icmp, label %then_1, label %else_2

then_1:                                           ; preds = %entry
  %call2 = call i64 @mv-data-idx(ptr null, i64 %call)
  %arrayidx = getelementptr i64, ptr %load, i64 %call2
  store i64 %1, ptr %arrayidx, align 4
  %add = add i64 %call, 1
  %call3 = call i64 @"mv-set-len!"(ptr null, ptr %load, i64 %add)
  br label %merge_3

else_2:                                           ; preds = %entry
  %call4 = call ptr @mv-grow(ptr null, ptr %0)
  %gep5 = getelementptr inbounds { i64, ptr }, ptr %call4, i32 0, i32 1
  %load6 = load ptr, ptr %gep5, align 8
  %call7 = call i64 @mv-get-len(ptr null, ptr %load6)
  %call8 = call i64 @mv-data-idx(ptr null, i64 %call7)
  %arrayidx9 = getelementptr i64, ptr %load6, i64 %call8
  store i64 %1, ptr %arrayidx9, align 4
  %add10 = add i64 %call7, 1
  %call11 = call i64 @"mv-set-len!"(ptr null, ptr %load6, i64 %add10)
  br label %merge_3

merge_3:                                          ; preds = %else_2, %then_1
  %_if_result_13 = phi ptr [ %0, %then_1 ], [ %call4, %else_2 ]
  ret ptr %_if_result_13
}

define i64 @"__MutGrowable_MutVector__mv-pop!"(ptr %0) {
entry:
  %gep = getelementptr inbounds { i64, ptr }, ptr %0, i32 0, i32 1
  %load = load ptr, ptr %gep, align 8
  %call = call i64 @mv-get-len(ptr null, ptr %load)
  %icmp = icmp eq i64 %call, 0
  br i1 %icmp, label %then_1, label %else_2

then_1:                                           ; preds = %entry
  br label %merge_3

else_2:                                           ; preds = %entry
  %sub = sub i64 %call, 1
  %call1 = call i64 @mv-data-idx(ptr null, i64 %sub)
  %arrayidx = getelementptr i64, ptr %load, i64 %call1
  %arrayget = load i64, ptr %arrayidx, align 4
  %call2 = call i64 @"mv-set-len!"(ptr null, ptr %load, i64 %sub)
  br label %merge_3

merge_3:                                          ; preds = %else_2, %then_1
  %_if_result_19 = phi i64 [ 0, %then_1 ], [ %arrayget, %else_2 ]
  ret i64 %_if_result_19
}

define i64 @test-push(ptr %0) {
entry:
  %call = call ptr @mut-vector(ptr null)
  %gep = getelementptr inbounds i64, ptr %call, i64 0
  %load = load i64, ptr %gep, align 4
  %icmp = icmp eq i64 %load, 1
  %call1 = call ptr @"__MutGrowable_MutVector__mv-push!"(ptr %call, i64 10)
  %select = select i1 %icmp, ptr %call1, ptr null
  %gep2 = getelementptr inbounds i64, ptr %select, i64 0
  %load3 = load i64, ptr %gep2, align 4
  %icmp4 = icmp eq i64 %load3, 1
  %call5 = call ptr @"__MutGrowable_MutVector__mv-push!"(ptr %select, i64 20)
  %select6 = select i1 %icmp4, ptr %call5, ptr null
  %gep7 = getelementptr inbounds i64, ptr %select6, i64 0
  %load8 = load i64, ptr %gep7, align 4
  %icmp9 = icmp eq i64 %load8, 1
  %call10 = call ptr @"__MutGrowable_MutVector__mv-push!"(ptr %select6, i64 30)
  %select11 = select i1 %icmp9, ptr %call10, ptr null
  %gep12 = getelementptr inbounds i64, ptr %select11, i64 0
  %load13 = load i64, ptr %gep12, align 4
  %icmp14 = icmp eq i64 %load13, 1
  %call15 = call i64 @__MutIndexed_MutVector__mv-nth(ptr %select11, i64 0)
  %select16 = select i1 %icmp14, i64 %call15, i64 0
  %gep17 = getelementptr inbounds i64, ptr %select11, i64 0
  %load18 = load i64, ptr %gep17, align 4
  %icmp19 = icmp eq i64 %load18, 1
  %call20 = call i64 @__MutIndexed_MutVector__mv-nth(ptr %select11, i64 1)
  %select21 = select i1 %icmp19, i64 %call20, i64 0
  %gep22 = getelementptr inbounds i64, ptr %select11, i64 0
  %load23 = load i64, ptr %gep22, align 4
  %icmp24 = icmp eq i64 %load23, 1
  %call25 = call i64 @__MutIndexed_MutVector__mv-nth(ptr %select11, i64 2)
  %select26 = select i1 %icmp24, i64 %call25, i64 0
  %add = add i64 %select21, %select26
  %add27 = add i64 %select16, %add
  ret i64 %add27
}

define i64 @test-growth(ptr %0) {
entry:
  %call = call ptr @mut-vector(ptr null)
  %gep = getelementptr inbounds i64, ptr %call, i64 0
  %load = load i64, ptr %gep, align 4
  %icmp = icmp eq i64 %load, 1
  %call1 = call ptr @"__MutGrowable_MutVector__mv-push!"(ptr %call, i64 1)
  %select = select i1 %icmp, ptr %call1, ptr null
  %gep2 = getelementptr inbounds i64, ptr %select, i64 0
  %load3 = load i64, ptr %gep2, align 4
  %icmp4 = icmp eq i64 %load3, 1
  %call5 = call ptr @"__MutGrowable_MutVector__mv-push!"(ptr %select, i64 2)
  %select6 = select i1 %icmp4, ptr %call5, ptr null
  %gep7 = getelementptr inbounds i64, ptr %select6, i64 0
  %load8 = load i64, ptr %gep7, align 4
  %icmp9 = icmp eq i64 %load8, 1
  %call10 = call ptr @"__MutGrowable_MutVector__mv-push!"(ptr %select6, i64 3)
  %select11 = select i1 %icmp9, ptr %call10, ptr null
  %gep12 = getelementptr inbounds i64, ptr %select11, i64 0
  %load13 = load i64, ptr %gep12, align 4
  %icmp14 = icmp eq i64 %load13, 1
  %call15 = call ptr @"__MutGrowable_MutVector__mv-push!"(ptr %select11, i64 4)
  %select16 = select i1 %icmp14, ptr %call15, ptr null
  %gep17 = getelementptr inbounds i64, ptr %select16, i64 0
  %load18 = load i64, ptr %gep17, align 4
  %icmp19 = icmp eq i64 %load18, 1
  %call20 = call ptr @"__MutGrowable_MutVector__mv-push!"(ptr %select16, i64 5)
  %select21 = select i1 %icmp19, ptr %call20, ptr null
  %gep22 = getelementptr inbounds i64, ptr %select21, i64 0
  %load23 = load i64, ptr %gep22, align 4
  %icmp24 = icmp eq i64 %load23, 1
  %call25 = call i64 @__MutCounted_MutVector__mv-count(ptr %select21)
  %select26 = select i1 %icmp24, i64 %call25, i64 0
  ret i64 %select26
}

define i64 @test-pop(ptr %0) {
entry:
  %call = call ptr @mut-vector(ptr null)
  %gep = getelementptr inbounds i64, ptr %call, i64 0
  %load = load i64, ptr %gep, align 4
  %icmp = icmp eq i64 %load, 1
  %call1 = call ptr @"__MutGrowable_MutVector__mv-push!"(ptr %call, i64 100)
  %select = select i1 %icmp, ptr %call1, ptr null
  %gep2 = getelementptr inbounds i64, ptr %select, i64 0
  %load3 = load i64, ptr %gep2, align 4
  %icmp4 = icmp eq i64 %load3, 1
  %call5 = call ptr @"__MutGrowable_MutVector__mv-push!"(ptr %select, i64 200)
  %select6 = select i1 %icmp4, ptr %call5, ptr null
  %gep7 = getelementptr inbounds i64, ptr %select6, i64 0
  %load8 = load i64, ptr %gep7, align 4
  %icmp9 = icmp eq i64 %load8, 1
  %call10 = call i64 @"__MutGrowable_MutVector__mv-pop!"(ptr %select6)
  %select11 = select i1 %icmp9, i64 %call10, i64 0
  %gep12 = getelementptr inbounds i64, ptr %select6, i64 0
  %load13 = load i64, ptr %gep12, align 4
  %icmp14 = icmp eq i64 %load13, 1
  %call15 = call i64 @__MutCounted_MutVector__mv-count(ptr %select6)
  %select16 = select i1 %icmp14, i64 %call15, i64 0
  %add = add i64 %select11, %select16
  ret i64 %add
}

define i64 @main() {
entry:
  %call = call i64 @test-push(ptr null)
  %call1 = call i64 @test-growth(ptr null)
  %call2 = call i64 @test-pop(ptr null)
  %add = add i64 %call1, %call2
  %add3 = add i64 %call, %add
  ret i64 %add3
}
