declare ptr @malloc(i64)

@x = global i32 0
@add = global ptr null
@addthree = global ptr null
@y = global i32 0
@z = global i32 0

define i32 @anon1(ptr %env, i32 %a, i32 %b) {
	%1 = getelementptr [1 x ptr], ptr %env, i32 0, i32 0
	%2 = load ptr, ptr %1
	%3 = load i32, ptr %2
	%4 = add i32 %a, %b
	%5 = add i32 %4, %3
	ret i32 %5
}

define void @caml() {
	; Bind("x", LiteralExpr(5))
	store i32 5, ptr @x 

	; FunExpr::Anonymous
	%3 = call ptr @malloc(i64 16)
	%4 = getelementptr {ptr, [1 x ptr]}, ptr %3, i32 0, i32 1
	%5 = getelementptr {ptr, [1 x ptr]}, ptr %3, i32 0, i32 0
	%6 = getelementptr [1 x ptr], ptr %4, i32 0, i32 0
	store ptr @anon1, ptr %5
	store ptr @x, ptr %6

	; Bind("add", FunExpr::Anonymous)
	store ptr %3, ptr @add

	; PartialExpr
	%7 = getelementptr {ptr, i32}, ptr null, i32 1
	%8 = ptrtoint ptr %7 to i64
	%9 = call ptr @malloc(i64 %8)
	%10 = getelementptr {ptr, i32}, ptr %9, i32 0, i32 0
	%11 = load ptr, ptr @add
	store ptr %11, ptr %10
	%12 = getelementptr {ptr, i32}, ptr %9, i32 0, i32 1
	store i32 3, ptr %12

	; Bind("addthree", ApplicationExpr)
	store ptr %9, ptr @addthree

	; ApplicationExpr
	%14 = load ptr, ptr @add
	%15 = getelementptr {ptr, [1 x ptr]}, ptr %14, i32 0, i32 0
	%16 = getelementptr {ptr, [1 x ptr]}, ptr %14, i32 0, i32 1
	%17 = load ptr, ptr %15
	%18 = load i32, ptr @x
	%19 = call i32 %17(ptr %16, i32 %18, i32 %18)

	; Bind("y", ApplicationExpr)
	store i32 %19, ptr @y

	; ApplicationExpr (with partial)
	%20 = load ptr, ptr @addthree
	%21 = getelementptr {ptr, i32}, ptr %20, i32 0, i32 0	
	%22 = getelementptr {ptr, i32}, ptr %20, i32 0, i32 1
	%23 = load ptr, ptr %21
	%24 = getelementptr {ptr, [1 x ptr]}, ptr %23, i32 0, i32 0
	%25 = getelementptr {ptr, [1 x ptr]}, ptr %23, i32 0, i32 1
	%26 = load i32, ptr %22
	%27 = load ptr, ptr %24
	%28 = load i32, ptr @x
	%29 = call i32 %27(ptr %25, i32 %26, i32 %28)

	; Bind("z", ApplicationExpr)
	store i32 %29, ptr @z

	ret void
}
