; ModuleID = 'llvm-test.cpp'
source_filename = "llvm-test.cpp"
target triple = "x86_64-pc-linux-gnu"

%ReturnTuple = type { i32, i32, i32 }

define %ReturnTuple @foo(i32 %arg0, i32 %arg1, ptr %arg2) {
start:
  ; let var: f32;
  %var = alloca float
  
  ; store var, 3.0;
  store float 3.0, ptr %var
  
  ; jump if_cond_0;
  br label %if_cond_0

if_cond_0:
  ; Load var for comparison
  %var_load_0 = load float, ptr %var
  
  ; branch var < 2.0, if_block_0, else_if_cond_0;
  %cond_0 = fcmp olt float %var_load_0, 2.0
  br i1 %cond_0, label %if_block_0, label %else_if_cond_0

if_block_0:
  ; let var1: i32;
  %var1 = alloca i32
  
  ; %inst0 = push (69);
  %inst0 = add i32 0, 69  ; Just holding the value 69
  
  ; store var1, 3 + 5;
  %temp_add_0 = add i32 3, 5
  store i32 %temp_add_0, ptr %var1
  
  ; let var2: i32;
  %var2 = alloca i32
  
  ; %inst1 = push (69);
  %inst1 = add i32 0, 69
  
  ; store var2, 3 * 66;
  %temp_mul_0 = mul i32 3, 66
  store i32 %temp_mul_0, ptr %var2
  
  ; %ret0 = jump end_if_0, (%inst0 %inst1);
  ; Creating a tuple to carry forward
  %ret0_temp = insertvalue %ReturnTuple undef, i32 %inst0, 0
  %ret0 = insertvalue %ReturnTuple %ret0_temp, i32 %inst1, 1
  ; Note: third value would be set here too, but let's assume it's also 69
  %ret0_final = insertvalue %ReturnTuple %ret0, i32 69, 2
  br label %end_if_0

else_if_cond_0:
  ; branch -420 < 5, else_if_block_0, else_if_cond_1;
  %cond_1 = icmp slt i32 -420, 5
  br i1 %cond_1, label %else_if_block_0, label %else_if_cond_1

else_if_block_0:
  ; %inst2 = push (0 0 0);
  %inst2_temp = insertvalue %ReturnTuple undef, i32 0, 0
  %inst2_temp2 = insertvalue %ReturnTuple %inst2_temp, i32 0, 1
  %inst2 = insertvalue %ReturnTuple %inst2_temp2, i32 0, 2
  
  ; %ret1 = jump end_if_0, %inst2;
  br label %end_if_0

else_if_cond_1:
  ; branch -69 < 69, else_if_block_1, else_block_0;
  %cond_2 = icmp slt i32 -69, 69
  br i1 %cond_2, label %else_if_block_1, label %else_block_0

else_if_block_1:
  ; %ret2 = jump end_if_0, push (1 1 1);
  %ret2_temp = insertvalue %ReturnTuple undef, i32 1, 0
  %ret2_temp2 = insertvalue %ReturnTuple %ret2_temp, i32 1, 1
  %ret2 = insertvalue %ReturnTuple %ret2_temp2, i32 1, 2
  br label %end_if_0

else_block_0:
  ; %inst4 = push (420 420 420);
  %inst4_temp = insertvalue %ReturnTuple undef, i32 420, 0
  %inst4_temp2 = insertvalue %ReturnTuple %inst4_temp, i32 420, 1
  %inst4 = insertvalue %ReturnTuple %inst4_temp2, i32 420, 2
  
  ; %ret3 = jump end_if_0, %inst4;
  br label %end_if_0

end_if_0:
  ; %inst5 = phi (if_block_0, %ret0), (else_if_block_0, %inst2), 
  ;             (else_if_block_1, %ret2), (else_block_0, %inst4);
  %inst5 = phi %ReturnTuple [ %ret0_final, %if_block_0 ],
                             [ %inst2, %else_if_block_0 ],
                             [ %ret2, %else_if_block_1 ],
                             [ %inst4, %else_block_0 ]
  
  ; let example: i32;
  %example = alloca i32
  
  ; store example, %inst5;
  ; Extract first element from tuple to store (assuming we want first value)
  %inst5_val = extractvalue %ReturnTuple %inst5, 0
  store i32 %inst5_val, ptr %example
  
  ; jump while_cond_0;
  br label %while_cond_0

while_cond_0:
  ; Load example for comparison
  %example_load_0 = load i32, ptr %example
  
  ; branch example != 0, while_block_0, end_while_0;
  %while_cond = icmp ne i32 %example_load_0, 0
  br i1 %while_cond, label %while_block_0, label %end_while_0

while_block_0:
  ; Load example for computation
  %example_load_1 = load i32, ptr %example
  
  ; store example, example - 1;
  %example_dec = sub i32 %example_load_1, 1
  store i32 %example_dec, ptr %example
  
  ; jump while_cond_0;
  br label %while_cond_0

end_while_0:
  ; jump end;
  br label %end

end:
  ; return %inst5;
  ret %ReturnTuple %inst5
}

define i32 @main() {
entry:
  ; Call foo with (1, 2, null_ptr)
  %null_str = inttoptr i64 0 to ptr
  %result = call %ReturnTuple @foo(i32 1, i32 2, ptr %null_str)
  
  ; @pop @pop; (extract and discard first two values)
  %val0 = extractvalue %ReturnTuple %result, 0
  %val1 = extractvalue %ReturnTuple %result, 1
  %val2 = extractvalue %ReturnTuple %result, 2
  
  ; Implicitly return 0 (or you could return val2 if that's the intent)
  ret i32 0
}
