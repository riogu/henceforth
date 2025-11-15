; Define the function signature: (i32, i32, ptr) -> {i32, i32, i32}
; Note: strings in LLVM are typically pointers

%ReturnTuple = type { i32, i32, i32 }

define %ReturnTuple @foo(i32 %arg0, i32 %arg1, ptr %arg2) {
start:
  ; let var: f32;
  %var = alloca float
  
  ; assign var, 3.0;
  store float 3.0, ptr %var
  
  ; Load for the first condition check
  %var_load_0 = load float, ptr %var
  
  ; branch var < 2.0, if_block_0, else_if_cond_0
  %cond_0 = fcmp olt float %var_load_0, 2.0
  br i1 %cond_0, label %if_block_0, label %else_if_cond_0

if_block_0:
  ; let var1: i32;
  %var1 = alloca i32
  
  ; push (69); assign var1, 3;
  store i32 3, ptr %var1
  
  ; let var2: i32;
  %var2 = alloca i32
  
  ; push (69); assign var2, 3;
  store i32 3, ptr %var2
  
  ; Compute the values we're jumping with: (69, 69)
  %if_ret_0 = add i32 0, 69      ; Could be any computation
  %if_ret_1 = add i32 0, 69
  %if_ret_2 = add i32 0, 69
  
  ; jump end, (69, 69, 69)
  br label %end

else_if_cond_0:
  ; Load var again for next comparison
  %var_load_1 = load float, ptr %var
  
  ; branch var < 2.0, else_if_block_0, else_if_cond_1
  %cond_1 = fcmp olt float %var_load_1, 2.0
  br i1 %cond_1, label %else_if_block_0, label %else_if_cond_1

else_if_block_0:
  ; branch -420 < 5 (always true)
  %cond_2 = icmp slt i32 -420, 5
  ; In the real version, this would branch somewhere, but let's assume it falls through
  
  ; Compute values: (0, 0, 0)
  %else_if_0_ret_0 = sub i32 1, 1    ; 0
  %else_if_0_ret_1 = mul i32 0, 5    ; 0
  %else_if_0_ret_2 = and i32 0, 0    ; 0
  
  ; jump end, (0, 0, 0)
  br label %end

else_if_cond_1:
  ; Load var again
  %var_load_2 = load float, ptr %var
  
  ; branch var < 2.0, else_if_block_1, else_cond_0
  %cond_3 = fcmp olt float %var_load_2, 2.0
  br i1 %cond_3, label %else_if_block_1, label %else_cond_0

else_if_block_1:
  ; branch -420 < 5 (always true)
  %cond_4 = icmp slt i32 -420, 5
  
  ; Compute values: (1, 1, 1)
  %else_if_1_ret_0 = add i32 0, 1
  %else_if_1_ret_1 = or i32 1, 0
  %else_if_1_ret_2 = xor i32 0, 1
  
  ; jump end, (1, 1, 1)
  br label %end

else_cond_0:
  ; Compute values: (420, 420, 420)
  %else_ret_0 = add i32 400, 20
  %else_ret_1 = mul i32 210, 2
  %else_ret_2 = sub i32 500, 80
  
  ; jump end, (420, 420, 420)
  br label %end

end:
  ; return phi (if_block_0, else_if_block_0, else_if_block_1, else_cond_0)
  ; Need one phi node per return value
  %ret_val_0 = phi i32 [ %if_ret_0, %if_block_0 ], 
                       [ %else_if_0_ret_0, %else_if_block_0 ], 
                       [ %else_if_1_ret_0, %else_if_block_1 ], 
                       [ %else_ret_0, %else_cond_0 ]
  
  %ret_val_1 = phi i32 [ %if_ret_1, %if_block_0 ], 
                       [ %else_if_0_ret_1, %else_if_block_0 ], 
                       [ %else_if_1_ret_1, %else_if_block_1 ], 
                       [ %else_ret_1, %else_cond_0 ]
  
  %ret_val_2 = phi i32 [ %if_ret_2, %if_block_0 ], 
                       [ %else_if_0_ret_2, %else_if_block_0 ], 
                       [ %else_if_1_ret_2, %else_if_block_1 ], 
                       [ %else_ret_2, %else_cond_0 ]
  
  ; Pack into return struct
  %ret_0 = insertvalue %ReturnTuple undef, i32 %ret_val_0, 0
  %ret_1 = insertvalue %ReturnTuple %ret_0, i32 %ret_val_1, 1
  %ret_2 = insertvalue %ReturnTuple %ret_1, i32 %ret_val_2, 2
  
  ret %ReturnTuple %ret_2
}

define i32 @main() {
entry:
  ; Call foo with (1, 2, null_ptr)
  %null_str = inttoptr i64 0 to ptr
  %result = call %ReturnTuple @foo(i32 1, i32 2, ptr %null_str)
  
  ; Extract the values (even though we pop them)
  %val0 = extractvalue %ReturnTuple %result, 0
  %val1 = extractvalue %ReturnTuple %result, 1
  %val2 = extractvalue %ReturnTuple %result, 2
  
  ; Pop them (in LLVM, this just means we don't use them)
  ; Return 0 implicitly
  ret i32 0
}
