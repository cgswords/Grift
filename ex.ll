define i64 @main() nounwind {
    %tmp1 =     mul i64 1, 1
    %tmp2 =     mul i64 %tmp1, 2
    %tmp3 =     mul i64 %tmp2, 3
    %tmp4 =     mul i64 %tmp3, 4
    %tmp5 =     mul i64 %tmp4, 5
    ret i64 %tmp5 
} 
 
