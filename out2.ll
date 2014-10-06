; ModuleID = 'ex.ll'

; Function Attrs: nounwind
define i64 @main() #0 {
  %tmp1 = mul i64 1, 1
  %tmp2 = mul i64 %tmp1, %tmp1
  %tmp3 = mul i64 %tmp2, %tmp2
  %tmp4 = mul i64 %tmp3, %tmp3
  %tmp5 = mul i64 %tmp4, %tmp4
  ret i64 %tmp5
}

attributes #0 = { nounwind }
