define i64 @even(i64 %v) {
  entry:
    ret i64 0

}

define i64 @main() nounwind {
    call i64 @even(i64 5)
    ret i64 0 


} 
 
