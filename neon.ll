; ModuleID = 'ex.neon'
source_filename = "ex.neon"

declare void @putchar(i64)

define void @main() {
entry:
  call void @putchar(i64 118)
  ret void
}
