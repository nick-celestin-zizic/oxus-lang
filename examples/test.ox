sys-write :: 1
std-out   :: 1
;write :: (sycall 4 sys-write std-out)

test :: proc {
  msg :: "Hello, Procedure!\n"
  syscall 4 sys-write std-out msg 18
}


start :: proc {
  syscall 4 sys-write std-out "Hello, World!!\n" 15; testing
  ;comments
}