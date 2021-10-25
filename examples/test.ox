sys-write :: 1
std-out   :: 1
;write :: (sycall 4 sys-write std-out)


test :: proc {
  msg :: "Hello, World!\n"
  syscall 4 sys-write std-out msg 14
}


start :: proc {
  syscall 4 sys-write std-out "Hello, World!!\n" 15
  test
}