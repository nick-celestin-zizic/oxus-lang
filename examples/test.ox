sys-write :: 1
sys-exit  :: 60
std-out   :: 1

anothertest :: proc {
  syscall 4 sys-write std-out "???\n" 4
}

test :: proc {
  msg :: "Hello, Procedure!\n"
  dummy :: 12
  huhh :: 100
  anothertest
  syscall 4 sys-write std-out msg 18
}

start :: proc {
  exit-code :: 10
  another-one :: 12
  third :: 100
  test
  ;syscall 2 sys-exit exit-code
  if > 100 10 {
    syscall 4 sys-write std-out "Hello, World!!\n" 15
    anothertest
    syscall 2 sys-exit third
  }
}