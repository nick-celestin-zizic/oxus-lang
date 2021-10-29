sys-write :: 1
sys-exit  :: 60
std-out   :: 1

another_test :: proc {
  count :: 4
  syscall 4 sys-write std-out "???\n" count
}

test :: proc {
  msg :: "Hello, Procedure!\n"
  dummy :: 12
  huhh :: 100
  if > huhh dummy {
    another_test
  }
  syscall 4 sys-write std-out msg 18
}

initial_test :: proc {
  exit-code :: 10
  another-one :: 12
  third :: 100
  
  test
  ;syscall 2 sys-exit exit-code
  if > 999 another-one {
    syscall 4 sys-write std-out "Hello, World!!\n" 15
    another_test
    syscall 2 sys-exit third
  }
}

recursive :: proc {
  syscall 4 sys-write std-out "penis\n" 6
  recursive
}


start :: proc {
  initial_test
}