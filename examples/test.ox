val *sys-write* : I64 = 1
val *sys-exit*  : I64 = 60
val *std-out*   : I64 = 1

val exit_with_code: proc code: I64 {
  syscall 2 *sys-exit* code
}

val another_test: proc {
  val count: I64 = 4
  syscall 4 *sys-write* *std-out* "???\n" count
}

val test: proc {
  val msg:   String = "Hello, Procedure!\n"
  val dummy: I64    = 12
  val huhh:  I64    = 100
  
  if > huhh dummy another_test
  syscall 4 *sys-write* *std-out* msg 18
}

val initial_test: proc {
  val exit-code:   I64 = 10
  val another-one: I64 = 12
  val third:       I64 = 69
  
  test
  ;;syscall 2 *sys-exit* exit-code
  if > 999 another-one {
    syscall 4 *sys-write* *std-out* "Hello, World!!\n" 15
    another_test
    syscall 2 *sys-exit* third
  }
}

val recursive: proc {
  syscall 4 *sys-write* *std-out* "penis\n" 6
  recursive
}

val print_n_times: proc n: I64 {
 if < n 1 return

 syscall 4 *sys-write* *std-out* "nice\n" 5
 print_n_times - n 1
}

val print_n_and_exit: proc n: I64 exit: I64 {
  print_n_times n
  syscall 2 *sys-exit* exit
}

val start: proc {
  ;;print_n_and_exit 3 5
  val nice: I64 = 69
  print_n_times 5
  ;;exit_with_code nice
  initial_test
}
