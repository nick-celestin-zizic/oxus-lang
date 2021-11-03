let *sys-write* : I64 = 1
let *sys-exit*  : I64 = 60
let *std-out*   : I64 = 1

let exit_with_code: proc code: I64 {
  syscall 2 *sys-exit* code
}

let another_test: proc {
  let count: I64 = 4
  syscall 4 *sys-write* *std-out* "???\n" count
}

let test: proc {
  let msg:   String = "Hello, Procedure!\n"
  let dummy: I64    = 12
  let huhh:  I64    = 100
  
  if > huhh dummy another_test
  syscall 4 *sys-write* *std-out* msg 18
}

let initial_test: proc {
  let exit-code:   I64 = 10
  let another-one: I64 = 12
  let third:       I64 = 69
  
  test
  ;;syscall 2 *sys-exit* exit-code
  if > 999 another-one {
    syscall 4 *sys-write* *std-out* "Hello, World!!\n" 15
    another_test
    syscall 2 *sys-exit* third
  }
}

let recursive: proc {
  syscall 4 *sys-write* *std-out* "penis\n" 6
  recursive
}

let print_n_times: proc n: I64 {
 if < n 1 return

 syscall 4 *sys-write* *std-out* "nice\n" 5
 print_n_times - n 1
}

let print_n_and_exit: proc n: I64 exit: I64 {
  print_n_times n
  syscall 2 *sys-exit* exit
}

let start: proc {
  ;;print_n_and_exit 3 5
  let nice: I64 = 69
  print_n_times 5
  ;;exit_with_code nice
  ;;initial_test
}
