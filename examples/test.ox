start :: proc {
  sys_exit  :: 60
  sys_write :: 1  
  
  msg :: "Hello, World\n"
  syscall 4 sys_write 1 msg 13
  

  syscall 2 sys_exit 42
}