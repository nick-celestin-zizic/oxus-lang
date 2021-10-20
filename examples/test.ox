start :: proc {
  sys_exit  :: 60
  exit_code :: 9
  add :: + 1 2
  syscall 2 sys_exit add
}