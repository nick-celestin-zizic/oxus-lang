start: proc = {
  sys_exit  := 60
  exit_code := 42
  syscall sys_exit exit_code
}