sys-write :: 1
sys-exit  :: 60
std-out   :: 1

;another-test :: proc {
;  syscall 4 sys-write std-out "???\n" 4
;}

test :: proc {
  msg :: "Hello, Procedure!\n"
  dummy :: 12
  huhh :: 100
  ;another-test
  syscall 4 sys-write std-out msg 18
}

start :: proc {
  exit-code :: 10
  another-one :: 12
  test
  syscall 2 sys-exit exit-code
  ;if > 100 10 {
    ;syscall 4 sys-write std-out "Hello, World!!\n" 15
    ;syscall 2 sys-exit penis
  ;}
}