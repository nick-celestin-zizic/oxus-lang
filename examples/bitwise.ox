let *sys-read*  : Int = 0
let *sys-write* : Int = 1
let *sys-exit*  : Int = 60

let *std-in*: Int  = 0
let *std-out*: Int = 1
let *std-err*: Int = 2


let exit: proc Int -> Unit =
  syscall 2 *sys-exit* arg1

let write: proc Int Addr Int -> Unit =
  syscall 4 *sys-write* arg1 arg2 arg3

let puti: proc Int -> Unit = {
  let  base     : Int    = 10
  let  digits   : String = "0123456789"
  let* reversed : Int    = 0
  let* num      : Int    = arg1

  while != num 0 {
    set reversed = * reversed base
    set reversed = + reversed (% num base)
    set num      = / num base
  }

  while != reversed 0 {
    write *std-out* (offset digits.data (% reversed base)) 1
    set reversed = / reversed 10
  }
}

let puti2: proc Int -> Unit = {
  let  base     : Int    = 10
  let  digits   : String = "0123456789"
  let* reversed : Int    = 0
  let* num      : Int    = arg1

  jump reverse_condition label reverse_body
    set reversed = * reversed base
    set reversed = + reversed (% num base)
    set num      = / num base
  label reverse_condition
    if != num 0 jump reverse_body

  jump print_condition label print_body
    write *std-out* (offset digits.data (% reversed base)) 1
    set reversed = / reversed 10
  label print_condition
    if != reversed 0 jump print_body
}

let puts: proc String -> Unit =
  syscall 4 *sys-write* *std-out* arg1.data arg1.count

let putln: proc = puts "\n"

let start: proc Int Addr -> Unit = {
  puti arg1 putln
  write *std-out* arg2 1092
  ;syscall 4 *sys-write* *std-out* arg2 1092 ;TODO: for some reason I can `syscall 4 *sys-write*`, but i can't just `write`
  putln
  exit 0
  
  if != arg1 2 {
    puts "usage: ./bitwise <number>\n"
    exit 1
  }
  
  let* number   : Int = 412
  let  original : Int = number
  let* ones     : Int = 0
  let* zeroes   : Int = 0

  let* i: Int = 0 jump loop_condition label loop_body
    if & number 1
      inc ones
    else
      inc zeroes
      
    set number = shr number 1
  inc i label loop_condition if < i 64 jump loop_body
  
  puts "Your number was " puti original putln
  puts "There were "      puti zeroes   puts " bits set to 0\n"
  puts "There were "      puti ones     puts " bits set to 1\n"
}
