let *sys-write* : Int = 1
let *sys-exit*  : Int = 60
let *std-out*   : Int = 1


let write: proc String -> Unit =
  syscall 4 *sys-write* *std-out* arg1.data arg1.count
let exit: proc Int -> Unit =
  syscall 2 *sys-exit* arg1


let TwoStrings: Type = struct {
  s1: String
  s2: String
}

let *boolin*: Bool = true

let *cool-string*: String = "WOW VERY COOL\n"
let *cooler-string*: String = "WOW VERY COOL\n"

let Index: Type = Int

let Cool: Type = Index

let Nice: Type = Cool

let Test: Type = struct {
  left: Addr
  right: Addr
}
 
let start: proc Int = {
  let code: Int = 42
  let cool: String = "testing\n"
  let cooler: String = "deez\n"
  let a: String = "a"
  write cool
  write a
  write "coolio\n"
  write cooler
  
  exit code
}
