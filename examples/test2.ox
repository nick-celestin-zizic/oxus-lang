let *sys-write* : Int = 1
let *sys-exit*  : Int = 60
let *std-out*   : Int = 1

let String: Type = struct {
  data:  Mem
  count: Int
}

let TwoStrings: Type = struct {
  s1: String
  s2: String
}

;let* *global-counter*: Int = 0

let boolin: Bool = true

let cool-string: String = "WOW VERY COOL\n"

let Index: Type = Int

let Cool: Type = Index

let Nice: Type = Cool

let start: proc = {
  syscall 2 60 69
}

;;--
  ;this should be a block comment
;;-
