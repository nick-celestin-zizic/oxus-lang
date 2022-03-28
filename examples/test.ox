let *sys-write* : Int = 1
let *sys-exit*  : Int = 60
let *std-out*   : Int = 1

let write: proc String -> Unit =
  syscall 4 *sys-write* *std-out* arg1.data arg1.count
let exit: proc Int -> Unit =
  syscall 2 *sys-exit* arg1

let print_n_times: proc n: Int {
 ;if < n 1 return

 ;write "nice\n" 5
 print_n_times - n 1
}

let print_n_and_exit: proc n: Int code: Int {
  print_n_times n
  exit code
}

; let ExitCode = U8
; let ExitCode::check: func n: U8 -> Result ExitCode String =
;  if n 'in-range 0 127 then ok n else error "ExitCode not in range"
; let Maybe = enum {
;   some: arg1
;   none
; }
; let Maybe: $func a: Type -> Type = enum {
;   some: a
;   none
; }

; let tuple: { Int, Mem } = 12 mem guy

;  let value = match ptr with

; let type: macro (name: Ident) (param: TypedIdent) (r: Rest) =
;  let name: $func param -> Type r

; TEMPLATE Maybe a = enum {
;   some: a
;   none
; }

; TEMPLATE Array a n: U64 = struct {
;   data: Ptr a
;   count: n
; }

; TEMPLATE Result r l = enum {
;   ok: r
;   error: l
; }

; let nice: Maybe Int = some 3

; match ptr with
;  none -> expr
;  some value -> other-expr

; let Array: $func a: Type -> Type = struct {
;   count: U64
;   ptr:   Mem
;   typ:   Type
; }

let test: proc {
  ;let msg:  Str = "Hello, Procedure!\n"
  let less: Int    = 12
  let more: Int    = 100
  
  if > more less {
    write "Hello, Procedure!\n" 18
  }
  
}

let initial_test: proc {
  let exit-code:   Int = 10
  let another-one: Int = 12
  
  test
  if > 999 another-one {
    write "Hello, World!!\n" 15
  }
}

let start: proc {
  let count: Int = 0
  label loop
  write "cool\n" 5
  ++ count
  if < count 3 jump loop
      
  initial_test
  print_n_and_exit 3 5
}
