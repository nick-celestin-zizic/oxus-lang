let *sys-write* : I64 = 1
let *sys-exit*  : I64 = 60
let *std-out*   : I64 = 1

let exit: proc code: I64 = syscall 2 *sys-exit* code

let write: proc data: Mem count: U64 =
  syscall 4 *sys-write* *std-out* data count

let print_n_times: proc n: I64 {
 if < n 1 return

 write "nice\n" 5
 print_n_times - n 1
}

let print_n_and_exit: proc n: I64 code: I64 {
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

; let tuple: { I64, Mem } = 12 mem guy

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

; let nice: Maybe I64 = some 3

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
  let less: I64    = 12
  let more: I64    = 100
  
  if > more less {
    write "Hello, Procedure!\n" 18
  }
  
}

let initial_test: proc {
  let exit-code:   I64 = 10
  let another-one: I64 = 12
  
  test
  if > 999 another-one {
    write "Hello, World!!\n" 15
  }
}

let start: proc {
  let count: I64 = 0
  label loop
  write "cool\n" 5
  ++ count
  if < count 3 jump loop
      
  initial_test
  print_n_and_exit 3 5
}
