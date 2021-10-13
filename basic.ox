main: proc {
  a: I64 : 64
  b: I64 = 420
  b = a + b
  print b
}

contant :: 12

add: func a: U64 b: U64 -> U64 = a + a + b + b

monadic: func path: String -> IO Bool = {
  a: String <- readFile path
  a == "nice"
}