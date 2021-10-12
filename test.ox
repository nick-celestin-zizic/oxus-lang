main: proc = {
  count := 0
  
  label loop
  
  print "Hello, World\n"
  count += 1
  
  if count < 10 jump loop
}

square: func i: I64 -> I64 = i * i