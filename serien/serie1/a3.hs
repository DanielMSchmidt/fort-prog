fib1 n = if n == 0
         then 0
         else if n == 1
              then 1
              else fib1 (n-1) + fib1 ( n - 2)


fib2 n =
  let fib2' fibn fibnp1 n =
        if n == 0
        then fibn
        else fib2' fibnp1 (fibn + fibnp1) (n - 1)
  in fib2' 0 1 n