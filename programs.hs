fib :: Int -> Int
fib n
  | n == 0 || n == 1 = n
  | n > 1 = fib (n - 1) + fib (n - 2)

ack :: Int -> Int -> Int
ack m n
  | m == 0 = n + 1
  | m > 0 && n == 0 = ack (m - 1) 1
  | m > 0 && n > 0 = ack (m - 1) (ack m (n - 1))

mult :: Integer -> Integer -> Integer
mult a b
  | a == 0 || b == 0 = 0
  | a <= b = mult (a - 1) b + b
  | b < a = mult (b - 1) a + a

rpm :: Integer -> Integer -> Integer
rpm m n
  | mod n 2 == 0 = rpm (m * 2) (div n 2)
  | otherwise = m + rpm (m * 2) (div n 2)
