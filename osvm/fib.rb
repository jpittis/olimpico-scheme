def fib(n)
  return 0 if n == 0
  return 1 if n == 1
  fib(n - 1) + fib(n - 2)
end

before = Process.clock_gettime(Process::CLOCK_MONOTONIC)
puts fib(30)
after = Process.clock_gettime(Process::CLOCK_MONOTONIC)
delta = after - before
puts "Took #{delta}s"
