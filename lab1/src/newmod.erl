
-module(newmod).

-export([f/0, f/1, power/2, factorial/1]).

f(1) -> 1.
f() -> io:format("text"), "text".

power(A, 1) -> A;
power(A, B) -> A * power(A, B-1).

factorial(1) -> 1;
factorial(N) -> N * factorial(N-1).