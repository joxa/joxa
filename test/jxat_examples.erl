%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2011, Eric B Merritt
%%% @doc
%%%
%%% @end
%%% Created : 29 Dec 2011 by Eric B Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(jxat_examples).

%% API
-include_lib("proper/include/proper.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================
prop_seive() ->
    Source = <<"
(ns jxat-sieve-of-eratosthenes
        (require lists io)
        (use (joxa-core :only (!=/2))
             (erlang :only (rem/2 +/2))))

(defn sieve (v primes)
  (case v
    ([] primes)
    ((h . t)
      (sieve  (lists/filter (fn (x)
                             (!= (rem x h) 0)) t)
              (+ primes 1)))))

(defn+ sieve (v)
  (sieve (lists/seq 2 v) 1))">>,

    Ctx = 'joxa-compiler':forms(Source, []),
    ?FORALL(STarget, range(3, 1000),
            begin
                (is_binary('joxa-compiler':'get-context'(result, Ctx))
                 andalso (not 'joxa-compiler':'has-errors?'(Ctx))
                 andalso eratosthenes(STarget) == 'jxat-sieve-of-eratosthenes':sieve(STarget))
            end).



prop_fib() ->
    Source = <<"
(ns jxat-fibonacci
        (use (erlang :only (>/2 -/2 +/2))))

(defn+ fibo (n)
  (case n
    (0 0)
    (1 1)
    (_ (when (> n 0))
     (+ (fibo (- n 1))
        (fibo (- n 2)))))) ">>,
    Ctx = 'joxa-compiler':forms(Source, []),
    ?FORALL(FibTarget, range(0, 15),
            begin
                (is_binary('joxa-compiler':'get-context'(result, Ctx))
                 andalso (not 'joxa-compiler':'has-errors?'(Ctx))
                 andalso fibo(FibTarget) == 'jxat-fibonacci':fibo(FibTarget))
            end).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
fibo(0) -> 0 ;
fibo(1) -> 1 ;
fibo(N) when N > 0 -> fibo(N-1) + fibo(N-2) .

eratosthenes(Lim) -> eratosthenes(lists:seq(2,Lim), 1).
eratosthenes([], Primes) -> Primes;
eratosthenes([H|T], Primes) -> eratosthenes([X || X <- T, X rem H /= 0], Primes + 1).
