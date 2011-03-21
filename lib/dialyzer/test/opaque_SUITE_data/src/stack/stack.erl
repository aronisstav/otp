-module(stack).

-export([new/0, push/2, pop/1]).

-opaque stack() :: list().

-spec new() -> stack().

new() -> [].

-spec push(_, stack()) -> stack().

push(A, B) -> [A|B].

-spec pop(stack()) -> {_, stack()} | empty.

pop([]) -> empty;
pop([A,B]) -> {A, B}.
