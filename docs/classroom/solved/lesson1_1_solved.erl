-module(lesson1_1_solved).
-export([hello/0, average/2, average/3, average/4, test/0]).

hello() -> io:format("Hello World!~n").

average(A, B) -> (A + B) / 2.
average(A, B, C) -> (A + B + C) / 3.
average(A, B, C, D) -> (A + B + C + D) / 4.

% Ax^2 + Bx + C = 0
%     -B +/- sqrt(B^2 - 4AC)
% x = ----------------------
%              2A
quadratic(0, _B, _C) ->
    io:format("Not a quadratic~n");

quadratic(A, B, C) when 4*A*C > B*B ->
    io:format("Complex Roots~n");

quadratic(A, B, C) ->
    Temp = math:sqrt(B*B - 4*A*C),
    R1 = (-1*B + Temp) / 2*A,
    R2 = (-1*B - Temp) / 2*A,
    io:format("R1=~p R2=~p~n",[R1,R2]).

series(Start, Stop) when Start > Stop ->
    io:format("~n");

series(Start, Stop) ->
    io:format("~p ",[Start]),
    series(Start+1, Stop).

pair_process(A, B, Lambda) ->
    Lambda(A, B).

test() ->
    quadratic(2,3,-4),
    quadratic(1,0,-4),
    quadratic(2,3,4),
    quadratic(0,3,1),
    series(1,10),
    series(3,15),
    1024.0 = pair_process(2,10,fun math:pow/2),
    12 = pair_process(2,10,fun (X,Y) -> X + Y end),
    104 = pair_process(2,10,fun (X,Y) -> X*X - Y*Y end),
    pair_process(2,10,fun series/2),
    pass.


