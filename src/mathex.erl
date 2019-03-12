%%%----------------------------------------------------------------------
%%%
%%% @author chenduo
%%% @date 2015-03-25
%%% @doc math的扩展
%%%
%%%----------------------------------------------------------------------
-module(mathex).
-export([floor/1, ceil/1]).
-export([paracurve/5]).

%% @doc 向下取整
floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

%% @doc 向上取整
ceil(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

%% @doc 抛物线
paracurve(V, Ang, A, G, T) ->
    DX = V * T * math:cos(Ang) + 0.5 * A * T * T,
    DY = V * T * math:sin(Ang) + 0.5 * G * T * T,
    {DX, DY}.

