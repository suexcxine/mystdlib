%%%----------------------------------------------------------------------
%%%
%%% @author chenduo
%%% @date 2015-03-25
%%% @doc random的扩展
%%%
%%%----------------------------------------------------------------------
-module(randex).
-export([rand/0, rand/1, rand/2, rand_ex/1, rand_pick/1, rand_pick/2]).
-export([pick_index_by_weight/1, pick_index_by_weight/2, pick_by_weight/1, pick_by_prob/1]).
-export([hit_test/1, hit_test/2]).
-export([draw/2]).

%% @doc 返回1-10000之间的一个数字
rand() ->
    rand(10000).

%% @doc 返回一个随机数(结果为1-N)
rand(0) -> 0;
rand(N) when N > 0 ->
    rand:uniform(N).

rand_ex(0) -> 0;
rand_ex(N) when N > 0 ->
    rand:uniform(N).

%% @doc 返回两个数之间的随机数
rand(Min, Min) ->
    Min;
rand(Min, Max) ->
    rand(Max - Min + 1) + Min - 1.

%% @doc 在列表中返回1个
rand_pick([_|_] = List) ->
    Pos = rand(length(List)),
    lists:nth(Pos, List);
rand_pick(Contents) when is_tuple(Contents) ->
    element(rand:uniform(tuple_size(Contents)), Contents).

%% @doc 在列表中随机返回N个
%% @doc 在一个列表中获取N个可用的项
rand_pick(List, N) ->
    rand_pick(List, N, []).

% N个位置已经找到
rand_pick(_, 0, Result) ->
    Result;
% 遍历结束，还是没有找到N个，则返回Result
rand_pick([], _N, Result) ->
    Result;
rand_pick(List, N, Result) ->
    Len = erlang:length(List),
    Pick = rand(Len),
    Pos = lists:nth(Pick, List),
    rand_pick(List -- [Pos], N-1, [Pos | Result]).

%% @doc 依据权值选择
%% 列表格式:[{对象, 权值}]
%% 返回{对象,位置}
%% 每项的概率为  权值/权值总和
pick_by_weight(List) ->
    WeightList = [Weight || {_Obj, Weight} <- List],
    Point = rand_ex(lists:sum(WeightList)),
    do_pick_by_prob(List, 0, Point, 1).

%% @doc 依据概率选择
%% 列表格式:[{对象,概率}]
%% 返回{对象,位置}
pick_by_prob(List) -> %10000为基准
    Point = rand(),
    do_pick_by_prob(List, 0, Point, 1).

do_pick_by_prob([], _Cur, _Point, _Index)->
    false;
do_pick_by_prob([{Obj, Range} | Rest], Cur, Point, Index) ->
    if
        Cur < Point andalso Point =< (Range + Cur) ->
            {Obj, Index};
        true ->
            do_pick_by_prob(Rest, Range + Cur, Point, Index + 1)
    end.

%% 例如: [3000,3000,3000,500,500] -> [4,2,5]
pick_index_by_weight(L) ->
    pick_index_by_weight(L, 1).
pick_index_by_weight(L, N) ->
    L2 = lists:zip(lists:seq(1, length(L)), L),
    draw(L2, N).

%% @doc 从列表中按概率抽取N个
%%      抽牌方式(即抽一个就从随机池中拿掉,然后继续随机下一个)
%%      如果总数不够(如只有3个要抽5个), 就直接把列表返回, 不用抽了
%% @end
draw(L, N) ->
    if
        length(L) =< N -> [Obj || {Obj, _} <- L];
        true -> draw(L, N, [])
    end.

draw([], _N, Acc) ->
    Acc;
draw(_L, 0, Acc) ->
    Acc;
draw(L, N, Acc) ->
    {Obj, Index} = pick_by_weight(L),
    draw(listsex:removeat(Index, L), N - 1, [Obj|Acc]).

%% @doc 是否命中
hit_test(Prob) ->
    rand(10000) =< Prob.

hit_test(ProbFull, Prob) ->
    rand(ProbFull) =< Prob.

