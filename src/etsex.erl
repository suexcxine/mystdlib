%%%----------------------------------------------------------------------
%%%
%%% @author chenduo
%%% @date 2015-05-11
%%% @doc ets的扩展
%%%
%%%----------------------------------------------------------------------
-module(etsex).
-export([ets_update/4, ets_raw_update/4, ets_take/2, ets_foreach/2, ets_select/8]).
-export([sync/3]).

%% @doc 更新god_data中的某一个Key对应的tuple/record的某一项
ets_update(Mod, Key, N, Fun) when is_function(Fun) ->
    case Mod:get(Key) of
        undefined -> ok;
        E -> Mod:put(Key, setelement(N, E, Fun(element(N, E)))), ok
    end;
ets_update(Mod, Key, N, Val) ->
    case Mod:get(Key) of
        undefined -> ok;
        E -> Mod:put(Key, setelement(N, E, Val)), ok
    end.

ets_raw_update(Tab, Key, N, Val) ->
    case ets:lookup(Tab, Key) of
        [E] -> ets:insert(Tab, setelement(N, E, Val));
        _ -> ok
    end.

ets_take(Ets, N) ->
    ets_take(Ets, N, ets:first(Ets), 0, []).

ets_take(_Ets, _N, '$end_of_table', _, Acc) ->
    Acc;
ets_take(_Ets, N, _, N, Acc) ->
    Acc;
ets_take(Ets, N, Key, AccN, Acc) ->
    ets_take(Ets, N, ets:next(Ets, Key), AccN + 1, [Key|Acc]).

ets_foreach(Ets, Fun) ->
    ets_foreach(Ets, Fun, ets:first(Ets)).

ets_foreach(_Ets, _Fun, '$end_of_table') ->
    ok;
ets_foreach(Ets, Fun, Key) ->
    case ets:lookup(Ets, Key) of
        [E] -> Fun(E);
        _ -> ok
    end,
    ets_foreach(Ets, Fun, ets:next(Ets, Key)).

%% @doc 从Ets表里取出N个元素,每次取SN个,
%%      Fun: match | match_object | select | etc..
%%      MS: match spec | match pattern
%%      Pred: 用于取出SN个后进一步过滤的Fun
%%      CLF: 用于取Continuation的Fun
%%      CSF: 用于存Continuation的Fun
ets_select(Ets, Fun, N, SN, MS, Pred, CLF, CSF) ->
    Continuation = CLF(),
    ets_select(Ets, Fun, N, SN, MS, Pred, CLF, CSF, Continuation, 0, []).
ets_select(_, _, N, _, _, _, _, CSF, Continuation, EndCnt, Acc) when length(Acc) >= N; EndCnt >= 2 ->
    % EndCnt >= 2 意味着再扫下去也没意义了, 已经过了两次 end_of_table 了
    CSF(Continuation),
    Acc2 = sets:to_list(sets:from_list(Acc)),
    randex:rand_pick(Acc2, N);
ets_select(Ets, Fun, N, SN, MS, Pred, CLF, CSF, Continuation, EndCnt, Acc) ->
    {EndCnt2, SelectResult} = case Continuation of
        '$end_of_table' ->
            {EndCnt + 1, ets:Fun(Ets, MS, SN)};
        undefined ->
            {EndCnt + 1, ets:Fun(Ets, MS, SN)};
        Continuation ->
            {EndCnt, ets:Fun(Continuation)}
    end,
    case SelectResult of
        {Matches, '$end_of_table'} ->
            Matches2 = lists:filter(Pred, Matches),
            ets_select(Ets, Fun, N, SN, MS, Pred, CLF, CSF, '$end_of_table', EndCnt2 + 1, Matches2 ++ Acc);
        {Matches, Continuation2} ->
            Matches2 = lists:filter(Pred, Matches),
            ets_select(Ets, Fun, N, SN, MS, Pred, CLF, CSF, Continuation2, EndCnt2, Matches2 ++ Acc);
        '$end_of_table' ->
            ets_select(Ets, Fun, N, SN, MS, Pred, CLF, CSF, '$end_of_table', EndCnt2 + 1, Acc)
    end.

sync(Nodes, Ets, K) ->
    V = Ets:get(K),
    rpc:eval_everywhere(Nodes, ets, insert, [Ets, {K, V}]),
    ok.

