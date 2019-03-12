%%%----------------------------------------------------------------------
%%%
%%% @author chenduo
%%% @date 2013-11-13
%%% @doc lists的扩展
%%%
%%%----------------------------------------------------------------------
-module(listsex).
-export([for/2, foracc/3, while/2, while/3, untilok/2, untilerror/2, untilerror/3, untilerror_n/3]).
-export([take_one/2, find/2, find_n/3, search_n/3, takewhile_n/3, mapfind/2, mapfind/3, foldlwhile/3, foldwhile/4, findchange/5]).
-export([freplace/2, freplace_one/2]).
-export([mapindex/2]).

-export([count/2, fcount/2, fcountstop/2, takecount/1, keymerge/1, keymerge/2]).
-export([indexof/2, findexof/2, insertat/3, removeat/2, takeat/2, replaceat/3, exchangeat/3, swapat/3]).
-export([insertsorted/2, insertsortedf/3]).
-export([keycount/3, keyfoldl/5, keyfoldr/5, keyappend/4, keyupdate/4, keyupdate/5, keystore/4, keystore/6, keyfind/4, keyfind/5, propget/2, propget/3]).
-export([keymapfind/3, keymapmember/3, keymapstore/4]).
-export([classify/2, classify/4, classify/5, classify_sorted/3]).
-export([keyclassify/2, keyclassify/4, keyclassify/5]).

-export([randallot/2, randallot2/2, shuffle/1, shuffle2/1, shuffle3/1]).
-export([randstuff/2, randstuff_withlimit/3, randstuff_withlimit/4]).
-export([drawrand/1, drawrand/2, drawrand_index/1, drawrand_index/2]).
-export([rand_dissimilar_from_deeplist/1]).

-export([priority_accept/3, remove_duplicates/1, subtract/2, subtract_rmdup/2]).
-export([isallsame/1, key_isallsame/2]).

-export([pmap/2, split_into/2, splitwith2end/2, divide_every_two/1, divide_by_n/2, divide_n/2]).
-export([umerge/1, umerge/2]).
-export([append_rank/1]).
-export([concat/1, zip/2, unzip/1]).

-export([asc_sort/2, desc_sort/2, multi_sort/2]).

-export([get_by_page/3]).
-export([add_uniques/2, add_unique/2, noduplicates/1]).

-export([keymin_safe/2, keymin/2, keymax_safe/2, keymax/2]).
-export([keysum/2, keyavg/2]).
-export([max/1, max/2, min/2, last/1]).
-export([ensure_flat/1]).

keysum(N, L) ->
    lists:sum([element(N, I) || I <- L]).

keyavg(_N, []) ->
    0;
keyavg(N, L) ->
    keysum(N, L) / length(L).

keymin_safe(_N, []) ->
    [];
keymin_safe(N, [H|T]) ->
    [keymin(N, H, T)].

keymin(N, [H|T]) ->
    keymin(N, H, T).

keymin(_N, Min, []) ->
    Min;
keymin(N, Min, [H|T]) ->
    case element(N, H) of
        Val when Val < element(N, Min) ->
            keymin(N, H, T);
        _ ->
            keymin(N, Min, T)
    end.

keymax_safe(_N, []) ->
    [];
keymax_safe(N, [H|T]) ->
    [keymax(N, H, T)].

keymax(N, [H|T]) ->
    keymax(N, H, T).

keymax(_N, Min, []) ->
    Min;
keymax(N, Min, [H|T]) ->
    case element(N, H) of
        Val when Val > element(N, Min) ->
            keymax(N, H, T);
        _ ->
            keymax(N, Min, T)
    end.

%% @doc 执行指定函数N次
for(_Fun, N) when N =< 0 ->
    ok;
for(Fun, N) ->
    Fun(),
    for(Fun, N - 1).

%% @doc 相当于 for index, value do xxx in L
mapindex(Fun, L) ->
    {_, L2} = lists:foldl(fun(I, {Idx, Acc}) -> {Idx + 1, [Fun(Idx, I)|Acc]} end, {1, []}, L),
    lists:reverse(L2, []).

%% @doc 执行指定函数N次并fold结果
foracc(_, 0, Acc) ->
    Acc;
foracc(Fun, N, Acc) ->
    foracc(Fun, N-1, Fun(Acc)).

%% @doc 执行指定函数直至条件不满足
while(F, Acc) ->
    case F(Acc) of
        false ->
            Acc;
        Acc2 ->
            while(F, Acc2)
    end.

%% @doc 执行指定函数直至条件不满足
while(Pred, F, Acc) ->
    while(Pred, F, F(Acc), Acc).
while(Pred, F, Next, Acc) ->
    case Pred(Next) of
        true ->
            while(Pred, F, F(Next), Next);
        false ->
            Acc
    end.

untilok(_Fun, []) ->
    error(badarg);
untilok(Fun, [H|T]) ->
    try
        Fun(H)
    catch
        _:_ ->
            untilok(Fun, T)
    end.

%% @doc 执行指定函数直至出错
untilerror(Fun, Acc) ->
    case catch Fun(Acc) of
        {error, Reason} -> {Reason, Acc};
        Acc2 -> untilerror(Fun, Acc2)
    end.

untilerror(_Fun, Acc, []) ->
    {ok, Acc};
untilerror(Fun, Acc, [H|T]) ->
    case catch Fun(H, Acc) of
        {error, Reason} -> {Reason, Acc};
        Acc2 -> untilerror(Fun, Acc2, T)
    end.

untilerror_n(_, Acc, 0) ->
    {ok, Acc};
untilerror_n(Fun, Acc, N) ->
    case catch Fun(Acc) of
        {error, Reason} -> {Reason, Acc};
        Acc2 -> untilerror_n(Fun, Acc2, N - 1)
    end.

freplace(F, L) ->
    freplace(F, L, []).
freplace(_F, [], Acc) ->
    lists:reverse(Acc, []);
freplace(F, [H|T], Acc) ->
    case F(H) of
        {ok, Val} -> freplace(F, T, [Val|Acc]);
        false -> freplace(F, T, [H|Acc])
    end.

freplace_one(F, L) ->
    freplace_one(F, L, []).
freplace_one(_F, [], _Acc) ->
    false;
freplace_one(F, [H|T], Acc) ->
    case F(H) of
        {ok, Val} -> lists:reverse([Val|Acc], T);
        false -> freplace_one(F,T,  [H|Acc])
    end.

%% @doc 返回第一个匹配的元素
take_one(Pred, L) ->
    take_one(Pred, L, []).

take_one(Pred, [H|T], Acc) ->
    case Pred(H) of
        true ->
            {value, H, lists:reverse(Acc, T)};
        false ->
            take_one(Pred, T, [H|Acc])
    end;
take_one(_, [], _Acc) ->
    false.

%% @doc 返回第一个匹配的元素
find(Pred, [H|T]) ->
    case Pred(H) of
        true ->
            H;
        false ->
            find(Pred, T)
    end;
find(_, []) ->
    false.

%% @doc 找出N个匹配的元素
find_n(N, Pred, [H|T]) ->
    find_n(N, Pred, [H|T], 0, []).

find_n(N, _, _, N, Acc) ->
    % 找够了
    lists:reverse(Acc, []);
find_n(_, _, [], _, Acc) ->
    % 没有了
    lists:reverse(Acc, []);
find_n(N, Pred, [H|T], AccN, Acc) ->
    case Pred(H) of
        true ->
            find_n(N, Pred, T, AccN + 1, [H|Acc]);
        false ->
            find_n(N, Pred, T, AccN, Acc)
    end.

%% @doc From - To找出N个匹配的元素
search_n(N, F, To) ->
    search_n(N, F, 1, To, 0, []).

search_n(N, _, _, _, AccN, Acc) when AccN >= N ->
    % 找够了
    lists:reverse(Acc, []);
search_n(_, _, Cur, To, _, Acc) when Cur > To ->
    % 没有了
    lists:reverse(Acc, []);
search_n(N, F, Cur, To, AccN, Acc) ->
    case F(Cur) of
        false ->
            search_n(N, F, Cur + 1, To, AccN, Acc);
        Val ->
            search_n(N, F, Cur + 1, To, AccN + 1, [Val|Acc])
    end.

%% @doc From - To找出N个匹配的元素, 遇到false即停止
takewhile_n(N, F, To) ->
    takewhile_n(N, F, 1, To, 0, []).

takewhile_n(N, _, _, _, AccN, Acc) when AccN >= N ->
    % 找够了
    lists:reverse(Acc, []);
takewhile_n(_, _, Cur, To, _, Acc) when Cur > To ->
    % 没有了
    lists:reverse(Acc, []);
takewhile_n(N, F, Cur, To, AccN, Acc) ->
    case F(Cur) of
        false ->
            lists:reverse(Acc, []);
        Val ->
            takewhile_n(N, F, Cur + 1, To, AccN + 1, [Val|Acc])
    end.

%% @doc 返回第一个执行指定函数后结果不为Default的结果
mapfind(Fun, [H|T]) ->
    mapfind(Fun, [H|T], false);
mapfind(_Fun, []) ->
    false.

mapfind(Fun, [H|T], Default) ->
    case Fun(H) of
        Default ->
            mapfind(Fun, T, Default);
        Val ->
            Val
    end;
mapfind(_, [], Default) ->
    Default.

%% @doc 数个数
count(E, L) ->
    count(E, L, 0).
count(_E, [], Acc) ->
    Acc;
count(E, [E|T], Acc) ->
    count(E, T, Acc + 1);
count(E, [_|T], Acc) ->
    count(E, T, Acc).

%% @doc 数个数
fcount(F, L) ->
    fcount(F, L, 0).
fcount(_F, [], Acc) ->
    Acc;
fcount(F, [H|T], Acc) ->
    case F(H) of
        true ->
            fcount(F, T, Acc + 1);
        false ->
            fcount(F, T, Acc)
    end.

%% @doc 数个数(遇到false停止
fcountstop(F, L) ->
    fcountstop(F, L, 0).
fcountstop(_F, [], Acc) ->
    Acc;
fcountstop(F, [H|T], Acc) ->
    case F(H) of
        true ->
            fcountstop(F, T, Acc + 1);
        false ->
            Acc
    end.
%% @doc 统计列表各元素个数
%% e.g [1, 2, 3, 4, 2, 1, 3] -> [{1,2},{2,2},{3,2},{4,1}]
takecount(L) ->
	takecount(L, []).
takecount([], Acc) ->
	Acc;
takecount([H|T], Acc) ->
	case lists:keytake(H, 1, Acc) of
		false ->
			takecount(T, [{H, 1} | Acc]);
		{value, {H, N}, Acc2} ->
			takecount(T, [{H, N + 1} | Acc2])
	end.

%%---------------------------------------------------------------
%% 位置相关
%%---------------------------------------------------------------

%% @doc 返回给定元素在列表中的位置,从1开始
indexof(Ele, L) ->
    indexof(Ele, L, 1).
indexof(Ele, [Ele|_T], Idx) ->
    Idx;
indexof(Ele, [_|T], Idx) ->
    indexof(Ele, T, Idx + 1);
indexof(_, [], _) ->
    false.

%% @doc 返回符合给定条件的元素在列表中的位置,从1开始
findexof(Fun, L) when is_function(Fun, 1) ->
    findexof(Fun, L, 1).
findexof(Fun, [H|T], Idx) ->
    case Fun(H) of
        true ->
            Idx;
        false ->
            findexof(Fun, T, Idx + 1)
    end;
findexof(_, [], _) ->
    false.

%% @doc 向list中指定位置插入元素
insertat(E, Idx, L) ->
    insertat(E, Idx, L, [], 1).
insertat(_, _, [], Acc, _) ->
    lists:reverse(Acc);
insertat(E, Idx, [H|T], Acc, Count) when Count =:= Idx ->
    lists:reverse([E|Acc], [H|T]);
insertat(E, Idx, [H|T], Acc, Count) ->
    insertat(E, Idx, T, [H|Acc], Count+1).

%% @doc 从list中删除指定位置的元素
removeat(Idx, L) ->
    removeat(Idx, L, [], 1).
removeat(_, [], Acc, _) ->
    lists:reverse(Acc);
removeat(Idx, [_|T], Acc, Count) when Count =:= Idx ->
    lists:reverse(Acc, T);
removeat(Idx, [H|T], Acc, Count) ->
    removeat(Idx, T, [H|Acc], Count+1).

%% @doc 从list中删除指定位置的元素,返回删除后的列表和删掉的元素
takeat(Idx, L) ->
    takeat(Idx, L, [], 1).
takeat(_, [], Acc, _) ->
    {false, lists:reverse(Acc)};
takeat(Idx, [H|T], Acc, Count) when Count =:= Idx ->
    {H, lists:reverse(Acc, T)};
takeat(Idx, [H|T], Acc, Count) ->
    takeat(Idx, T, [H|Acc], Count+1).

%% @doc 替换list中指定位置的元素
replaceat(Idx, L, Val) ->
	replaceat(Idx, L, [], 1, Val).
replaceat(_, [], Acc, _, _) ->
	lists:reverse(Acc);
replaceat(Idx, [_|T], Acc, Count, Val) when Count =:= Idx ->
    lists:reverse(Acc, [Val|T]);
replaceat(Idx, [H|T], Acc, Count, Val) ->
	replaceat(Idx, T, [H|Acc], Count+1, Val).

%% @doc 替换list中指定位置的元素并返回旧值
exchangeat(Idx, L, Val) ->
	exchangeat(Idx, L, [], 1, Val).
exchangeat(_, [], _Acc, _, _) ->
    false;
exchangeat(Idx, [H|T], Acc, Count, Val) when Count =:= Idx ->
    {H, lists:reverse(Acc, [Val|T])};
exchangeat(Idx, [H|T], Acc, Count, Val) ->
	exchangeat(Idx, T, [H|Acc], Count+1, Val).

swapat(IdxA, IdxB, L) when IdxA < IdxB ->
    swapat(IdxA, IdxB, L, [], 1);
swapat(IdxA, IdxB, L) when IdxA > IdxB ->
    swapat(IdxB, IdxA, L);
swapat(_IdxB, _IdxA, L) ->
    L.

swapat(_, _, [], Acc, _) ->
    lists:reverse(Acc, []);
swapat(IdxA, IdxB, [H|T] = L, Acc, Count) when Count =:= IdxA ->
    case exchangeat(IdxB - IdxA, T, H) of
        {OutVal, T2} -> lists:reverse(Acc, [OutVal|T2]);
        false -> lists:reverse(Acc, L)
    end;
swapat(IdxA, IdxB, [H|T], Acc, Count) ->
    swapat(IdxA, IdxB, T, [H|Acc], Count+1).

%%-------------------------------------------------------------
%% 顺序相关
%%-------------------------------------------------------------

%% @doc 将元素插入一个列表,要求两个参数均有序,这样结果仍然保持有序
insertsorted(NewGoods, L) ->
    insertsorted(NewGoods, L, [], 1, []).
insertsorted([], T, Acc, _, Counts) ->
    {lists:reverse(Acc, T), lists:reverse(Counts, [])};
insertsorted([NH|NT], [H|_T]=L, Acc, Count, Counts) when NH < H ->
    insertsorted(NT, L, [NH|Acc], Count+1, [Count|Counts]);
insertsorted(Adds, [H|T], Acc, Count, Counts) ->
    insertsorted(Adds, T, [H|Acc], Count+1, Counts).

%% @doc 将元素插入一个列表,要求两个参数均有序,这样结果仍然保持有序
%%      返回值是新列表和插入的几个元素被插入的位置列表
%% @end
insertsortedf(Pred, NewGoods, L) ->
    insertsortedf(Pred, NewGoods, L, [], 1, []).
insertsortedf(_, NewGoods, [], Acc, Count, Counts) ->
    {lists:reverse(Acc, NewGoods), lists:reverse(Counts, lists:seq(Count, Count+length(NewGoods)-1))};
insertsortedf(_, [], T, Acc, _, Counts) ->
    {lists:reverse(Acc, T), lists:reverse(Counts, [])};
insertsortedf(Pred, [NH|NT] = Adds, [H|T]=L, Acc, Count, Counts) ->
    case Pred(NH, H) of
        true ->
            insertsortedf(Pred, NT, L, [NH|Acc], Count+1, [Count|Counts]);
        false ->
            insertsortedf(Pred, Adds, T, [H|Acc], Count+1, Counts)
    end.

%% @doc 直到Pred返回false之前一直fold
foldlwhile(Pred, Acc0, [H|T]) ->
    case Pred(H, Acc0) of
        {true, Acc} ->
            foldlwhile(Pred, Acc, T);
        {false, Acc} ->
            Acc
    end;
foldlwhile(_Pred, Acc, []) ->
    Acc.

%% @doc 直到Pred返回default之前一直fold,注意返回的列表逆序
foldwhile(Pred, Acc0, [H|T], Target) ->
    case Pred(H) of
        Target -> Target;
        Val -> foldwhile(Pred, [Val| Acc0], T, Target)
    end;
foldwhile(_Pred, Acc, [], _Target) ->
    Acc.

%% 尝试从[H|T]中找出Pred(H, Elem)不为default的值,并修改改值为value,如果没有找到,
%% 则将Elem加入表中
findchange(Pred, Acc0, [H|T], Elem, Default) ->
    case Pred(H, Elem) of
        Default -> findchange(Pred, [H|Acc0], T, Elem, Default);
        Val -> [Val|T] ++ Acc0
    end;
findchange(_Pred, Acc, [], Elem, _Default) ->
    [Elem| Acc].

%% @doc 将一个列表分成几个子列表
%%      e.g. [{a,1},{b,1},{a,2},{b,3},{c,1}] -> [{c,[1]},{b,[1,3]},{a,[1,2]}]
%% @end
classify(FC, L) ->
    classify(FC, L, [], fun(E, Acc) -> [E|Acc] end).

classify(FC, L, Acc0, FA) ->
    classify(FC, L, [], Acc0, FA).

classify(FC, L, TotalAcc, Acc0, FA) ->
    lists:reverse(lists:foldl(fun(Ele, Acc) ->
        Key = FC(Ele),
        case lists:keytake(Key, 1, Acc) of
            false ->
                [{Key, FA(Ele, Acc0)}|Acc];
            {value, {Key, OldVal}, T} ->
                [{Key, FA(Ele, OldVal)}|T]
        end
    end, TotalAcc, L)).

%% @doc 以Tuple中的第N个元素为Key,将一个列表分成几个子列表
%%      e.g. [{a,1},{b,1},{a,2},{b,3},{c,1}] -> [{c,[1]},{b,[1,3]},{a,[1,2]}]
%% @end
keyclassify(N, L) ->
    keyclassify(N, L, [], [], fun(E, Acc) -> [E|Acc] end).

keyclassify(N, L, Acc0, F) ->
    keyclassify(N, L, [], Acc0, F).

keyclassify(N, L, TotalAcc, Acc0, F) ->
    classify(fun(E) -> element(N, E) end, L, TotalAcc, Acc0, F).

%% @doc 按tuple的第一个元素进行数量合并
%% eg. [{a, 1}, {b, 2}, {a, 3}, {c, 2}, {b, 1}] -> [{a, 4}, {b, 3}, {c, 2}].
keymerge(L) ->
    keymerge([], L).

keymerge(L1, L2) ->
    classify(fun({X, _}) -> X end, L2, L1, 0, fun({_, N}, Acc) -> N + Acc end).

%% @doc 按Key数个数
keycount(Key, N, L) when is_integer(N), N > 0 ->
    keycount(Key, N, L, 0).
keycount(_Key, _N, [], Acc) ->
    Acc;
keycount(Key, N, [H|T], Acc) when element(N, H) =:= Key ->
    keycount(Key, N, T, Acc + 1);
keycount(Key, N, [_|T], Acc) ->
    keycount(Key, N, T, Acc).

%% @doc 按Key做fold
keyfoldl(_, _, _, Acc, []) ->
    Acc;
keyfoldl(Key, N, F, Acc, [H|T]) when element(N, H) =:= Key ->
    keyfoldl(Key, N, F, F(H, Acc), T);
keyfoldl(Key, N, F, Acc, [_|T]) ->
    keyfoldl(Key, N, F, Acc, T).

%% @doc 按Key做fold, 从右向左
keyfoldr(_, _, _, Acc, []) ->
    Acc;
keyfoldr(Key, N, F, Acc, [H|T]) when element(N, H) =:= Key ->
    F(H, keyfoldr(Key, N, F, Acc, T));
keyfoldr(Key, N, F, Acc, [_|T]) ->
    keyfoldr(Key, N, F, Acc, T).

%% @doc 按key取值
keyfind(Key, N, E, L) ->
    keyfind(Key, N, E, L, undefined).
keyfind(Key, N, E, L, D) ->
    case lists:keyfind(Key, N, L) of
        false ->
            D;
        Tuple ->
            element(E, Tuple)
    end.

%% @doc 相当于listsex:keyfind(Key, 1, 2, L)
propget(Key, L) ->
    propget(Key, L, undefined).
propget(Key, L, D) ->
    case lists:keyfind(Key, 1, L) of
        false ->
            D;
        Tuple ->
            element(2, Tuple)
    end.


%% @doc 按key加值到对应的list的头部
keyappend(Key, N, L, V) ->
    case lists:keyfind(Key, N, L) of
        false ->
            lists:keystore(Key, N, L, {Key, [V]});
        {Key, VL} ->
            lists:keystore(Key, N, L, {Key, [V|VL]})
    end.

%% @doc 按key更新
keyupdate(Key, N, L, Fun) when is_function(Fun, 1) ->
    case lists:keyfind(Key, N, L) of
        false ->
            L;
        Tuple ->
            lists:keyreplace(Key, N, L, Fun(Tuple))
    end.

%% @doc 更新List中的某一项的某一项
%% e.g: -record(troop, {pos, fac_id, round}).
%% keyupdate(2, #troop.fac_id, #troop.round, [{troop, 1, 1101, 2}, {troop, 2, 1103, 1}], 3) ->
%%     [{troop, 1, 1101, 2}, {troop, 2, 1103, 3}]
%% @end
keyupdate(Key, KN, EN, L, Fun) when is_function(Fun, 1) ->
    case lists:keyfind(Key, KN, L) of
        false -> L;
        Tuple ->
            Tuple2 = setelement(EN, Tuple, Fun(element(EN, Tuple))),
            lists:keyreplace(Key, KN, L, Tuple2)
    end;
keyupdate(Key, KN, EN, L, NewEle) ->
    case lists:keyfind(Key, KN, L) of
        false -> L;
        Tuple ->
            Tuple2 = setelement(EN, Tuple, NewEle),
            lists:keyreplace(Key, KN, L, Tuple2)
    end.

keystore(Key, L, Fun, Default) when is_function(Fun, 1) ->
    keystore(Key, 1, 2, L, Fun, Default).

keystore(Key, KN, EN, L, Fun, Default) when is_function(Fun, 1) ->
    case lists:keyfind(Key, KN, L) of
        false -> lists:keystore(Key, KN, L, Default);
        Tuple ->
            Tuple2 = setelement(EN, Tuple, Fun(element(EN, Tuple))),
            lists:keyreplace(Key, KN, L, Tuple2)
    end.

%%----------------------------------------------------------------------------
%% 优先级条件取子列表
%%----------------------------------------------------------------------------

%% @doc 按优先级过滤列表,要求得到N个元素;
%%      当满足所有条件的元素不足N个时,降格以求,去掉第一个条件继续检索;
%%      仍不足时,继续降格以求,但最后一个条件必须满足;
%%      得到N个元素时返回
%%      一个很重要的考虑是,List可能很大,发现N个时立即返回以满足性能要求,不要对List做全遍历
%% @end
priority_accept(L, FilterList, N) ->
    priority_accept(L, FilterList, N, []).
priority_accept([], _, _, Acc) -> % 只有这些了
    Acc;
priority_accept(_, [], _, Acc) -> % 只能这样了,不能再降格以求了
    Acc;
priority_accept(_, _, 0, Acc) -> % 找齐了
    Acc;
priority_accept(L, [_|T] = FilterList, N, Acc) ->
    {Left, GotN, Got} = take(L, N, FilterList),
    priority_accept(Left, T, N - GotN, Got ++ Acc).

%% @doc 从列表里取符合所有条件的N个元素
take(L, TargetN, FilterList) ->
    take(L, TargetN, FilterList, 0, [], []).
take([], _, _, AccN, Acc, LeftAcc) -> % 只有这些
    {LeftAcc, AccN, Acc};
take(_, TargetN, _, TargetN, Acc, LeftAcc) -> % 找齐了
    {LeftAcc, TargetN, Acc};
take([H|T], TargetN, FilterList, AccN, Acc, LeftAcc) ->
    case multi_filter(H, FilterList) of
        true ->
            take(T, TargetN, FilterList, AccN + 1, [H|Acc], LeftAcc);
        false ->
            take(T, TargetN, FilterList, AccN, Acc, [H|LeftAcc])
    end.

%% @doc 判断一个元素是否符合一个条件列表
multi_filter(Item, FilterList) ->
    lists:all(fun(Filter) -> Filter(Item) end, FilterList).

%% 没有想到这个实现还没有上面的实现效率高, 差了一倍
% %% @doc 按优先级过滤列表,要求得到N个元素;
% %%      当满足所有条件的元素不足N个时,降格以求,去掉第一个条件继续检索;
% %%      仍不足时,继续降格以求,但最后一个条件必须满足;
% %%      得到N个元素时返回
% %% @end
% priority_accept2(L, FilterList, Must, N) ->
%     priority_accept2(L, FilterList, Must, N, []).
% priority_accept2([], _, _, _, Acc) -> % 只有这些了
%     Acc;
% priority_accept2(_, _, _, 0, Acc) -> % 找齐了
%     Acc;
% priority_accept2(L, [], Must,  N, Acc) -> % 只能这样了,不能再降格以求了
%     {_, _, Got} = take2(L, N, [], Must),
%     Got ++ Acc;
% priority_accept2(L, [_|T] = FilterList, Must, N, Acc) ->
%     {Left, GotN, Got} = take2(L, N, FilterList, Must),
%     priority_accept2(Left, T, Must, N - GotN, Got ++ Acc).
%
% %% @doc 从列表里取符合所有条件的N个元素
% take2(L, TargetN, FilterList, Must) ->
%     take2(L, TargetN, FilterList, Must, 0, [], []).
% take2([], _, _, _, AccN, Acc, LeftAcc) -> % 只有这些
%     {LeftAcc, AccN, Acc};
% take2(_, TargetN, _, _, TargetN, Acc, LeftAcc) -> % 找齐了
%     {LeftAcc, TargetN, Acc};
% take2([H|T], TargetN, FilterList, Must, AccN, Acc, LeftAcc) ->
%     case multi_filter2(H, FilterList, Must) of
%         true ->
%             take2(T, TargetN, FilterList, Must, AccN + 1, [H|Acc], LeftAcc);
%         false ->
%             take2(T, TargetN, FilterList, Must, AccN, Acc, [H|LeftAcc]);
%         dead ->
%             take2(T, TargetN, FilterList, Must, AccN, Acc, LeftAcc)
%     end.
%
% %% @doc 判断一个元素是否符合一个条件列表
% multi_filter2(Item, FilterList, Must) ->
%     case Must(Item) of
%         false ->
%             dead;
%         true ->
%             lists:all(fun(Filter) -> Filter(Item) end, FilterList)
%     end.

%% @doc 去除重复的元素
remove_duplicates(L) ->
    sets:to_list(sets:from_list(L)).

%% @doc 减集, 保持原顺序
subtract(L1, L2) when is_list(L2) ->
    Set = gb_sets:from_list(L2),
    [E || E <- L1, not gb_sets:is_element(E, Set)];
subtract(L1, E) ->
    L1 -- [E].

%% @doc 减集并去重, 不保持原顺序
%%      e.g. ([3,4,5,6], [1,2,3,3,4]) -> [6,5]
%%           ([1,2,3,3,4], [3,4,5,6]) -> [2,1]
subtract_rmdup(L1, L2) ->
    sets:to_list(sets:subtract(sets:from_list(L1), sets:from_list(L2))).

%%----------------------------------------------------------------------------
%% 随机相关
%%----------------------------------------------------------------------------

%% @doc 将数随机地分配给列表中的元素
%%      e.g: 5, [a,b,c] -> [{a,1},{b,4}]
%% @end
randallot(Val, L) ->
    N = length(L),
    randallot(Val, L, N, lists:zip(L, lists:duplicate(N, 0))).
randallot(0, _, _, Acc) ->
    Acc;
randallot(Val, L, N, Acc) ->
    Key = lists:nth(rand:uniform(N), L),
    {_, OldVal} = lists:keyfind(Key, 1, Acc),
    Acc2 = lists:keyreplace(Key, 1, Acc, {Key, OldVal+1}),
    randallot(Val-1, L, N, Acc2).

%% @doc 将数随机地分配给列表中的元素, 另一种算法
%%      e.g: 5, [a,b,c] -> [{a,1},{b,2},{c,2}]
%% @end
randallot2(_, []) ->
    [];
randallot2(Val, [H]) ->
    [{H,Val}];
randallot2(0, L) ->
    N = length(L),
    lists:zip(L, lists:duplicate(N, 0));
randallot2(Val, [H|T]) ->
    V = rand:uniform(Val+1)-1, % 直接用uniform范围是[1-Val],这样写的范围是[0-Val]
    [{H, V}|randallot(Val - V, T)].

%% @doc 另一种算法,随机打乱一个列表(实测最快)
%%      测试代码: tc_avg(fun() -> listsex:shuffle(lists:seq(1, 100)) end, 10000).
%% @end
shuffle(L) ->
    List1 = [{rand:uniform(), X} || X <- L],
    List2 = lists:keysort(1, List1),
    [E || {_, E} <- List2].

%% @doc 又一种算法,随机打乱一个列表(实测第二快)
%%      测试代码: tc_avg(fun() -> listsex:shuffle(lists:seq(1, 100)) end, 10000).
%% @end
shuffle2(L) ->
    shuffle2(L, []).
shuffle2([], Acc) ->
    Acc;
shuffle2(L, Acc) ->
    {Leading, [H | T]} = lists:split(rand:uniform(length(L)) - 1, L),
    shuffle2(Leading ++ T, [H | Acc]).

%% @doc 随机打乱一个列表(实测最慢)
%%      测试代码: tc_avg(fun() -> listsex:shuffle(lists:seq(1, 100)) end, 10000).
%% @end
shuffle3(L) ->
    lists:sort(
    fun(_, _) ->
        rand:uniform(2) > 1
    end, L).

%% @doc 随机填充,即从候选列表中随机取数得到一个新列表,
%%      Size表示目标列表的长度
%% @end
randstuff(Candidates, Size) when Size >= 0  ->
    randstuff(Candidates, length(Candidates), Size, []).
randstuff(_, _, 0, Acc) ->
    Acc;
randstuff(Candidates, N, Size, Acc) ->
    randstuff(Candidates, N, Size - 1, [lists:nth(rand:uniform(N), Candidates)|Acc]).

%% @doc 随机填充,即从候选列表中随机取数得到一个新列表,
%%      Size表示目标列表的长度,
%%      Limit表示每个候选值能出现的最大次数,
%%      Size和Limit冲突时以Limit为准
%% @end
randstuff_withlimit(Candidates, Size, Limit, Tail) ->
    L1 = randstuff_withlimit(Candidates, Size, Limit),
    L = if length(Tail) > Size -> L1 ++ Tail; true -> Tail ++ L1 end,
    shuffle(L).
randstuff_withlimit(Candidates, Size, Limit) when Size >= 0, Size =< Limit ->
    % Size =< Limit 此时Limit无效
    randstuff(Candidates, Size);
randstuff_withlimit(Candidates, Size, Limit) when Size >= 0, Size >= length(Candidates) * Limit ->
    % 此情况下把所有能出的值出完也不够Size指定的数, 也就不用随机了
    shuffle(lists:append([lists:duplicate(Limit, C) || C <- Candidates]));
randstuff_withlimit(Candidates, Size, Limit) when Size >= 0, is_list(Candidates) ->
    randstuff_withlimit(Candidates, length(Candidates), Size, Limit, [], []).
randstuff_withlimit(_, _, 0, _, _, Acc) ->
    % 达到目标Size
    Acc;
randstuff_withlimit(_, 0, _, _, _, Acc) ->
    % Candidates耗尽
    Acc;
randstuff_withlimit(Candidates, N, Size, Limit, AccLimit, Acc) ->
    C = lists:nth(rand:uniform(N), Candidates),
    {NewCount, AccLimit2} = case lists:keytake(C, 1, AccLimit) of
        false ->
            {1, AccLimit};
        {value, {C, Count}, AccLimitTemp} ->
            {Count + 1, AccLimitTemp}
    end,
    case NewCount >= Limit of
        true ->
            % 已达到Limit, 去除该候选值
            randstuff_withlimit(Candidates -- [C], N - 1, Size - 1, Limit, [{C, NewCount}|AccLimit2], [C|Acc]);
        false ->
            randstuff_withlimit(Candidates, N, Size - 1, Limit, [{C, NewCount}|AccLimit2], [C|Acc])
    end.

%% @doc 从附有概率的列表中抽取几个值,每得到一个值即将其从列表中移除再进行下一次抽取,使用相对概率
%%      例如: drawrand([{a, 1000}, {b, 2000}, {c, 500}, {d, 4000}], 2) -> [b, d]
%% @end
drawrand(L) ->
    drawrand(L, 1).
drawrand(L, Round) ->
    drawrand(L, Round, []).
drawrand([], _, Acc) -> % 牌抽完了,没得抽了
    Acc;
drawrand(_, 0, Acc) -> % 抽够了,不抽了
    Acc;
drawrand(L, Round, Acc) ->
	{Obj, Pos} = randex:pick_by_weight(L),
    L2 = removeat(Pos, L),
	drawrand(L2, Round - 1, [Obj | Acc]).

%% @doc 例: drawrand_index([3000,2000,300,100,50], 3) -> [1,2,4]
drawrand_index(L) ->
    drawrand_index(L, 1).
drawrand_index(L, Round) ->
    L2 = lists:zip(lists:seq(1, length(L)), L),
    drawrand(L2, Round).

%% @doc 对每个元素随机选择
%% e.g. ([[{411002,6000},{411003, 4000}],[{411001, 3000}, {411002, 3000}, {411006, 4000}]]) -> [411002, 411006]
%% @end
rand_dissimilar_from_deeplist(L) when is_list(L) ->
    rand_dissimilar_from_deeplist(L, []);
rand_dissimilar_from_deeplist(L) when is_tuple(L) ->
    rand_dissimilar_from_deeplist(tuple_to_list(L), []).
rand_dissimilar_from_deeplist([], Acc) ->
    lists:reverse(Acc);
rand_dissimilar_from_deeplist([H|T], Acc) ->
    case lists:foldl(fun(Id, R) -> lists:keydelete(Id, 1, R) end, H, Acc) of
        [] ->
            rand_dissimilar_from_deeplist(T, Acc);
        L ->
            {Id, _} = randex:pick_by_weight(L),
            rand_dissimilar_from_deeplist(T, [Id | Acc])
    end.

%% 列表元素是否完全一样, false表示不完全相同, 如果完全相同则将元素返回
isallsame([]) ->
    false;
isallsame([H]) ->
    H;
isallsame([H|T]) ->
    case lists:all(fun(I) -> I =:= H end, T) of
        true -> H;
        false -> false
    end.

%% 列表元素tuple的某一项是否完全一样
key_isallsame(_N, []) ->
    false;
key_isallsame(N, [H]) ->
    element(N, H);
key_isallsame(N, [H|T]) ->
    Key = element(N, H),
    case lists:all(fun(I) -> element(N, I) =:= Key end, T) of
        true -> Key;
        false -> false
    end.

%% 将列表按类型和顺序切分成多个子列表
split_into(F, [H|T]) ->
    split_into2(F, T, F(H), [H], []).

split_into2(_F, [], Tag, AccSub, Acc) ->
    Acc2 = [{Tag, lists:reverse(AccSub)} | Acc],
    lists:reverse(Acc2);
split_into2(F, [H|T], Tag, AccSub, Acc) ->
    TagCur = F(H),
    case TagCur of
        Tag ->
            % 类型相同
            split_into2(F, T, Tag, [H|AccSub], Acc);
        _ ->
            % 类型不同
            AccSub2 = {Tag, lists:reverse(AccSub)},
            split_into2(F, T, TagCur, [H], [AccSub2|Acc])
    end.

%% 将list分为两个子列表,一个是返回true的一个是返回false
splitwith2end(F, List) ->
    splitwith2end(F, List, [], []).
splitwith2end(_F, [], Acc1, Acc2) ->
    {Acc1, Acc2};
splitwith2end(F, [H|T], Acc1, Acc2) ->
    case F(H) of
        true ->
            splitwith2end(F, T, [H|Acc1], Acc2);
        false ->
            splitwith2end(F, T, Acc1, [H|Acc2])
    end.

%% 两两分组
divide_every_two(L) ->
    divide_every_two(L, []).
divide_every_two([], Acc) ->
    {lists:reverse(Acc), []};
divide_every_two([H], Acc) ->
    {lists:reverse(Acc), [H]};
divide_every_two([H1, H2|T], Acc) ->
    Acc2 = [[H1, H2]|Acc],
    divide_every_two(T, Acc2).

%% 多线程执行lists:map
pmap(F, L) ->
    S = self(),
    %% make_ref() returns a unique reference
    %% we'll match on this later
    Ref = erlang:make_ref(),
    Pids = lists:map(fun(I) ->
                spawn(fun() -> do_f(S, Ref, F, I) end)
        end, L),
    %% gather the results
    gather(Pids, Ref).
do_f(Parent, Ref, F, I) ->
    Parent ! {self(), Ref, (catch F(I))}.
gather([Pid|T], Ref) ->
    receive
        {Pid, Ref, Ret} -> [Ret|gather(T, Ref)]
    end;
gather([], _) ->
    [].

%% 每N个一组
divide_by_n(N, List) ->
    divide_by_n(N, List, []).
divide_by_n(N, List, Acc) when length(List) > N ->
    {List1, List2} = lists:split(N, List),
    divide_by_n(N, List2, [List1|Acc]);
divide_by_n(_N, List, Acc) ->
    lists:reverse([List|Acc], []).

%% 等分为N组
divide_n(0, _List) ->
    [];
divide_n(N, List) ->
    divide_by_n(ceil(length(List) / N), List).

umerge(ListOfList) ->
    lists:foldl(fun(L, Acc) -> umerge(Acc, L) end, [], ListOfList).

umerge(L1, L2) ->
    umerge2(L2, lists:reverse(L1, [])).

%% TODO 看看能不能优化,现在每次lists:member都是遍历,看看能不能依靠已经有序这一点做优化
umerge2([], Acc) ->
    lists:reverse(Acc, []);
umerge2([H|T], Acc) ->
    case lists:member(H, Acc) of
        true -> umerge2(T, Acc);
        false -> umerge2(T, [H|Acc])
    end.


%% @doc 需要已排序,每一项末尾加个名次
append_rank(L) ->
    {L2, _} = lists:mapfoldl(fun(I, Acc) -> {erlang:append_element(I, Acc), Acc + 1} end, 1, L),
    L2.

%% @doc 比lists:concat好在,允许binary元素
concat(List) ->
    lists:flatmap(fun thing_to_list/1, List).

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_binary(X)  -> binary_to_list(X);
thing_to_list(X) when is_list(X)    -> X.

zip(L1, L2) ->
    zip(L1, L2, []).

zip([], _, Acc) ->
    lists:reverse(Acc, []);
zip(_, [], Acc) ->
    lists:reverse(Acc, []);
zip([H1|T1], [H2|T2], Acc) ->
    zip(T1, T2, [{H1, H2}|Acc]).

unzip(L) -> unzip(L, [], []).
unzip([{X, Y} | Ts], Xs, Ys) -> unzip(Ts, [X | Xs], [Y | Ys]);
unzip([[X, Y] | Ls], Xs, Ys) -> unzip(Ls, [X | Xs], [Y | Ys]);
unzip([], Xs, Ys) -> {lists:reverse(Xs), lists:reverse(Ys)}.

%% @doc 对列表升序排列
%% KeyFun 是从元素提交比较值的函数
asc_sort(List, KeyFun) when is_function(KeyFun) ->
    Compare = fun(Ele1, Ele2) -> KeyFun(Ele1) < KeyFun(Ele2) end,
    lists:sort(Compare, List).

%% @doc 对列表进行降序排列
%% %% KeyFun 是从元素提交比较值的函数
desc_sort(List, KeyFun) when is_function(KeyFun) ->
    Compare = fun(Ele1, Ele2) -> KeyFun(Ele1) > KeyFun(Ele2) end,
    lists:sort(Compare, List).

%% @doc 多级排序 参数：List：待排序列表，Funs：使用的排序函数列表
multi_sort(List, Funs) when is_list(List), is_list(Funs) ->
    Fun = fun(A, B) -> sort_fun(A, B, Funs) =< 0 end,
    lists:sort(Fun, List).

%% @doc 多级排序 要求函数列表中的每一个函数返回-1小于, 0等于, 1大于三种值
sort_fun(A, B, [Fun]) ->
    Fun(A, B);
sort_fun(A, B, [H|TFuns]) ->
    Result = H(A, B),
    % 如果相等的话就按其他条件继续比
    if Result =:= 0 -> sort_fun(A, B, TFuns); true -> Result end.

%% @doc 已经有序(即Id相同的都在一起)的列表如[[301,a,b,c],[301,c,d,f],[301,e,d,a],[302,d,f,e],[302,d,e,f],[303,a,c,d]]
%%      变成按Id分组的列表,如[{301,[{a,b,c},{c,d,f},{e,d,a}],{302,[{d,f,e},{d,e,f}]},{303,[{a,c,d}]}]
%%      FC: 传入元素如[301,a,b,c],返回{Id,Element}如{301, {a,b,c}}
%%      FS: 如何将Id和对应的元素列表组合在一起,如fun(Id, L) -> #cache_skill{id = Id, list = L} end
%% @end
classify_sorted([], _FC, _FS) ->
    [];
classify_sorted([H|T], FC, FS) ->
    {Id, E} = FC(H),
    classify_sorted(T, FC, FS, Id, [E], []).

classify_sorted([], _FC, FS, CurId, CAcc, Acc) ->
    [FS(CurId, CAcc)|Acc];
classify_sorted([H|T], FC, FS, CurId, CAcc, Acc) ->
    {Id, E} = FC(H),
    if
        Id =:= CurId ->
            % 当前Id还没完
            classify_sorted(T, FC, FS, CurId, [E|CAcc], Acc);
        true ->
            % 换下一个Id
            classify_sorted(T, FC, FS, Id, [E], [FS(CurId, CAcc)|Acc])
    end.

%% @doc 分页获取列表数据
get_by_page(Page, PerPage, List) when Page >= 1, is_integer(PerPage) ->
    Start = (Page - 1) * PerPage + 1,
    try
        lists:sublist(List, Start, PerPage)
    catch
        _:_ ->
            []
    end.

%% @doc 不重复的元素加进去
add_uniques([], L) ->
    L;
add_uniques([Val|T], L) ->
    add_uniques(T, add_unique(Val, L)).

%% @doc 不重复的元素加进去
add_unique(Val, L) ->
    case lists:member(Val, L) of
        true -> L;
        false -> [Val|L]
    end.

%% @doc 去重并保持顺序
noduplicates(L) ->
    noduplicates(L, []).
noduplicates([], Acc) ->
    lists:reverse(Acc);
noduplicates([H|T], Acc) ->
    case lists:member(H, Acc) of
        true ->
            noduplicates(T, Acc);
        false ->
            noduplicates(T, [H|Acc])
    end.

max(L) ->
    ?MODULE:max(L, 0).

max([], D) ->
    D;
max(L, _D) ->
    lists:max(L).

min([], D) ->
    D;
min(L, _D) ->
    lists:min(L).

keymapfind(_Val, _Key, []) ->
    false;
keymapfind(Val, Key, [H|T]) ->
    case maps:find(Key, H) of
        {ok, Val} -> H;
        _ -> keymapfind(Val, Key, T)
    end.

keymapmember(_Val, _Key, []) ->
    false;
keymapmember(Val, Key, [H|T]) ->
    case maps:find(Key, H) of
        {ok, Val} -> true;
        _ -> keymapmember(Val, Key, T)
    end.

keymapstore(Val, Key, L, Map) ->
    keymapstore(Val, Key, L, Map, []).
keymapstore(_Val, _Key, [], Map, Acc) ->
    lists:reverse([Map|Acc], []);
keymapstore(Val, Key, [H|T], Map, Acc) ->
    case maps:find(Key, H) of
        {ok, Val} -> lists:reverse([Map|Acc], T);
        _ -> keymapstore(Val, Key, T, Map, [H|Acc])
    end.

last([H]) ->
    H;
last([_|T]) ->
    last(T).

ensure_flat(X) when is_list(X) ->
    lists:flatten(X);
ensure_flat(X) ->
    [X].

