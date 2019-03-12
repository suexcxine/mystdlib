%%%----------------------------------------------------------------------
%%%
%%% @author chenduo
%%% @date 2014-07-25
%%% @doc dict的扩展
%%%
%%%----------------------------------------------------------------------
-module(dictex).
-export([get/2, get/3, get/4, findstore/3, store_list/3, erase_list/2]).
-export([update/3, keyupdate/4, keyupdate_withnew/4, keystore/5, init_if_notexist/3]).
-export([values/1, values/2]).
-export([foreach/2, map/2]).

get(Key, Dict) ->
    get(Key, Dict, undefined).

%% @doc 带默认值的取值
get(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
        error -> Default;
        {ok, Val} -> Val
    end.

%% @doc 带默认值的取值
get(Key, Dict, N, Default) ->
    case dict:find(Key, Dict) of
        error -> Default;
        {ok, Val} -> element(N, Val)
    end.

%% @doc 有值时直接用,没值时存上默认值
findstore(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
        error ->
            Dict2 = dict:store(Key, Default, Dict),
            {Default, Dict2};
        {ok, Val} ->
            {Val, Dict}
    end.

%% @doc 将一个List存入一个字典
store_list(N, List, Dict) when is_list(List) ->
    lists:foldl(fun(I, Acc) -> dict:store(element(N, I), I, Acc) end, Dict, List).

%% @doc 删除指定的这些key
erase_list(KeyList, Dict) when is_list(KeyList) ->
    lists:foldl(fun(K, Acc) -> dict:erase(K, Acc) end, Dict, KeyList).

update(Key, Dict, Fun) when is_function(Fun, 1) ->
    case dict:find(Key, Dict) of
        error ->
            Dict;
        {ok, V} ->
            V2 = Fun(V),
            dict:store(Key, V2, Dict)
    end.

%% @doc 适用于存储record或tuple的字典,指定Key和元素位置更新
keyupdate(Key, N, Dict, Fun) when is_function(Fun, 1) ->
    case dict:find(Key, Dict) of
        error ->
            Dict;
        {ok, V} ->
            V2 = setelement(N, V, Fun(element(N, V))),
            dict:store(Key, V2, Dict)
    end;
keyupdate(Key, N, Dict, NewValue) ->
    case dict:find(Key, Dict) of
        error ->
            Dict;
        {ok, V} ->
            V2 = setelement(N, V, NewValue),
            dict:store(Key, V2, Dict)
    end.

%% @doc 适用于存储record或tuple的字典,指定Key和元素位置更新
keyupdate_withnew(Key, N, Dict, Fun) when is_function(Fun, 1) ->
    case dict:find(Key, Dict) of
        error ->
            {undefined, Dict};
        {ok, V} ->
            V2 = setelement(N, V, Fun(element(N, V))),
            {V2, dict:store(Key, V2, Dict)}
    end;
keyupdate_withnew(Key, N, Dict, NewValue) ->
    case dict:find(Key, Dict) of
        error ->
            {undefined, Dict};
        {ok, V} ->
            V2 = setelement(N, V, NewValue),
            {V2, dict:store(Key, V2, Dict)}
    end.

%% @doc 适用于存储record或tuple的字典,指定Key和元素位置更新
%%      没有Key对应的值时用Default填充
%% @end
keystore(Key, N, Dict, Fun, Default) when is_function(Fun, 1) ->
    case dict:find(Key, Dict) of
        error ->
            dict:store(Key, Default, Dict);
        {ok, V} ->
            V2 = setelement(N, V, Fun(element(N, V))),
            dict:store(Key, V2, Dict)
    end;
keystore(Key, N, Dict, NewValue, Default) ->
    case dict:find(Key, Dict) of
        error ->
            dict:store(Key, Default, Dict);
        {ok, V} ->
            V2 = setelement(N, V, NewValue),
            dict:store(Key, V2, Dict)
    end.

%% @doc 没有值时初始化, 已有值就算了
init_if_notexist(Key, Default, Dict) ->
    case dict:find(Key, Dict) of
        error -> dict:store(Key, Default, Dict);
        {ok, _} -> Dict
    end.

%% @doc 字典里的值列表
values(Dict) ->
    dict:fold(fun(_, V, Acc) -> [V|Acc] end, [], Dict).

%% @doc 字典里的值列表
values(Dict, Filter) ->
    dict:fold(fun(K, V, Acc) ->
        case Filter(K, V) of
            true -> [V|Acc];
            false -> Acc
        end
    end, [], Dict).

%% @doc 接受二元函数Fun(Key, Value), 类似lists:foreach, 只为副作用, 返回值无效
foreach(Fun, Dict) when is_function(Fun, 2) ->
    dict:fold(fun(K, V, _) -> Fun(K, V) end, ok, Dict).

map(Fun, Dict) ->
    dict:fold(fun(K, V, Acc) ->
        dict:store(K, Fun(K, V), Acc)
    end, Dict, Dict).

