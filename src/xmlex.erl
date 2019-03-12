%%%----------------------------------------------------------------------
%%%
%%% @author chenduo
%%% @date 2015-08-12
%%% @doc xmerl的扩展
%%%
%%%----------------------------------------------------------------------
-module(xmlex).
-include_lib("xmerl/include/xmerl.hrl").
-export([get_attr/2, get_attr/3, get_elem/2, strip_dom/1]).

%% @doc xml获取属性
get_attr(Id, Attrs, Default) ->
    case get_attr(Id, Attrs) of
        false ->
            Default;
        Val ->
            Val
    end.

get_attr(Id, #xmlElement{attributes = Attrs}) ->
    get_attr(Id, Attrs);
get_attr(Id, Attrs) when is_list(Attrs) ->
    case lists:keyfind(Id, #xmlAttribute.name, Attrs) of
        false ->
            false;
        #xmlAttribute{value = Value} ->
            Value
    end.

%% 获取某个节点
get_elem(Name, [#xmlElement{name = Name} = Elem | _T]) ->
    Elem;
get_elem(Name, [_|T]) ->
    get_elem(Name, T);
get_elem(_Name, []) ->
    false.

%% 清除text中的空格,'\n','\t'
strip_dom(#xmlElement{content = Content} = Elem) ->
    Content2 =
    lists:foldr(fun(Child, Acc) ->
        case strip_dom(Child) of
            skip -> Acc;
            Elem2 -> [Elem2 | Acc]
        end
    end, [], Content),
    Elem#xmlElement{content = Content2};
strip_dom(#xmlText{value = Val} = Text) ->
    case strip_space(Val) of
        "" -> skip;
        Stripped -> Text#xmlText{value = Stripped}
    end;
strip_dom(#xmlComment{}) ->
    skip;
strip_dom(Elem) ->
    Elem.

strip_space(Text) when is_list(Text) ->
    strip_space_left(strip_space_right(Text)).

-define(SPACES, "\r\n\t\s").
strip_space_left([H|T] = S)  ->
    case lists:member(H, ?SPACES) of
        true -> strip_space_left(T);
        false -> S
    end;
strip_space_left([]) ->
    [].

strip_space_right([H|T]) ->
    case lists:member(H, ?SPACES) of
        true ->
            case strip_space_right(T) of
                [] -> [];
                T2 -> [H|T2]
            end;
        false ->
            [H | strip_space_right(T)]
    end;
strip_space_right([]) ->
    [].

