-module(uaparser_browser).
-compile({parse_transform, ct_expand}).
-include("uaparser.hrl").
-export([parse/1, get_browsers/0]).

parse(UserAgent) ->
    make_proplist(find(UserAgent, get_browsers()), UserAgent).

make_proplist(undefined, UserAgent) ->
    make_proplist(#browser{}, UserAgent);
make_proplist(Browser = #browser{name = Name, family = Family, manufacturer = Manufacturer, browser_type = Type, renderer = RenderingEngine}, UserAgent) ->
    {Version, Details} = get_version(Browser, UserAgent),
    [
        {name,                  Name},
        {family,                Family},
        {manufacturer,          Manufacturer},
        {type,                  Type},
        {renderer,              RenderingEngine},
        {version,               Version},
        {version_details,       Details}
    ].

get_version(_ = #browser{version_regex = undefined}, _UserAgent) ->
    {<<"0">>, []};
get_version(Browser = #browser{version_regex = RX}, UserAgent) ->
    case re:run(UserAgent, RX, [{capture, all_but_first, binary}]) of
        {match, [FullVersion|Rest]} ->
            {FullVersion, get_version_details(Rest)};
        nomatch ->
            error({badmatch, Browser, UserAgent})
    end.

get_version_details(Details) ->
    [ {K, uaparser_utils:bin_to_num(Detail)} || {K, Detail} <- do_get_version_details(Details) ].

do_get_version_details([Major, Minor, Build, Patch]) ->
    [{major, Major}, {minor, Minor}, {build, Build}, {patch, Patch}];
do_get_version_details([Major, Minor, Patch]) ->
    [{major, Major}, {minor, Minor}, {patch, Patch}];
do_get_version_details([Major, Minor]) ->
    [{major, Major}, {minor, Minor}];
do_get_version_details([Major]) ->
    [{major, Major}, {minor, <<"0">>}].

get_browsers() ->
    ct_expand:term(uaparser_utils:load_browsers()).

find(UserAgent, [Browser|Rest]) ->
    case check_useragent(Browser, UserAgent) of
        undefined -> find(UserAgent, Rest);
        Result -> Result
    end;
find(_UserAgent, []) ->
    undefined.

check_useragent(Browser = #browser{aliases = Aliases, exclusions = Exclusions, children = Children}, UserAgent) ->
    case is_in_useragent(Aliases, UserAgent) of
        true ->
            case find(UserAgent, Children) of
                undefined ->
                    case contains_exclude_token(Exclusions, UserAgent) of
                        true -> undefined;
                        false -> Browser
                    end;
                Result ->
                    inherit(Result, Browser)
            end;
        false ->
            undefined
    end.

is_in_useragent(null, _UserAgent) ->
    false;
is_in_useragent([], _UserAgent) ->
    false;
is_in_useragent(Aliases, UserAgent) ->
    lists:any(fun(Alias) -> contains(UserAgent, Alias) end, Aliases).

contains_exclude_token(null, _UserAgent) ->
    false;
contains_exclude_token([], _UserAgent) ->
    false;
contains_exclude_token(Exclusions, UserAgent) ->
    lists:any(fun(Token) -> contains(UserAgent, Token) end, Exclusions).

contains(Binary, Token) ->
    Result = case binary:match(Binary, Token, []) of
        nomatch -> false;
        _ -> true
    end,
    Result.

inherit(Child = #browser{version_regex = undefined}, _Parent = #browser{version_regex = RX}) ->
    Child#browser{version_regex = RX};
inherit(Child, _Parent) ->
    Child.
