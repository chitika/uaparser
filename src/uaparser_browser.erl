-module(uaparser_browser).
-compile({parse_transform, ct_expand}).
-include("uaparser.hrl").
-export([parse/1, get_browsers/0]).

-spec parse(UserAgent :: binary()) -> [{atom(), ua_value()}].
parse(UserAgent) ->
    make_proplist(find(UserAgent, get_browsers()), UserAgent).

-spec make_proplist(Browser :: #browser{}, UserAgent :: binary()) -> [{atom(), ua_value()}].
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

-spec get_version(Browser :: #browser{}, UserAgent :: binary()) -> {binary(), version_details()}.
get_version(_ = #browser{version_regex = undefined}, _UserAgent) ->
    {<<"0">>, []};
get_version(Browser = #browser{version_regex = RX}, UserAgent) ->
    case re:run(UserAgent, RX, [{capture, all_but_first, binary}]) of
        {match, [FullVersion|Rest]} ->
            {FullVersion, get_version_details(Rest)};
        nomatch ->
            error({badmatch, Browser, UserAgent})
    end.

-spec get_version_details(Details :: [binary()]) -> version_details().
get_version_details(Details) ->
    [ {K, uaparser_utils:bin_to_num(Detail)} || {K, Detail} <- do_get_version_details(Details) ].

-spec do_get_version_details([binary()]) -> [{atom(), binary()}].
do_get_version_details([Major, Minor, Build, Patch]) ->
    [{major, Major}, {minor, Minor}, {build, Build}, {patch, Patch}];
do_get_version_details([Major, Minor, Patch]) ->
    [{major, Major}, {minor, Minor}, {patch, Patch}];
do_get_version_details([Major, Minor]) ->
    [{major, Major}, {minor, Minor}];
do_get_version_details([Major]) ->
    [{major, Major}, {minor, <<"0">>}].

-spec get_browsers() -> [#browser{}].
get_browsers() ->
    ct_expand:term(uaparser_utils:load_browsers()).

-spec find(UserAgent :: binary(), Options :: [#browser{}] | []) -> 'undefined' | #browser{}.
find(UserAgent, [Browser|Rest]) ->
    case check_useragent(Browser, UserAgent) of
        undefined -> find(UserAgent, Rest);
        Result -> Result
    end;
find(_UserAgent, []) ->
    undefined.

-spec check_useragent(Browser :: #browser{}, UserAgent :: binary()) -> 'undefined' | #browser{}.
check_useragent(Browser = #browser{aliases = Aliases, exclusions = Exclusions, children = Children}, UserAgent) ->
    case contains(Aliases, UserAgent) of
        true ->
            case find(UserAgent, Children) of
                undefined ->
                    case contains(Exclusions, UserAgent) of
                        true -> undefined;
                        false -> Browser
                    end;
                Result ->
                    inherit(Result, Browser)
            end;
        false ->
            undefined
    end.

-spec contains(Tokens :: [binary()] | [], Binary :: binary()) -> boolean().
contains([Token|Tokens], Binary) ->
    binary:match(Binary, Token, []) =/= nomatch orelse contains(Tokens, Binary);
contains([], _Binary) ->
    false.

-spec inherit(Child :: #browser{}, Parent :: #browser{}) -> #browser{}.
inherit(Child = #browser{version_regex = undefined}, _Parent = #browser{version_regex = RX}) ->
    Child#browser{version_regex = RX};
inherit(Child, _Parent) ->
    Child.
