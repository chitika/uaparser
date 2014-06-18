-module(uaparser_os).
-compile({parse_transform, ct_expand}).
-include("uaparser.hrl").
-export([parse/1, get_operating_systems/0, load_operating_systems/0]).

parse(UserAgent) ->
    make_proplist(find(UserAgent, get_operating_systems()), UserAgent).

make_proplist(undefined, UserAgent) ->
    make_proplist(undefined, UserAgent);
make_proplist(OS = #os{name = Name, family = Family, manufacturer = Manufacturer, device_type = DeviceType}, UserAgent) ->
    {Version, Details} = get_version(OS, UserAgent),
    [
        {name,                  Name},
        {family,                Family},
        {manufacturer,          Manufacturer},
        {type,                  DeviceType},
        {version,               Version},
        {version_details,       Details}
    ].

get_version(_ = #os{version_regex = undefined}, _UserAgent) ->
    {<<"0">>, []};
get_version(OS = #os{version_regex = RX}, UserAgent) ->
    case re:run(UserAgent, RX, [{capture, all_but_first, binary}]) of
        {match, [FullVersion|Rest]} ->
            {FullVersion, get_version_details(Rest)};
        nomatch ->
            error({badmatch, OS, UserAgent})
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

get_operating_systems() ->
    ct_expand:term(uaparser_utils:load_operating_systems()).

load_operating_systems() ->
    case code:priv_dir(uaparser) of
        {error, bad_name} ->
            Dir = "priv/";
        Dir ->
            ok
    end,
    Path = filename:join(Dir, "operating_systems.json"),
    {ok, C} = file:read_file(Path),
    lists:map(fun uaparser_utils:jiffy_to_os/1, jiffy:decode(C)).

find(UserAgent, [OS|Rest]) ->
    case check_useragent(OS, UserAgent) of
        undefined -> find(UserAgent, Rest);
        Result -> Result
    end;
find(_UserAgent, []) ->
    undefined.

check_useragent(OS = #os{aliases = Aliases, exclusions = Exclusions, children = Children}, UserAgent) ->
    case is_in_useragent(Aliases, UserAgent) of
        true ->
            case find(UserAgent, Children) of
                undefined ->
                    case contains_exclude_token(Exclusions, UserAgent) of
                        true -> undefined;
                        false -> OS
                    end;
                Result ->
                    inherit(Result, OS)
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

inherit(Child = #os{version_regex = undefined}, _Parent = #os{version_regex = RX}) ->
    Child#os{version_regex = RX};
inherit(Child, _Parent) ->
    Child.
