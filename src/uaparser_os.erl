-module(uaparser_os).
-compile({parse_transform, ct_expand}).
-include("uaparser.hrl").
-export([parse/1, get_operating_systems/0]).

parse(UserAgent) ->
    make_proplist(find(UserAgent, get_operating_systems()), UserAgent).

make_proplist(undefined, UserAgent) ->
    make_proplist(#os{}, UserAgent);
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

find(UserAgent, [OS|Rest]) ->
    case check_useragent(OS, UserAgent) of
        undefined -> find(UserAgent, Rest);
        Result -> Result
    end;
find(_UserAgent, []) ->
    undefined.

check_useragent(OS = #os{aliases = Aliases, exclusions = Exclusions, children = Children}, UserAgent) ->
    case contains(Aliases, UserAgent) of
        true ->
            case find(UserAgent, Children) of
                undefined ->
                    case contains(Exclusions, UserAgent) of
                        true -> undefined;
                        false -> OS
                    end;
                Result ->
                    inherit(Result, OS)
            end;
        false ->
            undefined
    end.

contains([Token|Tokens], Binary) ->
    binary:match(Binary, Token, []) =/= nomatch orelse contains(Tokens, Binary);
contains([], _Binary) ->
    false.

inherit(Child = #os{version_regex = undefined}, _Parent = #os{version_regex = RX}) ->
    Child#os{version_regex = RX};
inherit(Child, _Parent) ->
    Child.
