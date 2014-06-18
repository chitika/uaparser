-module(uaparser_utils).
-compile({parse_transform, ct_expand}).
-include("uaparser.hrl").
-export([
        bin_to_num/1,
        jiffy_to_browser/1,
        jiffy_to_os/1,
        load_browsers/0,
        load_operating_systems/0
    ]).

-type jiffy_basic() :: number() | binary().
-type jiffy_array() :: [jiffy_basic()|jiffy_array()|jiffy_struct()].
-type jiffy_struct() :: {[{binary(), jiffy_basic() | jiffy_struct() | jiffy_array()}]}.
-type jiffy_term() :: jiffy_basic()|jiffy_array()|jiffy_struct().

-spec jiffy_to_browser(jiffy_struct()) -> #browser{}.
jiffy_to_browser({Browser}) ->
    lists:foldl(fun jiffy_to_browser/2, #browser{}, Browser).

-spec jiffy_to_browser({binary(), jiffy_term()}, #browser{}) -> #browser{}.
jiffy_to_browser({<<"family">>, Family}, Browser) ->
    Browser#browser{family = binary_to_atom(Family, utf8)};
jiffy_to_browser({<<"manufacturer">>, Manufacturer}, Browser) ->
    Browser#browser{manufacturer = binary_to_atom(Manufacturer, utf8)};
jiffy_to_browser({<<"name">>, Name}, Browser) ->
    Browser#browser{name = Name};
jiffy_to_browser({<<"aliases">>, Aliases}, Browser) ->
    Browser#browser{aliases = Aliases};
jiffy_to_browser({<<"exclusions">>, Exclusions}, Browser) ->
    Browser#browser{exclusions = Exclusions};
jiffy_to_browser({<<"browser_type">>, BrowserType}, Browser) ->
    Browser#browser{browser_type = binary_to_atom(BrowserType, utf8)};
jiffy_to_browser({<<"rendering_engine">>, RenderingEngine}, Browser) ->
    Browser#browser{renderer = binary_to_atom(RenderingEngine, utf8)};
jiffy_to_browser({<<"children">>, Children}, Browser) ->
    Browser#browser{children = lists:map(fun jiffy_to_browser/1, Children)};
jiffy_to_browser({<<"version_regex">>, VersionRegex}, Browser) ->
    {ok, Compiled} = re:compile(VersionRegex, [unicode]),
    Browser#browser{version_regex = Compiled}.

-spec jiffy_to_os(jiffy_struct()) -> #os{}.
jiffy_to_os({OS}) ->
    lists:foldl(fun jiffy_to_os/2, #os{}, OS).

-spec jiffy_to_os({binary(), jiffy_term()}, #os{}) -> #os{}.
jiffy_to_os({<<"family">>, Family}, OS) ->
    OS#os{family = binary_to_atom(Family, utf8)};
jiffy_to_os({<<"manufacturer">>, Manufacturer}, OS) ->
    OS#os{manufacturer = binary_to_atom(Manufacturer, utf8)};
jiffy_to_os({<<"name">>, Name}, OS) ->
    OS#os{name = Name};
jiffy_to_os({<<"aliases">>, Aliases}, OS) ->
    OS#os{aliases = Aliases};
jiffy_to_os({<<"exclusions">>, Exclusions}, OS) ->
    OS#os{exclusions = Exclusions};
jiffy_to_os({<<"device_type">>, DeviceType}, OS) ->
    OS#os{device_type = binary_to_atom(DeviceType, utf8)};
jiffy_to_os({<<"children">>, Children}, OS) ->
    OS#os{children = lists:map(fun jiffy_to_os/1, Children)};
jiffy_to_os({<<"version_regex">>, VersionRegex}, OS) ->
    {ok, Compiled} = re:compile(VersionRegex, [unicode]),
    OS#os{version_regex = Compiled}.

-spec bin_to_num(Bin :: binary()) -> number().
bin_to_num(Binary) when is_binary(Binary) ->
    List = re:replace(Binary, ?COMPILE_ONCE("\\."), "", [{return, list}]),
    case string:to_float(List) of
        {error, no_float} -> list_to_integer(List);
        {F,_Rest} -> F
    end.

-spec load_operating_systems() -> [#os{}].
load_operating_systems() ->
    {ok, C} = read_priv_file("operating_systems.json"),
    lists:map(fun jiffy_to_os/1, jiffy:decode(C)).

-spec load_browsers() -> [#browser{}].
load_browsers() ->
    {ok, C} = read_priv_file("browsers.json"),
    lists:map(fun uaparser_utils:jiffy_to_browser/1, jiffy:decode(C)).

-spec read_priv_file(Filename :: string()) -> iolist().
read_priv_file(Filename) ->
    case code:priv_dir(uaparser) of
        {error, bad_name} ->
            Dir = "priv/";
        Dir ->
            ok
    end,
    Path = filename:join(Dir, Filename),
    file:read_file(Path).
