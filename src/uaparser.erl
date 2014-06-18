-module(uaparser).
-export([parse/1]).

parse(UserAgent) when is_list(UserAgent) ->
    parse(iolist_to_binary(UserAgent));
parse(UserAgent) when is_binary(UserAgent) ->
    Lower = list_to_binary(string:to_lower(binary_to_list(UserAgent))),
    [
        {browser, parse_browser(Lower)},
        {os, parse_os(Lower)}
    ].

parse_browser(UserAgent) ->
    uaparser_browser:parse(UserAgent).

parse_os(UserAgent) ->
    uaparser_os:parse(UserAgent).
