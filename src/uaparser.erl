-module(uaparser).
-export([parse/1]).
-include("uaparser.hrl").

-spec parse(binary()) -> [{os|browser, [{atom(), ua_value()}]}].
parse(UserAgent) when is_list(UserAgent) ->
    parse(iolist_to_binary(UserAgent));
parse(UserAgent) when is_binary(UserAgent) ->
    Lower = list_to_binary(string:to_lower(binary_to_list(UserAgent))),
    [
        {browser, uaparser_parser:parse(Lower, browser)},
         {os, uaparser_parser:parse(Lower, os)}
    ].
%%     [
%%         {browser, uaparser_browser:parse(Lower)}
%% %%         {os, uaparser_os:parse(Lower)}
%%     ].
