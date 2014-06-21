-ifndef(UAPARSERL_HRL).
-define(UAPARSERL_HRL, 1).

-define(COMPILE_ONCE(RX), ct_expand:term((fun() -> {ok, C} = re:compile(RX, [unicode]), C end)())).

-record(browser, {
        family          = undefined             :: undefined | atom(),
        manufacturer    = undefined             :: undefined | atom(),
        name            = undefined             :: undefined | binary(),
        aliases         = []                    :: [binary()] | [],
        exclusions      = []                    :: [binary()] | [],
        browser_type    = undefined             :: undefined | atom(),
        renderer        = undefined             :: undefined | atom(),
        children        = []                    :: [#browser{}] | [],
        version_regex   = undefined             :: undefined | binary() | re:mp()
    }).

-record(os, {
        family          = undefined             :: undefined | atom(),
        manufacturer    = undefined             :: undefined | atom(),
        name            = undefined             :: undefined | binary(),
        aliases         = []                    :: [binary()] | [],
        exclusions      = []                    :: [binary()] | [],
        device_type     = undefined             :: undefined | atom(),
        children        = []                    :: [#os{}] | [],
        version_regex   = undefined             :: undefined | binary() | re:mp()
    }).

-type version_details() :: [{atom(),integer()}].
-type ua_value()        :: atom()|binary()|version_details().

-endif.
