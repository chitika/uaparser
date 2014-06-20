UAParser
========

A user-agent parser for Erlang, based on the process used by [ferd's useragent](https://github.com/ferd/useragent). Currently, this library supports:

* Browser Identification
   * MSIE, Firefox, Chrome, Opera, and many others
   * Version identification for a majority of support browsers
* Operating System Identification
   * Windows, Mac OS X, Android, IOS, "Linux", WebOS, and others.
   * Version Identification for the following:
      * Windows
      * Android
      * WebOS
      * iOS
      * Mac OS X

The process used has proven to be significantly more efficient than libraries which rely upon long lists of regular expressions, all the while losing little with respect to accuracy. 

Usage
========
Simply call the function `uaparser:parse/1` with the user-agent as its argument, as follows:

```erlang
Erlang R16B03 (erts-5.10.4) [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V5.10.4  (abort with ^G)
1> uaparser:parse("Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0; Trident/5.0; SLCC1; .NET CLR 2.0.50727; InfoPath.1; .NET CLR 3.5.30729; WinTSI 08.02.2010; .NET CLR 3.0.30729; .NET4.0C; BRI/1)").
[{browser,[{name,<<"internet explorer 7">>},
           {family,ie},
           {manufacturer,microsoft},
           {type,web_browser},
           {renderer,trident},
           {version,<<"7.0">>},
           {version_details,[{major,7},{minor,0}]}]},
 {os,[{name,<<"windows vista">>},
      {family,windows},
      {manufacturer,microsoft},
      {type,computer},
      {version,<<"6.0">>},
      {version_details,[{major,6},{minor,0}]}]}]
```

You will receive nested proplists back containing extracted information on both the browser and the operating system.
