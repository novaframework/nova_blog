-module(nova_blog_main_controller).
-export([
         index/1
        ]).

index(#{method := <<"GET">>} = _Req) ->
    Result = nova_blog_db:get_entries(),
    {ok, [{entries, Result}]}.
