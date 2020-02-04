-module(nova_blog_documentation_controller).
-export([
         index/1
        ]).

index(#{method := <<"GET">>} = Req) ->
    Page = cowboy_req:binding(page, Req, undefined),
    {ok, [{page1, Page}]}.
