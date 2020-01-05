-module(nova_blog_release_controller).
-export([index/1]).

index(Req) ->
    {ok, [LatestRelease]} = nova_blog_db:get_latest_release(),
    Version = cowboy_req:binding(version, Req, maps:get(<<"version">>, LatestRelease)),
    {ok, Releases} = nova_blog_db:get_releases(),
    {ok, [Release]} = nova_blog_db:get_release(Version),
    {ok, [{releases, Releases}, {current_release, Release}]}.
