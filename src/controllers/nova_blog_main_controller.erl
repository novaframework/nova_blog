-module(nova_blog_main_controller).
-export([
         index/1,
         not_found/1,
         internal_error/1
        ]).

index(#{method := <<"GET">>} = _Req) ->
    LastRelease =
        case nova_blog_db:get_latest_release() of
            {ok, [Entry|_]} -> Entry;
            _ ->
                #{version => "0.0.0",
                  description => "no description",
                  added => calendar:local_time()}
        end,
    {ok, LatestNews} = nova_blog_db:get_entries(4),
    {ok, [{last_release, LastRelease}, {latest_news, LatestNews}]}.


not_found(_Req) ->
    {ok, [{error, "We could not find the page"}]}.

internal_error(_Req) ->
    {ok, [{error, "Woops. Internal server error"}]}.
