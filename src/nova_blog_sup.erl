%%%-------------------------------------------------------------------
%% @doc nova_blog top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(nova_blog_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ChildSpecs = [
                  #{
                    id => X,
                    start => {X, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [X]} || X <- []
                 ],
    {ok, { {one_for_all, 0, 1}, ChildSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================
