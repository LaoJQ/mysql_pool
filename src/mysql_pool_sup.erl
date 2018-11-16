%%%-------------------------------------------------------------------
%% @doc mysql_pool top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mysql_pool_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         start_child/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(ChildSpec) ->
    supervisor:start_child(?MODULE, ChildSpec).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
