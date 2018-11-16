-module(mysql_pool_proxy).

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([connect/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          worker_module,
          worker_args,
          worker_pid
         }).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

connect(Pid) ->
    gen_server:call(Pid, connect).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([WorkerModule, WorkerArgs]) ->
    process_flag(trap_exit, true),
    {ok, #state{
            worker_module = WorkerModule,
            worker_args = WorkerArgs
           }}.

handle_call(connect, _From, #state{
                               worker_module = Mod,
                               worker_args = Args,
                               worker_pid = undefined
                              } = State) ->
    case Mod:start_link(Args) of
        {ok, Pid} ->
            {reply, {ok, Pid}, State#state{
                                 worker_pid = Pid
                                }};
        {error, Error} ->
            io:format("[mysql_pool_proxy-~p] start worker failed, reason: ~p~n", [?LINE, Error]),
            {reply, {error, Error}, State}
    end;
handle_call(connect, _From, #state{
                               worker_pid = Pid
                              } = State) ->
    {reply, {ok, Pid}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _}, #state{
                                 worker_pid = Pid
                                } = State) ->
    io:format("[mysql_pool_proxy-~p] worker exit~n", [?LINE]),
    {noreply, reset_worker(State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
reset_worker(#state{
                worker_pid = undefined
               } = State) ->
    State;
reset_worker(State) ->
    State#state{
      worker_pid = undefined
     }.

