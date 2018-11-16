-module(mysql_pool).

-export([
         start/0,
         init/4,
         query/2
        ]).

%% TODO: del mysql_pool app
start() ->
    application:start(mysql_pool).

init(PoolId, PoolSize, MaxOverFlow, WorkerArgs) ->
    PoolArgs = [
                {strategy, fifo},
                {name, {local, PoolId}},
                {worker_module, mysql_pool_proxy},
                {size, PoolSize},
                {max_overflow, MaxOverFlow},
                {overflow_ttl, 0},
                {overflow_check_period, 0}
               ],
    ChildSpec = poolboy:child_spec(PoolId, PoolArgs, [mysql, WorkerArgs]),
    case catch mysql_pool_sup:start_child(ChildSpec) of
        {ok, _} ->
            ok;
        {error, {already_started, _Pid}} ->
            ok;
        Other ->
            exit({mysql_pool_init, Other})
    end.

query(PoolId, SQL) ->
    Fun = fun(Pid) ->
                  case mysql_pool_proxy:connect(Pid) of
                      {ok, DbPid} ->
                          mysql:query(DbPid, SQL);
                      Other ->
                          {db_error, Other}
                  end
          end,
    catch poolboy:transaction(PoolId, Fun).

