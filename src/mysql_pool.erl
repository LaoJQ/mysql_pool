-module(mysql_pool).

-export([
         init/5,
         query/2
        ]).

init(ParentSup, PoolId, PoolSize, MaxOverFlow, WorkerArgs) ->
    PoolArgs = [
                {strategy, fifo},
                {name, {local, PoolId}},
                {worker_module, mysql_pool_proxy},
                {size, PoolSize},
                {max_overflow, MaxOverFlow}
               ],
    ChildSpec = poolboy:child_spec(PoolId, PoolArgs, [mysql, WorkerArgs]),
    case catch supervisor:start_child(ParentSup, ChildSpec) of
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

