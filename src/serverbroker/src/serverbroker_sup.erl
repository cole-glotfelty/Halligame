%%%-------------------------------------------------------------------
%% @doc serverbroker top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(serverbroker_sup).

-behaviour(supervisor).

-export([start_link/0, stop/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [#{
        id => main,
        start => {serverbroker, start_link, []}
    }],
    {ok, {SupFlags, ChildSpecs}}.

stop() ->
    supervisor:terminate_child(?MODULE, main).
%% internal functions
