%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(oam_db).
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports


-export([
	 create_cluster/4,
	 create_dir/2,
	 create_cluster_node_info/4,
	 load_start_node_w_basic_appls/4

	]).


-export([
	 install_oam_db/2,
	 first_node/0,
%	 add_nodes/0,
	 install_first_node/0
	]).

-export([
	 start/0,
	 stop/0,
	 appl_start/1,
	 ping/0
	]).


%% gen_server callbacks



-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-------------------------------------------------------------------
-record(state, {db_callbacks,
		nodes,
		first_node,
		nodes_to_add
	       }).
%-define(DbCallbacks,[application_spec,deployment_info,deployments,host_spec]).

%% ====================================================================
%% External functions
%% ====================================================================
appl_start([])->
    application:start(?MODULE).



create_cluster(ClusterName,NumNodesPerHost,Hosts,Cookie)->
    gen_server:call(?MODULE,{create_cluster,ClusterName,NumNodesPerHost,Hosts,Cookie},infinity).
    
create_dir(HostName,Dir)->
    gen_server:call(?MODULE,{create_dir,HostName,Dir},infinity).
    
create_cluster_node_info(NumNodesPerHost,ClusterName,Host,Cookie)->
    gen_server:call(?MODULE,{create_cluster_node_info,
			     NumNodesPerHost,ClusterName,Host,Cookie},infinity).
    
load_start_node_w_basic_appls(HostName,NodeName,NodeDir,Cookie)->
    gen_server:call(?MODULE, {load_start_node_w_basic_appls,
			      HostName,NodeName,NodeDir,Cookie},infinity).

install_oam_db(DbCallBacks,Nodes)->
     gen_server:call(?MODULE, {install_oam_db,DbCallBacks,Nodes},infinity).

first_node()->
    gen_server:call(?MODULE, {first_node},infinity).
install_first_node()->
    gen_server:call(?MODULE, {install_first_node},infinity).
%% call
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).

ping()->
    gen_server:call(?MODULE,{ping},infinity).

%% cast

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->

    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
				 {"OK, started server  ",?MODULE,node()}]), 
    {ok, #state{}}.   
  % {ok, #state{},0}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({create_cluster,ClusterName,NumNodesPerHost,Hosts,Cookie},_From, State) ->
    io:format("DBG:  ~p~n",[{ClusterName,NumNodesPerHost,Hosts,Cookie,?MODULE,?FUNCTION_NAME,?LINE}]),
    Reply=oam_db_lib:create_cluster(ClusterName,NumNodesPerHost,Hosts,Cookie),
    io:format("DBG: Reply ~p~n",[{ Reply,?MODULE,?FUNCTION_NAME,?LINE}]),
    {reply, Reply, State};

handle_call({create_dir,HostName,Dir},_From, State) ->
    io:format("DBG: HostName,Dir ~p~n",[{HostName,Dir,?MODULE,?FUNCTION_NAME,?LINE}]),
    Reply=oam_db_lib:create_dir(HostName,Dir),
    io:format("DBG: Reply ~p~n",[{ Reply,?MODULE,?FUNCTION_NAME,?LINE}]),
    {reply, Reply, State};

handle_call({create_cluster_node_info,
	     NumNodesPerHost,ClusterName,Host,Cookie},_From, State) ->
    io:format("DBG: NumNodesPerHost,ClusterName,Host,Cookie ~p~n",[{NumNodesPerHost,ClusterName,Host,Cookie,?MODULE,?FUNCTION_NAME,?LINE}]),
    Reply=oam_db_lib:create_cluster_node_info(
	    NumNodesPerHost,ClusterName,Host,Cookie),
    io:format("DBG: Reply ~p~n",[{ Reply,?MODULE,?FUNCTION_NAME,?LINE}]),
    {reply, Reply, State};

handle_call({load_start_node_w_basic_appls,
	     HostName,NodeName,NodeDir,Cookie},_From, State) ->
    io:format("DBG: HostName,NodeName,NodeDir,Cookie ~p~n",[{ HostName,NodeName,NodeDir,Cookie,?MODULE,?FUNCTION_NAME,?LINE}]),
    Reply=oam_db_lib:load_start_node_w_basic_appls(
	    HostName,NodeName,NodeDir,Cookie),
    io:format("DBG: Reply ~p~n",[{ Reply,?MODULE,?FUNCTION_NAME,?LINE}]),
    {reply, Reply, State};



handle_call({install_oam_db,DbCallBacks,Nodes},_From, State) ->
    {ok,FirstNode,NodesToAdd}=oam_db_lib:install_oam_db(DbCallBacks,Nodes),
    Reply={ok,DbCallBacks,Nodes,FirstNode,NodesToAdd},
    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
				 {"OK,  ",DbCallBacks,Nodes,node()}]), 
    {reply, Reply, State#state{db_callbacks=DbCallBacks,
			       nodes=Nodes,
			       first_node=FirstNode,
			       nodes_to_add=NodesToAdd}};


handle_call({install_first_node},_From, State) ->
    FirstNode=State#state.first_node,
    DbCallBacks=State#state.db_callbacks,
    Reply=oam_db_lib:install_first_node(FirstNode,DbCallBacks),
    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
				 {"OK,  ",Reply,node()}]), 
    {reply, Reply, State};


handle_call({first_node},_From, State) ->
    Reply=State#state.first_node,
    {reply, Reply, State};

handle_call({add_nodes},_From, State) ->
    NodesToAdd=lists:delete(node(),State#state.nodes),
    
    Reply=NodesToAdd,
    {reply, Reply, State};

handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{Msg,?MODULE,?LINE}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info({ssh_cm,_,_}, State) ->
   
    {noreply, State};

handle_info(Info, State) ->
    io:format("unmatched match~p~n",[{Info,?MODULE,?LINE}]), 
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
