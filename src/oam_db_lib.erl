%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(oam_db_lib).     
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
%-compile(export_all).

-export([
	 create_dir/2,
	 load_start_node_w_basic_appls/4,
	 create_cluster_node_info/4
	 ]).


-export([
	 install_oam_db/2,
	 install_first_node/2,
	 init/2,
	 init_tables/3
	 ]).


-define(StorageType,ram_copies).
-define(WAIT_FOR_TABLES,2*5000).

-define(BasicAppls,["common","sd","nodelog"]).

%% ====================================================================
%% External functions
%% ====================================================================
create_dir(HostName,Dir)->
    Ip=config:host_local_ip(HostName),
    SshPort=config:host_ssh_port(HostName),
    Uid=config:host_uid(HostName),
    Pwd=config:host_passwd(HostName),
    TimeOut=5000,
    my_ssh:ssh_send(Ip,SshPort,Uid,Pwd,"rm -rf "++Dir,TimeOut),
    my_ssh:ssh_send(Ip,SshPort,Uid,Pwd,"mkdir "++Dir,TimeOut),
    case ssh_vm:is_dir(Dir,{Ip,SshPort,Uid,Pwd,TimeOut}) of
	true->
	    ok;
	false ->
	    {error,["failed to create ",Dir]}
    end.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
load_start_node_w_basic_appls(HostName,NodeName,NodeDir,Cookie)->
    Node=list_to_atom(NodeName++"@"++HostName),
    rpc:call(Node,init,stop,[]),
    timer:sleep(2000),
    PaArgs=" ",
    EnvArgs=" ",
    Reply=case create_basic_appls(HostName,NodeName,NodeDir,Cookie,PaArgs,EnvArgs) of
	      {error,Reason}->
		  {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
	      {ok,Node}->
		  Ping=[{error,[not_connected,Node,AppId]}||AppId<-?BasicAppls,
							    pong/=rpc:call(Node,list_to_atom(AppId),ping,[])],
		  case Ping of
		      []->
			  {ok,Node,Cookie};
		      ErrorList->
			  {error,[ErrorList,?MODULE,?FUNCTION_NAME,?LINE]}
		  end
	  end,
 
    Reply.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
create_basic_appls(HostName,NodeName,NodeDir,Cookie,PaArgs,EnvArgs)->
    Ip=config:host_local_ip(HostName),
    Port=config:host_ssh_port(HostName),
    Uid=config:host_uid(HostName),
    Passwd=config:host_passwd(HostName),
    TimeOut=7000,
    Reply=case ssh_vm:create(HostName,NodeName,Cookie,PaArgs,EnvArgs,
				 {Ip,Port,Uid,Passwd,TimeOut}) of
	       {error,Reason}->
		   {error,Reason};
	       {ok,Node}->
		  GitLoadStart=[git_load_start(Node,Appl,NodeDir)||Appl<-?BasicAppls],
		   CheckGitLoadStart=[{error,Reason}||{error,Reason}<-GitLoadStart],
		   case CheckGitLoadStart of
		       []->
			   {ok,Node};
		       Reason ->
			   {error,Reason}
		   end
	  end,
    Reply.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
git_load_start(Node,Appl,NodeDir)->
    DirToClone=filename:join(NodeDir,Appl),
%    io:format("DirToClone ~p~n",[{DirToClone,?MODULE,?FUNCTION_NAME,?LINE}]),
    GitPath=config:application_gitpath(Appl++".spec"),
    App=list_to_atom(Appl),
    Paths=[filename:join([NodeDir,Appl,"ebin"])], 
    
    Reply=case create_dirs_git_clone(Node,DirToClone,GitPath) of
	      {error,Reason}->
		  {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
	      {badrpc,Reason}->
		  {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
	      ok->
		  case load_start_appl(Node,Appl,App,Paths) of
		      {error,Reason}->
			  {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		      {badrpc,Reason}->
			  {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		      {ok,Node,Appl}->
			  {ok,Node,Appl}
		  end
	  end,
    Reply.

create_dirs_git_clone(Node,DirToClone,GitPath)->
    Reply=case rpc:call(Node,os,cmd,["rm -rf "++DirToClone]) of
	      []->
		  case rpc:call(Node,file,make_dir,[DirToClone]) of
		      {error,Reason}->
			  {error,[Reason,DirToClone,?MODULE,?FUNCTION_NAME,?LINE]};
		      {badrpc,Reason}->
			  {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		      ok->
			  case rpc:call(Node,filelib,is_dir,[DirToClone]) of
			      {badrpc,Reason}->
				  {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
			      false->
				  {error,[DirToClone,eexists,"couldnt git clone application:",
					  " GitPath:",GitPath," DirToClone:",DirToClone," on Node:",Node,?MODULE,?LINE]};
			      true->
				  case appl:git_clone_to_dir(Node,GitPath,DirToClone) of
				      {error,Reason}->
					  {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
				      {ok,_}->
					  ok
				  end
			  end
		  end
	  end,
    Reply.

load_start_appl(Node,Appl,App,Paths)->
    Reply=case appl:load(Node,App,Paths) of
	      {error,Reason}->
		  {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
	      ok->
		  case appl:start(Node,App) of
		      {error,Reason}->
			  {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		      ok->
			  case rpc:call(Node,App,ping,[]) of
			      pong->
				  {ok,Node,Appl};
			      pang->
				  {error,[pang,"couldnt connect to Node",Node,?MODULE,?LINE]}
			  end
		  end
	  end,
    Reply.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
create_cluster_node_info(NumNodesPerHost,ClusterName,HostName,Cookie)->
    create_cluster_node_info(NumNodesPerHost,ClusterName,HostName,Cookie,[]).

create_cluster_node_info(0,_,_,_,HostNodeNameNodeDirCookieList)->
    HostNodeNameNodeDirCookieList;
create_cluster_node_info(N,ClusterName,HostName,Cookie,Acc)->
    NodeName=ClusterName++"_"++HostName++"_"++integer_to_list(N),
    NodeDir=filename:join(ClusterName++".dir",NodeName++".dir"),
    Node=list_to_atom(NodeName++"@"++HostName),
    NewAcc=[{Node,HostName,NodeName,NodeDir,Cookie}|Acc],
    create_cluster_node_info(N-1,ClusterName,HostName,Cookie,NewAcc). 

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
install_first_node(FirstNode,DbCallBacks)->
    Nodes=[],
    ok=rpc:call(FirstNode,application,load,[db]),
    ok=rpc:call(FirstNode,application,set_env,
		[[{db,[{db_callbacks,DbCallBacks},
				 {nodes,Nodes}]}]]),
    ok=rpc:call(FirstNode,application,start,[db]),

    DynamicDbInitTables=dynamic_db:init_tables(DbCallBacks,node(),FirstNode),  
    DynamicDbInitTables.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
install_oam_db(DbCallBacks,Nodes)->
    application:start(config),
%    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
%				 {"DBG, DbCallBacks ",DbCallBacks,node()}]), 
%    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
%				 {"DBG, Nodes ",Nodes,node()}]), 

   
    % Install local
    _DynamicDbInit=dynamic_db:init(DbCallBacks,[]),
   % rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
%				 {"DBG, DynamicDbInit ",DynamicDbInit,node()}]),
    _DynamicDbInitTables=dynamic_db:init_tables(DbCallBacks,node(),node()),
 %   rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
%				 {"DBG, DynamicDbInitTables ",DynamicDbInitTables,node()}]),   
    
    [FirstNode|NodesToAdd]=Nodes,
    {ok,FirstNode,NodesToAdd}.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
init_tables(DbCallbacks,Source,Dest)->
    InitTables=[{CallBack,CallBack:init_table(Source,Dest)}||CallBack<-DbCallbacks],
    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
				 {"DBG: CreateTables   ",?MODULE,node()}]),  
    InitTables.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
init(DbCallbacks,Nodes)->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    start(DbCallbacks,Nodes).

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start(DbCallbacks,[])->
    % Add unique code to create the specific tables
 %% Create tables on TestNode
    CreateTables=[{CallBack,CallBack:create_table()}||CallBack<-DbCallbacks],
    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
				 {"DBG: CreateTables   ",?MODULE,node()}]), 
    CreateTables;

start(DbCallbacks,Nodes) ->
    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
				 {"DBG: Nodes   ",Nodes,node()}]), 
    add_extra_nodes(DbCallbacks,Nodes).


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
add_extra_nodes(DbCallbacks,[Node|T])->
    case mnesia:change_config(extra_db_nodes,[Node]) of
	{ok,[Node]}->
	    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
					 {"DBG: add_extra_nodes Node at node()  ",Node,?MODULE,node()}]), 
 
	    AddSchema=mnesia:add_table_copy(schema,node(),?StorageType),
	    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
					 {"DBG: add_extra_nodes AddSchema  ",AddSchema,?MODULE,node()}]), 
	    TablesFromNode=rpc:call(Node,mnesia,system_info,[tables]),
	    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
					 {"DBG: TablesFromNode  ",TablesFromNode}]), 
	    AddTableCopies=[{Table,mnesia:add_table_copy(Table,node(),?StorageType)}||Table<-TablesFromNode,
										      Table/=schema],
	    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
				 {"DBG: AddTableCopies  ",AddTableCopies,?MODULE,node()}]), 
	    Tables=mnesia:system_info(tables),
	    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
					 {"DBG: add_extra_nodes Tables  ",Tables,?MODULE,node()}]),
	    WaitForTables=mnesia:wait_for_tables(Tables,?WAIT_FOR_TABLES),
	    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
					 {"DBG: add_extra_nodes WaitForTables  ",WaitForTables,?MODULE,node()}]);
	Reason->
	    rpc:cast(node(),nodelog,log,[notice,?MODULE_STRING,?LINE,
					 {"DBG: Didnt connect to Node Reason  ",Reason,?MODULE,node()}]),
	    add_extra_nodes(DbCallbacks,T)
    end.


