%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(oam_config).     
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
%-compile(export_all).

-export([
	 create_config_node/1
	 ]).

-define(ConfigDir,"config").
%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
create_config_node(HostName)->
    %Assume that configs files are loaded/stored at root dir!
    % Start Node
    
    NodeName=common:config_nodename(),
    Cookie=common:config_cookie(),
    Node=list_to_atom(NodeName++"@"++HostName),
    erlang:set_cookie(Node,list_to_atom(Cookie)),
    rpc:call(Node,application,stop,[config]),
    rpc:call(Node,application,unload,[config]),
    ssh_vm:delete_dir(HostName,?ConfigDir),
    Result=case ssh_vm:create_dir(HostName,?ConfigDir) of
	       {error,Reason}->
		   {error,Reason};
	       {ok,?ConfigDir}->
		   PaArgs=" -pa ~/* ",
		 %  PaArgs=" ",
		   EnvArgs=" -hidden ",
		   case ssh_vm:create(HostName,NodeName,Cookie,PaArgs,EnvArgs) of
		       {error,Reason}->
			   {error,Reason};
		       {ok,Node}->
			   erlang:set_cookie(Node,list_to_atom(Cookie)),
			   case oam_db_lib:git_load_start(Node,"config",?ConfigDir) of
			       {error,Reason}->
				   {error,Reason};
			       {ok,Node,"config"}->			 
				   {ok,Node}
			   end
		   end
	   end,
    Result.

			 

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
