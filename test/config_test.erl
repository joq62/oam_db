%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(config_test).   
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-define(HostNames,["c200","c201"]).
%-define(HostNames,["c100"]).

-define(HostNames,["c100","c200","c201"]).

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    ok=setup(),
     
    R1=[{HostName,oam_db:create_config_node(HostName)}||HostName<-?HostNames],
    R11=[{HostName,Node}||{HostName,{ok,Node}}<-R1],
  %  {ok,'config@c201'}=oam_db:create_config_node(H1),
 
    R2=[rpc:call(Node,config,host_local_ip,[HostName])||{HostName,Node}<-R11],
    io:format("~p~n",[{lists:sort(R2),?MODULE,?FUNCTION_NAME,?LINE}]),
 
    
    ok=cleanup(),
    io:format("TEST Ok, there you go! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
cleanup()->
    OamTestLogsDir="test_logs",
    []=os:cmd("rm -rf "++OamTestLogsDir),
    Nodes=[list_to_atom("config@"++HostName)||HostName<-?HostNames],
    [rpc:call(Node,init,stop,[])||Node<-Nodes],
    [ssh_vm:delete(HostName,"config")||HostName<-?HostNames],
    ok.
    
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    ok=application:start(common),
    "config"=common:config_cookie(),
    ok=application:start(sd),
    ok=application:start(nodelog),
    OamTestLogsDir="test_logs",
    []=os:cmd("rm -rf "++OamTestLogsDir),
    ok=file:make_dir(OamTestLogsDir),
    LogFile=filename:join(OamTestLogsDir,"log"),
    nodelog:create(LogFile),
    
    ok=application:start(config),
    HostName="c202",
    "192.168.1.202"=config:host_local_ip(HostName),
    
   % ok=application:start(test_lib),
   
    ok=application:start(oam_db),
%    appl:stop('config@c100',config),
 %   appl:unload('config@c100',config,"config"),

    [rpc:call(N,init,stop,[])||N<-['config@c100','config@c201']],
    io:format("SUB-TEST OK  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]), 
    ok.
