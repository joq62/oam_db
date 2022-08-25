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

-define(AllHostNames,["c100","c200"]).
-define(ApplInfoSpecs,{"application_info_specs",".spec"}).
-define(HostInfoSpecs,{"host_info_specs",".host"}).
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    ok=setup(),
     
    {ok,AllConfigNodes}=create_config_node_test(),
    io:format("AllConfigNodes ~p~n",[{AllConfigNodes,?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok=copy_dir_test(?ApplInfoSpecs,AllConfigNodes),
    
 
    
 %   ok=cleanup(),
    io:format("TEST Ok, there you go! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
copy_dir_test({SourceDir,Ext},AllConfigNodes)->
    DeleteDir1=[rm:r(N,SourceDir)||N<-AllConfigNodes],
    io:format("DeleteDir1 ~p~n",[{DeleteDir1,?MODULE,?FUNCTION_NAME,?LINE}]),
    
    MkDir=[{N,rpc:call(N,file,make_dir,[SourceDir],5000)}||N<-AllConfigNodes],
    io:format("MkDir ~p~n",[{MkDir,?MODULE,?FUNCTION_NAME,?LINE}]),

    SourceNode=node(),
    DestDir=SourceDir,
    CopyResult=[copy:copy_dir_ext(SourceNode,SourceDir,DestNode,DestDir,Ext)||DestNode<-AllConfigNodes],
    io:format("CopyResult ~p~n",[{CopyResult,?MODULE,?FUNCTION_NAME,?LINE}]),

    DeleteDir2=[rm:r(N,SourceDir)||N<-AllConfigNodes],
    io:format("DeleteDir2 ~p~n",[{DeleteDir2,?MODULE,?FUNCTION_NAME,?LINE}]),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
create_config_node_test()->
    R1=[{HostName,oam_db:create_config_node(HostName)}||HostName<-?AllHostNames],
    AllConfigNodes=[Node||{_HostName,{ok,Node}}<-R1],
    {ok,AllConfigNodes}.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
cleanup()->
    OamTestLogsDir="test_logs",
    []=os:cmd("rm -rf "++OamTestLogsDir),
    Nodes=[list_to_atom("config@"++HostName)||HostName<-?AllHostNames],
    [rpc:call(Node,init,stop,[])||Node<-Nodes],
    [ssh_vm:delete(HostName,"config")||HostName<-?AllHostNames],
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

    Nodes=[list_to_atom("config@"++HostName1)||HostName1<-?AllHostNames],
    [erlang:set_cookie(N,config)||N<-Nodes],
    [rpc:call(N,init,stop,[])||N<-Nodes],
    io:format("SUB-TEST OK  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]), 
    ok.
