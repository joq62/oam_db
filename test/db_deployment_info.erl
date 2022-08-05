-module(db_deployment_info).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_deployment_info.hrl").

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}
				]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList,StorageType)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {StorageType,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

add_node(Node,StorageType)->
    Result=case mnesia:change_config(extra_db_nodes, [Node]) of
	       {ok,[Node]}->
		   mnesia:add_table_copy(schema, node(),StorageType),
		   mnesia:add_table_copy(?TABLE, node(), StorageType),
		   Tables=mnesia:system_info(tables),
		   mnesia:wait_for_tables(Tables,20*1000);
	       Reason ->
		   Reason
	   end,
    Result.

create(Name,Vsn,ApplSpecs,NumInstaces,Directive)->
    Record=#?RECORD{
		    name=Name,
		    vsn=Vsn,
		    appl_specs=ApplSpecs,
		    num_instances=NumInstaces,
		    directive=Directive
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

member(Name)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.name==Name])),
    Member=case Z of
	       []->
		   false;
	       _->
		   true
	   end,
    Member.

read(Key,Name)->
    Return=case read(Name) of
	       []->
		   {error,[eexist,Name,?FUNCTION_NAME,?MODULE,?LINE]};
	       {Name,Vsn,ApplSpecs,NumInstaces,Directive} ->
		   case  Key of
		       name->
			   {ok,Name};
		       vsn->
			   {ok,Vsn};
		       appl_specs->
			   {ok,ApplSpecs};
		       num_instances->
			   {ok,NumInstaces};
		       directive->
			   {ok,Directive};
		       Err ->
			   {error,['Key eexists',Err,?FUNCTION_NAME,?MODULE,?LINE]}
		   end
	   end,
    Return.

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{Name,Vsn,ApplSpecs,NumInstaces,Directive}||{?RECORD,Name,Vsn,ApplSpecs,NumInstaces,Directive}<-Z].

read(Object)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.name==Object])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Info]=[{Name,Vsn,ApplSpecs,NumInstaces,Directive}||{?RECORD,Name,Vsn,ApplSpecs,NumInstaces,Directive}<-Z],
		   Info
	   end,
    Result.

delete(Object) ->
    F = fun() -> 
		mnesia:delete({?TABLE,Object})
		    
	end,
    mnesia:transaction(F).


do(Q) ->
    F = fun() -> qlc:e(Q) end,
    Result=case mnesia:transaction(F) of
	       {atomic, Val} ->
		   Val;
	       {error,Reason}->
		   {error,Reason}
	   end,
    Result.

%%-------------------------------------------------------------------------
init_table(SourceNode,DestNode)->
%    ok=rpc:call(DestNode,?MODULE,create_table,[[DestNode]]),
    AllFileNames=rpc:call(SourceNode,config,deployment_all_filenames,[]),
    init_table(AllFileNames,SourceNode,DestNode).
    
init_table([],_,_)->
    ok;
init_table([FileName|T],SourceNode,DestNode)->
    {atomic,ok}=rpc:call(DestNode,?MODULE,create,
			 [FileName,
			  rpc:call(SourceNode,config,deployment_vsn,[FileName]),
			  rpc:call(SourceNode,config,deployment_appl_specs,[FileName]),
			  rpc:call(SourceNode,config,deployment_num_instances,[FileName]),
			  rpc:call(SourceNode,config,deployment_directive,[FileName])
			 ]),
    
    init_table(T,SourceNode,DestNode).
