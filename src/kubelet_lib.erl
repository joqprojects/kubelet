%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(kubelet_lib).
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kubelet/src/kubelet_local.hrl").

-include("include/trace_debug.hrl").
-include("include/kubelet_data.hrl").
-include("include/dns_data.hrl").
-include("include/repository_data.hrl").
-include("include/loader.hrl").
%% --------------------------------------------------------------------

%% External exports
-compile(export_all).

%-export([load_start_node/3,stop_unload_node/3
%	]).


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
dns_register(DnsInfo, DnsList) ->
    TimeStamp=erlang:now(),
    NewDnsInfo=DnsInfo#dns_info{time_stamp=TimeStamp},
    #dns_info{time_stamp=_,ip_addr=IpAddr,port=Port,service_id=ServiceId}=DnsInfo,
    
    X1=[X||X<-DnsList,false==({IpAddr,Port,ServiceId}==
				  {X#dns_info.ip_addr,X#dns_info.port,X#dns_info.service_id})],
    NewDnsList=[NewDnsInfo|X1],
    NewDnsList.

de_dns_register(DnsInfo,DnsList)->
    #dns_info{time_stamp=_,ip_addr=IpAddr,port=Port,service_id=ServiceId}=DnsInfo,
    NewDnsList=[X||X<-DnsList,false==({IpAddr,Port,ServiceId}==
				  {X#dns_info.ip_addr,X#dns_info.port,X#dns_info.service_id})],
    NewDnsList.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
load_start_service(ServiceId,GitUrl,{ServiceIp,ServicePort},{DnsIp,DnsPort})->
    os:cmd("rm -r "++?LOADPACKAGE++ServiceId),
    GitService=GitUrl++?LOADPACKAGE++ServiceId++".git",
    os:cmd("git clone "++GitService),

    Service=list_to_atom(ServiceId),
    ok=application:set_env(Service,ip_addr,ServiceIp),
    ok=application:set_env(Service,port,ServicePort),
    ok=application:set_env(Service,service_id,ServiceId),
    ok=application:set_env(Service,dns_ip_addr,DnsIp),
    ok=application:set_env(Service,dns_port,DnsPort),
    
    code:add_path(?LOADPACKAGE++ServiceId),
    R=application:start(Service),
    io:format("~p~n",[{?MODULE,?LINE,ServiceId,R}]),
    R.    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
stop_unload_app(ServiceId)->
    Service=list_to_atom(ServiceId),
    R1=application:stop(Service),
    R2=application:unload(Service),    
    os:cmd("rm -rf "++?LOADPACKAGE++ServiceId),
    code:del_path(?LOADPACKAGE++ServiceId),
    {R1,R2}.
    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
zone()->
    {ok,I}=file:consult("kubelet.config"),
    R=case lists:keyfind(zone,1,I) of
	  {zone,Z}->
	      Z;
	  false ->
	      []
      end,
    R.

capabilities()->
    {ok,I}=file:consult("kubelet.config"),
    R=case lists:keyfind(capabilities,1,I) of
	  {capabilities,C}->
	      C;
	  false ->
	      []
      end,
    R.


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

    
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
