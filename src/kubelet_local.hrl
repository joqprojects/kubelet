%% service_info
%% Information of the servie
-record(state, {git_url,kubelet_info,lSock,max_workers,active_workers,workers,dns_list,dns_addr}).

-define(SERVICE_EBIN,"service_ebin").
-define(KUBELET_EBIN,"kubelet_ebin").
