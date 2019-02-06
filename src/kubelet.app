{application, kubelet,
 [{description, " kubelet"},
  {vsn, "1.0.0"},
  {modules, [kubelet_app,
             kubelet_sup,
	     kubelet_lib,
	     kubelet]},
  {registered, [kubelet]},
  {applications, [kernel, stdlib]},
  {mod, {kubelet_app, []}}
 ]}.
