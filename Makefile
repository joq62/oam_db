all:
	rm -rf  *~ */*~ src/*.beam test/*.beam erl_cra*;
	rm -rf  ~/*_specs ~/deployments;
	rm -rf  logs *.pod_dir rebar.lock;
	rm -rf config sd;
	rm -rf deployments *_info_specs;
	rm -rf _build test_ebin ebin *_info_specs;
	rm -rf Mnesia.*;
	mkdir ebin;		
	rebar3 compile;
	cp /home/joq62/erlang/infra_2/db/ebin/* ebin;
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build test_ebin logs log;
	git add -f *;
	git commit -m $(m);
	git push;
	echo Ok there you go!
start:
	erl -pa ebin -pa test_ebin\
	    -pa /home/joq62/erlang/infra_2/common/ebin\
	    -pa /home/joq62/erlang/infra_2/sd/ebin\
	    -pa /home/joq62/erlang/infra_2/nodelog/ebin\
	    -pa /home/joq62/erlang/infra_2/config/ebin\
	    -pa /home/joq62/erlang/infra_2/etcd/ebin\
	    -pa /home/joq62/erlang/infra_2/test_lib/ebin\
	   -sname db_oam -setcookie cookie_test -hidden
eunit:
	rm -rf  *~ */*~ src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build logs log;
	rm -rf deployments *_info_specs;
	rm -rf  ~/*_specs ~/deployments;
	rm -rf config sd;
	rm -rf rebar.lock;
	rm -rf Mnesia.*;
	rm -rf ebin;
	mkdir  application_info_specs;
	cp ../../specifications/application_info_specs/*.spec application_info_specs;
	mkdir ~/application_info_specs;
	cp ../../specifications/application_info_specs/*.spec ~/application_info_specs;
	mkdir  host_info_specs;
	cp ../../specifications/host_info_specs/*.host host_info_specs;
	mkdir ~/host_info_specs;
	cp ../../specifications/host_info_specs/*.host ~/host_info_specs;
	mkdir deployment_info_specs;
	cp ../../specifications/deployment_info_specs/*.depl deployment_info_specs;
	mkdir ~/deployment_info_specs;
	cp ../../specifications/deployment_info_specs/*.depl ~/deployment_info_specs;
	mkdir deployments;
	cp ../../specifications/deployments/*.depl_spec deployments;
	mkdir ~/deployments;
	cp ../../specifications/deployments/*.depl_spec ~/deployments;
	mkdir test_ebin;
	mkdir ebin;
	rebar3 compile;
	cp _build/default/lib/*/ebin/* ebin;
	erlc -I include -o test_ebin test/*.erl;
	erl -pa ebin -pa test_ebin\
	    -pa /home/joq62/erlang/infra_2/common/ebin\
	    -pa /home/joq62/erlang/infra_2/sd/ebin\
	    -pa /home/joq62/erlang/infra_2/nodelog/ebin\
	    -pa /home/joq62/erlang/infra_2/config/ebin\
	    -sname db_oam_test -run $(m) start -setcookie cookie_test\
	    -hidden
