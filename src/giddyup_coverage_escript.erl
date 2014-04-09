-module(giddyup_coverage_escript).
-include_lib("kernel/include/file.hrl").

-export([main/1]).

cli_options() ->
%% Option Name, Short Code, Long Code, Argument Spec, Help Message
[
 {project,            $p, "project",     string,  "specifices which project"},
 {version,            $v, "version",     string,  "specifices which version of the project(s)"},
 {debug,              $v, "debug",         undefined,  "debug?"}
].


main(Args) ->
    {ok, {Parsed, _Other}} = getopt:parse(cli_options(), Args),
    lager:start(),
    case proplists:get_all_values(project, Parsed) of
        undefined ->
            lager:error("Must specify project!"),
            application:stop(lager),
            halt(1);
        Projects ->
            lager:info("Projects ~p~n", [Projects])
    end.

