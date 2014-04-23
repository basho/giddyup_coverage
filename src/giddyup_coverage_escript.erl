-module(giddyup_coverage_escript).

-export([main/1]).

cli_options() ->
    %% Option Name, Short Code, Long Code, Argument Spec, Help Message
    [
        {project,            $p, "project",     string,  "specifices which project"},
        {version,            $v, "version",     string,  "specifices which version of the project(s)"},
        {platform,            $P, "platform",     string,  "specifices which platform"},
        {debug,              $v, "debug",         undefined,  "debug?"}
    ].


main(Args) ->
    {ok, {Parsed, CodePaths}} = getopt:parse(cli_options(), Args),

    [code:add_pathz(D) || D <- CodePaths],
    ok = lager:start(),
    ok = application:start(pgsql),
    ok = application:start(asn1),
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(lhttpc),
    Projects  = case proplists:get_all_values(project, Parsed) of
        [] ->
            lager:error("Must specify project!"),
            application:stop(lager),
            halt(1);
        P ->
            lager:info("Projects ~p", [P]),
            P
    end,

    Version = case proplists:get_value(version, Parsed) of
        undefined ->
            lager:error("Must specify version!"),
            application:stop(lager),
            halt(1);
        V ->
            lager:info("Version is ~p", [V]),
            V
    end,

    Platform= case proplists:get_value(platform, Parsed) of
        undefined ->
            lager:error("Must specify platform!"),
            application:stop(lager),
            halt(1);
        Pl ->
            lager:info("Platform is ~p", [Pl]),
            Pl
    end,


    Conn = pgsql_connection:open("host", "db", "user", "password"),

    ProjectIDs = [begin
                case pgsql_connection:extended_query("SELECT id FROM projects WHERE name=$1", [P0], Conn) of
                    {{select, 1}, [{ID}]} ->
                        {P0, ID};
                    {{select, 0}, []} ->
                        lager:error("could not find project ~p", [P0]),
                        application:stop(lager),
                        halt(1)
                end
        end || P0 <- Projects],

    ScorecardIDs = [begin
                case pgsql_connection:extended_query("SELECT id FROM scorecards WHERE name=$1 AND project_id=$2", [Version, PID], Conn) of
                    {{select, 1}, [{SID}]} ->
                        {PName, SID};
                    {{select, 0}, []} ->
                        lager:error("could not find scorecard for project ~p and version ~p", [PName, Version]),
                        application:stop(lager),
                        halt(1)
                end
        end || {PName, PID} <- ProjectIDs],

    lager:info("Res ~p", [ProjectIDs]),
    lager:info("Res ~p", [ScorecardIDs]),
    cover:start(),

    prepare(ScorecardIDs, Conn, Platform, Version),

    {ok, ProjectDirs} = file:list_dir(filename:join("scratch", Version)),
    Workers = lists:map(fun(Project) ->
                case filelib:is_dir(filename:join(["scratch", Version, Project])) of
                    true ->
                        {ok, Tests} = file:list_dir(filename:join(["scratch", Version, Project])),
                        W0 = lists:map(fun(Test) ->
                                    case filelib:is_dir(filename:join(["scratch", Version, Project, Test])) of
                                        true ->
                                            spawn_analyze(filename:join(["scratch", Version, Project, Test]));
                                        false ->
                                            []
                                    end
                            end, Tests),
                        [spawn_analyze(filename:join(["scratch", Version, Project])) | W0];
                    false ->
                        []
                end
        end, ProjectDirs) ++ [spawn_analyze(filename:join(["scratch", Version]))],

    wait_for_workers(lists:flatten(Workers)),

    application:stop(lager),
    ok.

spawn_analyze(Dir) ->
    spawn(fun() -> analyze(Dir) end).

analyze(Dir) ->
    {ok, F} = file:open(filename:join(Dir, "index.html"), [write]),
    Sub = spawn(fun() ->
                %lager:info("subdirs for ~p are ~p", [Dir, SubDirs]),
                CoverFiles = filelib:fold_files(Dir, ".*.cover.txt", true, fun(E, A) -> [E|A] end, []),
                CD = calculate_coverage(CoverFiles),

                Workers = [spawn(fun() -> write_cover_file(Dir, M, lists:flatten(D)) end) || {M, D} <- CD],

                Modules = group_by_app(coverage_modules(CD)),

                CData = lists:map(fun({App, Mods}) ->
                            {App, [begin
                                            {Run, NotRun} = lists:partition(fun({_, {_, Calls}}) -> Calls > 0 end, coalesce(lists:flatten(proplists:get_value(atom_to_list(M), CD)))),
                                            {M, percent(length(Run), length(Run)+length(NotRun)), {length(Run), length(NotRun)}}
                                    end || M <- Mods]}
                    end, Modules),

                receive
                    lieutenant_green ->
                        %% ok safe to write to the HTML file now
                        lists:foreach(fun({App, Mods}) when Mods /= [] ->
                                    file:write(F, ["<h2>", App,"/<h2><table>"]),
                                    [file:write(F, "<tr><td><a href=\""++atom_to_list(M) ++ ".html\">" ++atom_to_list(M) ++ "</a></td><td>" ++ integer_to_list(Percent) ++ "%</td></tr>") || {M, Percent, _} <- lists:keysort(2, Mods)],
                                    {TotalCovered, TotalNotCovered} = lists:foldl(fun({_, _, {Cov, NotCov}}, {T1, T2}) ->
                                                {T1+Cov, T2+NotCov}
                                        end, {0, 0}, Mods),
                                    Total = percent(TotalCovered, TotalCovered + TotalNotCovered),
                                    file:write(F, ["<tr><td></td><td></td></tr><tr><td>Total</td><td>", integer_to_list(Total), "%</td></tr>"]),
                                    file:write(F, "</table>");
                                (_) ->
                                    ok
                            end, CData),
                        %lager:info("found ~p coverdata files for ~p", [length(CoverData), Dir]),
                        file:write(F, "</table>")
                end,
                wait_for_workers(Workers)
        end),

    SubDirs = case file:list_dir(Dir) of
        {ok, Dirs} ->
            [filename:join([Dir, D]) || D <- Dirs, filelib:is_dir(filename:join(Dir, D))]
    end,
    file:write(F, "<h1>Individual coverage</h1><table>"),
    case SubDirs of
        [] -> ok;
        _ ->
            [summarize(SubDir, F) || SubDir <- lists:sort(SubDirs)],
            file:write(F, "</table>"),
            file:write(F, "<h1>Aggregate coverage</h1><table>")
    end,
    Sub ! lieutenant_green,
    wait_for_workers([Sub]),
    ok.

coalesce([]) -> [];
coalesce(CD0) ->
    CD = lists:keysort(1, CD0),
    {LastCoveredLine, _} = lists:last(CD),
    lists:foldl(fun(Line, Acc) ->
                case proplists:get_all_values(Line, CD) of
                    [] ->
                        Acc;
                    Cov ->
                        Calls = [{Module, Calls} || {Module, Calls} <- Cov, Calls > 0],
                        case Calls of
                            [] ->
                                [{Line, hd(Cov)}|Acc];
                            _ ->
                                [{Line, hd(Calls)}|Acc]
                        end
                end
        end, [], lists:seq(1, LastCoveredLine)).

summarize(Dir, F) ->
    CoverFiles = filelib:fold_files(Dir, ".*.cover.txt", true, fun(E, A) -> [E|A] end, []),
    case CoverFiles of
        [] ->
            case {filelib:is_regular(filename:join(Dir, "failed")), filelib:is_regular(filename:join(Dir, "no_cover"))} of
                {true, _} ->
                    file:write(F, ["<tr><td>",filename:basename(Dir), "</td><td>test failed</td></tr>"]);
                {_, true} ->
                    file:write(F, ["<tr><td>",filename:basename(Dir), "</td><td>no coverage</td></tr>"]);
                _ ->
                    file:write(F, ["<tr><td>",filename:basename(Dir), "</td><td>unknown</td></tr>"])
            end;
        _ ->
            file:write(F, ["<tr><td><a href=\"./", filename:basename(Dir), "\">", filename:basename(Dir), "</a></td><td>"]),
            file:write(F, [total_coverage(calculate_coverage(CoverFiles)), "</td></tr>"])
    end.

read_cover_data(File) ->
    Mod = filename:basename(File, ".cover.txt"),
    TestMod = filename:basename(filename:dirname(File)),
    {ok, Bin} = file:read_file(File),
    RawLines = binary:split(Bin, <<"\n">>, [global]),
    Lines = lists:sublist(RawLines, 5, length(RawLines)),
    {_, Res} = lists:foldl(fun(Line, {LineNo, Dict}) ->
                case re:run(Line, <<"^ +([0-9]+)\.*|">>, [{capture, all_but_first, list}]) of
                    {match, []} ->
                        {LineNo+1, Dict};
                    {match, [NumStr]} ->
                        Num = list_to_integer(NumStr),
                        {LineNo+1, dict:store(LineNo, {TestMod, Num}, Dict)}
                end
        end, {1, dict:new()}, Lines),
    {Mod, dict:to_list(Res)}.

write_cover_file(Dir, Mod, CD) ->
    {ok, F} = file:open(filename:join([Dir, Mod++".html"]), [write]),
    file:write(F, "<html><head>
        <link rel='stylesheet' type='text/css' href='/hint.min.css'>
        <style>
            p {
                margin: 0em;
            }
            p:nth-child(odd)
            {
                background:Gainsboro;
            }
            p:nth-child(even)
            {
                background:LightGray;
            }
            p.run:nth-child(even) {
                background: PaleGreen;
            }
            p.run:nth-child(odd) {
                background: LightGreen;
            }
            p.notrun:nth-child(even) {
                background: LightSalmon;
            }
            p.notrun:nth-child(odd) {
                background: DarkSalmon;
            }
        </style></head><body>"),
    {ok, Bin} = file:read_file(mod_src(list_to_atom(Mod))),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    lists:foldl(fun(Line0, Number) ->
                Line = binary:replace(binary:replace(binary:replace(binary:replace(Line0, <<" ">>, <<"&nbsp;">>, [global]), <<"	">>, <<"&nbsp;&nbsp;">>, [global]), <<"<">>, <<"&lt;">>, [global]), <<">">>, <<"&gt;">>, [global]),
                case proplists:get_all_values(Number, CD) of
                    [] ->
                        file:write(F, ["<p>", Line, "</p>"]);
                    Cov ->
                        Calls = [{Module, Calls} || {Module, Calls} <- Cov, Calls > 0],
                        case Calls of
                            [] ->
                                file:write(F, ["<p class='notrun'>", Line, "</p>"]);
                            _ ->
                                file:write(F, ["<p class='run'><span class='hint  hint--top' data-hint='Covered by: ", string:join([M++"&nbsp;("++integer_to_list(CC)++"&nbsp;calls)" || {M, CC} <- Calls], ", "), "'>", Line, "</span></p>"])
                        end
                end,
                Number + 1
        end, 1, Lines),
    file:write(F, "</body></html>"),
    file:close(F).

calculate_coverage(CoverFiles) ->
    Res = lists:foldl(fun(File, Dict) ->
                {Mod, CD} = read_cover_data(File),
                dict:append(Mod, CD, Dict)
        end, dict:new(), CoverFiles),
    lists:keysort(1, dict:to_list(Res)).

coverage_modules(CD) ->
    lists:sort([list_to_atom(M) || {M, _} <- CD]).

total_coverage(CD) ->
    {A, B} = lists:foldl(fun({_Module, CD2}, {Covered, NotCovered}) ->
                {Run, NotRun} = lists:partition(fun({_, {_, Calls}}) -> Calls > 0 end, coalesce(lists:flatten(CD2))),
                {Covered + length(Run), NotCovered + length(NotRun)}
        end, {0, 0}, CD),
    integer_to_list(percent(A, A+B)) ++ "%".

prepare(ScorecardIDs, Conn, Platform, Version) ->
    filelib:ensure_dir(filename:join(["scratch", Version, "wtf"])),
    lists:map(fun({PName, SID}) ->
                case pgsql_connection:extended_query("SELECT test_results.id, status, tests.name, tags->'backend', tags->'upgrade_version' FROM test_results INNER JOIN tests ON (test_results.test_id = tests.id) WHERE tags->'platform'=$1 AND scorecard_id=$2 ORDER BY status DESC", [Platform, SID], Conn) of
                    {{select, 0}, []} -> ok;
                    {{select, _N}, Results} ->
                        ProjectIndexName = filename:join(["scratch", Version, PName, "index.html"]),
                        filelib:ensure_dir(ProjectIndexName),
                        lists:foldl(fun({TestResultID, Status, TestName0, Backend, UpgradeVersion}, Seen) ->
                                        TestName = list_to_binary(string:join([binary_to_list(T) || T <- [TestName0, Backend, UpgradeVersion], T /= null], "-")),
                                        lager:info("Testname ~p ->? ~p", [TestName0, TestName]),
                                        case lists:member(TestName, Seen) of
                                            false ->
                                                case Status of
                                                    true ->
                                                        %% get the URL of the coverdata
                                                        case pgsql_connection:extended_query("SELECT url FROM artifacts WHERE test_result_id=$1 AND url LIKE '%coverdata%'", [TestResultID], Conn) of
                                                            {{select, 1}, [{URL}]} ->
                                                                case lhttpc:request(binary_to_list(URL), "GET", [], 60000) of
                                                                    {ok, {{200, _}, _Headers, GzBody}} ->
                                                                        file:delete(filename:join(["scratch", Version, PName, TestName, "no_cover"])),
                                                                        file:delete(filename:join(["scratch", Version, PName, TestName, "failed"])),
                                                                        Body = zlib:gunzip(GzBody),
                                                                        Name = case binary:split(lists:last(binary:split(URL, <<"/">>, [global])), <<".gz">>, [trim]) of
                                                                            [<<"eunit.coverdata">>=N] ->
                                                                                list_to_binary([TestName, "-", N]);
                                                                            [N] -> N
                                                                        end,
                                                                        CoverFileName = filename:join(["scratch", Version, PName, TestName, Name]),
                                                                        lager:info("processing ~p", [TestName]),
                                                                        filelib:ensure_dir(CoverFileName),
                                                                        file:write_file(CoverFileName, Body),
                                                                        cover:import(CoverFileName),
                                                                        W = [begin
                                                                                    case is_good(M) of
                                                                                        true ->
                                                                                            cover:async_analyse_to_file(M, binary_to_list(filename:join(["scratch", Version, PName, TestName, atom_to_list(M) ++ ".cover.txt"])));
                                                                                        false ->
                                                                                            []
                                                                                    end
                                                                            end || M <- cover:imported_modules()],
                                                                        wait_for_workers(lists:flatten(W)),
                                                                        lager:info("done processing ~p", [TestName]),
                                                                        cover:stop(),
                                                                        cover:start(),
                                                                        [TestName|Seen];
                                                                    _ ->
                                                                        CoverFileName = filename:join(["scratch", Version, PName, TestName, "no_cover"]),
                                                                        filelib:ensure_dir(CoverFileName),
                                                                        file:write_file(CoverFileName, ""),
                                                                        lager:warning("unable to fetch coverdata for ~p from ~p", [TestName, URL]),
                                                                        Seen
                                                            end;
                                                        {{select, 0}, []} ->
                                                            CoverFileName = filename:join(["scratch", Version, PName, TestName, "no_cover"]),
                                                            filelib:ensure_dir(CoverFileName),
                                                            file:write_file(CoverFileName, ""),
                                                            lager:warning("no coverdata for ~p", [TestName]),
                                                            Seen
                                                end;
                                            false ->
                                                CoverFileName = filename:join(["scratch", Version, PName, TestName, "failed"]),
                                                filelib:ensure_dir(CoverFileName),
                                                file:write_file(CoverFileName, ""),
                                                lager:warning("~p failed", [TestName]),
                                                Seen
                                        end;
                                    true ->
                                        %% already dealt with this
                                        Seen
                                end
                            end, [], Results)
                end
        end, ScorecardIDs).

mod_src(M) ->
    try M:module_info(compile) of
        Props ->
            case filelib:is_regular(proplists:get_value(source, Props)) of
                true ->
                    proplists:get_value(source, Props);
                false ->
                    undefined
            end
    catch _:_ -> undefined
    end.

is_good(M) ->
    case mod_src(M) of
        undefined ->
            false;
        _ ->
            true
    end.

wait_for_workers(Workers) ->
    case lists:all(fun(Pid) -> not erlang:is_process_alive(Pid) end, Workers) of
        true -> ok;
        false ->
            %lager:info("waiting for workers: ~p remain", [length(lists:filter(fun erlang:is_process_alive/1, Workers))]),
            timer:sleep(1000),
            wait_for_workers(Workers)
    end.

percent(0, 0) -> 0;
percent(Part, All) ->
    trunc((Part / All) * 100).

group_by_app(Modules) ->
    lists:sort(dict:to_list(lists:foldl(fun(M, Dict) ->
                App = filename:basename(filename:dirname(filename:dirname(code:which(M)))),
                dict:append(App, M, Dict)
        end, dict:new(), Modules))).

