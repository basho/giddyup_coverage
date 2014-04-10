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

    %% select test_results.id, tests.name from test_results INNER JOIN tests ON (test_results.test_id = tests.id) WHERE scorecard_id=93 AND tags->'platform'='freebsd-9-64' AND status='t';
    %% select url from artifacts where test_result_id=47332 AND url LIKE '%coverdata%';
    
    VersionIndexName = filename:join([Version, "index.html"]),
    filelib:ensure_dir(VersionIndexName),
    {ok, VersionIndex} = file:open(VersionIndexName, [write]),
    init_analyze(VersionIndex),
    
    Blah = lists:foldl(fun({PName, SID}, Acc) ->
                case pgsql_connection:extended_query("select test_results.id, tests.name from test_results INNER JOIN tests ON (test_results.test_id = tests.id) WHERE status='t' AND tags->'platform'=$1 AND scorecard_id=$2", [Platform, SID], Conn) of
                    {{select, 0}, []} -> Acc;
                    {{select, _N}, Results} ->
                        ProjectIndexName = filename:join([Version, PName, "index.html"]),
                        filelib:ensure_dir(ProjectIndexName),
                        {ok, ProjectIndex} = file:open(ProjectIndexName, [write]),
                        init_analyze(ProjectIndex),
                        pre_analyze(VersionIndex, PName),
                        R = [begin
                                    %% get the URL of the coverdata
                                    case pgsql_connection:extended_query("SELECT url FROM artifacts WHERE test_result_id=$1 AND url LIKE '%coverdata%'", [TestResultID], Conn) of
                                        {{select, 1}, [{URL}]} ->
                                            case lhttpc:request(binary_to_list(URL), "GET", [], 60000) of
                                                {ok, {{200, _}, _Headers, GzBody}} ->
                                                    Body = zlib:gunzip(GzBody),
                                                    Name = case binary:split(lists:last(binary:split(URL, <<"/">>, [global])), <<".gz">>, [trim]) of
                                                        [<<"eunit.coverdata">>=N] ->
                                                            list_to_binary([TestName, "-", N]);
                                                        [N] -> N
                                                    end,
                                                    file:write_file(Name, Body),
                                                    cover:import(Name),
                                                    Index = filename:join([Version, PName, TestName, "index.html"]),
                                                    filelib:ensure_dir(Index),
                                                    {ok, F} = file:open(Index, [write]),
                                                    init_analyze(F),
                                                    pre_analyze(ProjectIndex, TestName),
                                                    {Total, {TotalCovered, TotalNotCovered}} = analyze(F, ProjectIndex, TestName, binary_to_list(filename:join([Version, PName, TestName]))),
                                                    file:sync(ProjectIndex),
                                                    %% XXX cover doesn't reset if there were no compiled moduled
                                                    cover:stop(),
                                                    cover:start(),
                                                    {TestName, Name, {TotalCovered, TotalNotCovered}};
                                                _ ->
                                                    lager:warning("unable to fetch coverdata for ~p from ~p", [TestName, URL]),
                                                    []
                                            end;
                                        {{select, 0}, []} ->
                                            lager:warning("no coverdata for ~p", [TestName]),
                                            []
                                    end
                            end || {TestResultID, TestName} <- Results],
                        file:write(ProjectIndex, "</table>"),
                        file:write(ProjectIndex, "<h1>Aggregate coverage:</h1><table>"),

                        [cover:import(F) || {_, F, _} <- R],
                        analyze(ProjectIndex, VersionIndex, PName, filename:join([Version, PName])),
                        %% XXX cover doesn't reset if there were no compiled moduled
                        cover:stop(),
                        cover:start(),
                        [{PName, R} | Acc]
                        %lists:keystore(Project, 1, Acc, {Project, lists:flatten(R ++ proplists:get_value(Platform, Acc, []))})
                end
        end, [], ScorecardIDs),

    io:format("~p~n", [Blah]),

    [ [cover:import(F) || {_, F, _} <- R] || {_Platform, R} <- Blah ],
    file:write(VersionIndex, "</table>"),
    file:write(VersionIndex, "<h1>Aggregate coverage:</h1><table>"),

    analyze(VersionIndex, undefined, Version, Version),

    application:stop(lager),
    ok.

init_analyze(F) ->
    file:write(F, "<h1>Individual coverage:</h1><table>").

pre_analyze(F2, Name) ->
    file:write(F2, ["<tr><td><a href=\"./", Name, "/\">", Name, "</a></td>"]).

analyze(F, F2, Name, Dir) ->
    Modules = cover:imported_modules(),
    CData = lists:foldl(fun(M, Acc2) ->
                %% don't crash if the module is not in the code path
                try M:module_info() of
                    _ ->
                        case cover:analyze(M, coverage, module) of
                            {ok, {M, {0, 0}}} ->
                                [{M, 0, {0, 0}} | Acc2];
                            {ok, {M, {Covered, NotCovered}}} ->
                                File = filename:join([Dir, atom_to_list(M) ++ ".html"]),
                                cover:analyse_to_file(M, File, [html]),
                                [{M, trunc((Covered / (Covered + NotCovered)) * 100), {Covered, NotCovered}} | Acc2];
                            {error, Reason} ->
                                lager:warning("Cover analysis for ~p failed: ~p", [M, Reason]),
                                Acc2
                        end
                    catch
                        _:_ ->
                            Acc2
                    end
        end, [], Modules),
    [file:write(F, "<tr><td><a href=\""++atom_to_list(M) ++ ".html\">" ++atom_to_list(M) ++ "</a></td><td>" ++ integer_to_list(Percent) ++ "%</td></tr>") || {M, Percent, _} <- lists:keysort(2, CData)],
    {TotalCovered, TotalNotCovered} = lists:foldl(fun({_, _, {Cov, NotCov}}, {T1, T2}) ->
                {T1+Cov, T2+NotCov}
        end, {0, 0}, CData),
    Total = percent(TotalCovered, TotalCovered + TotalNotCovered),
    file:write(F, ["<tr><td></td><td></td></tr><tr><td>Total</td><td>", integer_to_list(Total), "%</td></tr>"]),
    file:write(F, "</table>"),
    file:close(F),
    case F2 of
        undefined ->
            ok;
        _ ->
            file:write(F2, ["<td>", integer_to_list(Total),"%</td></tr>"])
    end,
    lager:info("total coverage for ~p is ~p", [Name, Total]),

    {Total, {TotalCovered, TotalNotCovered}}.

percent(0, 0) -> 0;
percent(Part, All) ->
    trunc((Part / All) * 100).
