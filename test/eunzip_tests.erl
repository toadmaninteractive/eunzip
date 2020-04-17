%% @author: Maxim Pushkar
%% @date: 17.04.2020

-module(eunzip_tests).

%% Include files
-include_lib("eunit/include/eunit.hrl").

%% Macros
-define(eunzip, eunzip).

%% API
verify_test() ->
    OpenResult = eunzip:open(otp_23_readme_zip()),
    ?assertMatch({ok, _}, OpenResult, "Failed to open a ZIP archive"),
    {ok, UnzipState} = OpenResult,
    ?assertEqual(ok, eunzip:verify(UnzipState, otp_23_readme_filename()), "CRC32 mismatch"),
    ?assertEqual(ok, eunzip:close(UnzipState), "Failed to close file").

deflate_test() ->
    OpenResult = eunzip:open(otp_23_readme_zip()),
    ?assertMatch({ok, _}, OpenResult, "Failed to open a ZIP archive"),
    {ok, UnzipState} = OpenResult,
    ?assertEqual(ok, eunzip:decompress(UnzipState, otp_23_readme_filename(), otp_23_readme_filename()), "Failed to decompress file"),
    file:delete(otp_23_readme_filename()),
    ?assertEqual(ok, eunzip:close(UnzipState), "Failed to close file").

%% Internal functions
otp_23_readme_filename() ->
    <<"otp_src_23.0-rc2.readme.txt">>.

otp_23_readme_zip() ->
    Dir = code:lib_dir(?eunzip),
    Path = filename:join([Dir, "test", "files", "otp_23_readme.zip"]),
    iolist_to_binary(Path).
