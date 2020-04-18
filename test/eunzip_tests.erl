%% @author: Maxim Pushkar
%% @date: 17.04.2020

-module(eunzip_tests).

%% Include files
-include_lib("eunit/include/eunit.hrl").
-include("eunzip.hrl").

%% API
-export([]).

%% Macros
-define(eunzip, eunzip).

%% API
open_close_test() ->
    OpenResult = eunzip:open(otp_23_readme_zip()),
    ?assertMatch({ok, _}, OpenResult),
    {ok, UnzipState} = OpenResult,
    ?assertEqual(ok, eunzip:close(UnzipState)).

entries_test() ->
    OpenResult = eunzip:open(otp_23_readme_zip()),
    ?assertMatch({ok, _}, OpenResult),
    {ok, UnzipState} = OpenResult,
    CdFileName = otp_23_readme_filename(),
    ?assertMatch({ok, [#cd_entry{file_name = CdFileName}]}, eunzip:entries(UnzipState)),
    ?assertEqual(ok, eunzip:close(UnzipState)).

entries2_test() ->
    OpenResult = eunzip:open(specs_zip()),
    ?assertMatch({ok, _}, OpenResult),
    {ok, UnzipState} = OpenResult,
    ?assertMatch({ok, [_, _, _]}, eunzip:entries(UnzipState)),
    ?assertEqual(ok, eunzip:close(UnzipState)).

entry_test() ->
    OpenResult = eunzip:open(otp_23_readme_zip()),
    ?assertMatch({ok, _}, OpenResult),
    {ok, UnzipState} = OpenResult,
    CdFileName = otp_23_readme_filename(),
    ?assertMatch({ok, #cd_entry{file_name = CdFileName}}, eunzip:entry(UnzipState, CdFileName)),
    ?assertEqual(ok, eunzip:close(UnzipState)).

verify_test() ->
    OpenResult = eunzip:open(otp_23_readme_zip()),
    ?assertMatch({ok, _}, OpenResult),
    {ok, UnzipState} = OpenResult,
    ?assertEqual(ok, eunzip:verify(UnzipState, otp_23_readme_filename())),
    ?assertEqual(ok, eunzip:close(UnzipState)).

decompress_test() ->
    OpenResult = eunzip:open(otp_23_readme_zip()),
    ?assertMatch({ok, _}, OpenResult),
    {ok, UnzipState} = OpenResult,
    CdFileName = otp_23_readme_filename(),
    ?assertEqual(ok, eunzip:decompress(UnzipState, CdFileName, CdFileName)),
    file:delete(otp_23_readme_filename()),
    ?assertEqual(ok, eunzip:close(UnzipState)).

stream_test() ->
    OpenResult = eunzip:open(otp_23_readme_zip()),
    ?assertMatch({ok, _}, OpenResult),
    {ok, UnzipState} = OpenResult,
    CdFileName = otp_23_readme_filename(),
    StreamInitResult = eunzip:stream_init(UnzipState, CdFileName),
    ?assertMatch({ok, _}, StreamInitResult),
    {ok, StreamState} = StreamInitResult,
    ExpectedMd5 = <<224, 197, 17, 243, 183, 239, 151, 148, 52, 62, 2, 208, 63, 223, 55, 59>>,
    Md5State = crypto:hash_init(md5),
    StreamResult = stream_fun(StreamState, Md5State),
    ?assertMatch({ok, _}, StreamResult),
    ?assertMatch({ok, ExpectedMd5}, StreamResult),
    ?assertEqual(ok, eunzip:close(UnzipState)).

%% Internal functions
test_path(FileName) ->
    Dir = code:lib_dir(?eunzip),
    Path = filename:join([Dir, "test", "files", FileName]),
    iolist_to_binary(Path).

otp_23_readme_zip() ->
    test_path("otp_23_readme.zip").

otp_23_readme_filename() ->
    <<"otp_src_23.0-rc2.readme.txt">>.

specs_zip() ->
    test_path("specs.zip").

stream_fun(StreamState, Md5State) ->
    case eunzip:stream_read_chunk(?file_chunk_size, StreamState) of
        {ok, Data} ->
            Md5State1 = crypto:hash_update(Md5State, Data),
            {ok, crypto:hash_final(Md5State1)};
        {more, Data, StreamState1} ->
            stream_fun(StreamState1, crypto:hash_update(Md5State, Data));
        {error, Reason} ->
            {error, Reason}
    end.
