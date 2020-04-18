# eunzip
Simple unzip library with Zip64 support

## Overview

`Eunzip` allows opening, verifying and decompressing of Zip files > 2GB and Zip64 archives, which are not supported by the standard Erlang `zip` module.

Supports STORE and DEFLATE compression methods.

## Basic usage

```erlang
% Open a Zip file and read its structure
{ok, UnzipState} = eunzip:open("foo/bar.zip")

% Get all file and directory entries from a Zip archive
{ok, Entries} = eunzip:entries(UnzipState)

% Get specific file or directory entry from a Zip archive
{ok, Entry} = eunzip:entry(UnzipState, "README.md")

% Verify an archived file checksum
ok = eunzip:verify(UnzipState, "README.md")

% Decompress file from a Zip archive into as a target filename
ok = eunzip:decompress(UnzipState, "README.md", "unpacked/README.md")

% Close a previously opened Zip file
ok = eunzip:close(UnzipState)
```

*Note:* closing a previously opened Zip file is mandatory.

## File streaming

Archived file streaming is a technique allowing to process decompressed file by chunks and perform arbitrary actions.

The example below calculates MD5 checksum on-the-fly.

```erlang
stream_md5(ZipFile, ArchivedFile) ->
    {ok, UnzipState} = eunzip:open("foo/bar.zip"),
    {ok, StreamState} = eunzip:stream_init(UnzipState, "hugefile.mkv"),
    {ok, Md5} = stream_fun(StreamState, crypto:hash_init(md5)),
    ok = eunzip:close(UnzipState),
    Md5.

stream_fun(StreamState, Md5State) ->
    % Read 1 MB chunks of archived data per call
    case eunzip:stream_read_chunk(1024 * 1024, StreamState) of
        {ok, Data} ->
            Md5State1 = crypto:hash_update(Md5State, Data),
            {ok, crypto:hash_final(Md5State1)};
        {more, Data, StreamState1} ->
            stream_fun(StreamState1, crypto:hash_update(Md5State, Data));
        {error, Reason} ->
            {error, Reason}
    end.
```
*Note:* stream is closed automatically if all of the file data is read or an error occured.

If you don't want to read until the end of the file, you should call `eunzip:stream_end/1` function:

```erlang
ok = eunzip:stream_end(StreamState)
```

## Credits
Eunzip is based on the [Unzip](https://github.com/akash-akya/unzip) Elixir library by [Akash Hiremath](https://github.com/akash-akya).
