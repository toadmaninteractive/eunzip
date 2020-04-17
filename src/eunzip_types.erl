%% @author: Maxim Pushkar
%% @date: 17.04.2020

-module(eunzip_types).

%% Include files
-include("eunzip.hrl").

%% Types
-type unzip_state() :: #unzip_state{}.
-export_type([unzip_state/0]).

-type cd_info() :: #cd_info{}.
-export_type([cd_info/0]).

-type cd_entry() :: #cd_entry{}.
-export_type([cd_entry/0]).

-type file_buffer() :: #file_buffer{}.
-export_type([file_buffer/0]).

-type direction() :: 'backward' | 'forward'.
-export_type([direction/0]).
