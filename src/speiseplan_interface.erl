%%
%% Copyright (c) 2013 Ulf Angermann  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann ua@innoq.com
%%% Description : 
%%%
%%% Created : 03.09.2014
-module(speiseplan_interface).

%% Application callbacks
-export([start/0, stop/0]).

start() ->  
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).  

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
