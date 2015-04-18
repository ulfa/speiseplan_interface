%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
-module(bookings).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([start/0]).
-export([get_eater/1, get_eater/2]).
%% ====================================================================
%% External functions
%% ====================================================================
get_eater(Year) ->
    gen_server:call({global, ?MODULE}, {get_eater, Year}).

get_eater(Year, Month) ->
    gen_server:call({global, ?MODULE}, {get_eater, Year, Month}).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {}).
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

start() ->
	start_link().
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({get_eater, Year}, From, State) ->
    Menus = get_menus_for_year(Year),
    {Intern, Extern} = analyse_menus(year, Menus, array:new(12, {default,0}), array:new(12, {default,0})),
    {reply, {Year, [{intern, Intern}, {extern, Extern}]}, State};

handle_call({get_eater, Year, Month}, From, State) ->
    Menus = get_menus_for_month(Year, Month),
    {Intern, Extern} = analyse_menus(month, Menus, 0, 0),
    {reply, {Year, Month, [{intern, Intern}, {extern, Extern}]}, State};

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
get_menus_for_year(Year) when is_list(Year) ->
    get_menus_for_year(list_to_integer(Year));
get_menus_for_year(Year) ->
    boss_db:find(menu, [{date, 'ge', {{Year, 1,1}, {0,0,0}}}, {date, 'le', {{Year, 12, 31}, {0,0,0}}}]).


get_menus_for_month(Year, Month) when is_list(Year) ->  
    get_menus_for_month(list_to_integer(Year), list_to_integer(Month));
get_menus_for_month(Year, Month) -> 
    boss_db:find(menu, [{date, 'ge', {{Year, Month,1}, {0,0,0}}}, {date, 'le', {{Year, Month, calendar:last_day_of_the_month(Year, Month)}, {0,0,0}}}]).

analyse_menus(month,[], Intern, Extern) ->
    {Intern, Extern};   
analyse_menus(month, [H|T], Intern, Extern) ->
    {Int_count, Ext_count} = count_bookings(H:booking()),
    analyse_menus(month,T, Int_count + Intern, Ext_count + Extern);

analyse_menus(year,[], Intern, Extern) ->
    {list_of_integer_to_string(array:to_list(Intern)), list_of_integer_to_string(array:to_list(Extern))};   
analyse_menus(year, [H|T], Intern, Extern) ->
    {{_Y, Month, _D}, _Time} = H:date(),
    {Int_count, Ext_count} = count_bookings(H:booking()),
    analyse_menus(year,T, add(Month, Int_count, Intern), add(Month, Ext_count, Extern)).

count_bookings(Bookings) ->
    count_bookings(Bookings, 0, 0). 
count_bookings([], Intern, Extern) ->
    {Intern, Extern};
count_bookings([H|T], Intern, Extern) ->
    Eater = H:eater(),
    case Eater:intern() of
        true -> count_bookings(T, Intern + 1, Extern);
        false ->count_bookings(T, Intern, Extern + 1)
    end.    

add(Month, Value, Array) ->
    Index = Month - 1,
    Act_value = array:get(Index, Array),
    array:set(Index, Act_value + Value, Array).

list_of_integer_to_string(List_of_integer) ->
    string:join([integer_to_list(S) || S <- List_of_integer],",").

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.