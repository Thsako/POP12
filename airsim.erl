-module(airsim).
-export([init/2, landingStrip/1, aircraftHandler/2, aircraft/2, fct/2, fctLimit/2, start/2]).

start(Freq, Limit) ->
    {H, M, S} = erlang:time(),
    SumSec = S + Limit,
    case SumSec of
	SumSec when SumSec > 60 ->
	    Min = SumSec div 60,
	    Sec = SumSec rem 60,
	    SumMin = M + Min,
	    case SumMin of
		SumMin when SumMin > 60 ->
		    Hour = SumMin div 60,
		    TempMin = SumMin rem 60,
		    SumHour = H + Hour,
		    case SumHour of
			SumHour when SumHour > 24 ->
			    TempHour = SumHour div 24,
			    RealHour = SumHour rem 24,   
			    NewLimit = {RealHour + TempHour, TempMin, Sec},
			    io:format("Time under 60hour ~p~n", [NewLimit]),
			    init(Freq, NewLimit);
			SumHour ->
			    NewLimit = {SumHour, TempMin, Sec},
			    io:format("Time under 60hour ~p~n", [NewLimit]),
			    init(Freq, NewLimit)
		    end;
	        SumMin ->
		    NewLimit = {H, SumMin, Sec},
		    io:format("Time under 60min ~p~n", [NewLimit]),
		    init(Freq, NewLimit)
	    end;
	SumSec -> 
	    NewLimit = {H, M, SumSec},
	    io:format("Time under 60sec ~p~n", [NewLimit]),
	    init(Freq, NewLimit)
    end.

%% @doc Initiates the flight control simulation.
% -spec(init(integer(), tuple())).
init(Freq, Limit) ->
    PID_FCT = self(),
    NewFreq = Freq / 100,
    spawn_link(fun() -> fctLimit(PID_FCT, Limit) end),
    PID_LS = spawn_link(fun() -> landingStrip(0) end), 
    spawn_link(fun() -> aircraftHandler(PID_FCT, NewFreq) end),
    io:format("startFCT ~p~n", [PID_FCT]),
    fct([], PID_LS).

%% @doc Creates "aircraft" processes.
% -spec(aircraftHandler(pid(), integer()).
aircraftHandler(PID_FCT, Freq) ->
   % io:format("Freq ~p~n", [Freq]),
    Random = random:uniform(),
   % io:format("Random ~p~n", [Random]),
    case Random of 
	Random when Freq > Random ->
	    %io:format("Klarat ~p~n", [Random]),
	    {H, M, S} = erlang:time(),
	    case {H, M, S} of
		{H, M, S} when S > 55 ->
		    TimeRequest = {H, M + 1, 0 + random:uniform(5)},
		    PID_AIR = spawn_link(fun() -> aircraft(PID_FCT, TimeRequest) end),
		    PID_FCT ! {ready, PID_AIR},
		    timer:sleep(1000),
		    aircraftHandler(PID_FCT, Freq);
				     _ ->
                    TimeRequest = {H, M, S+random:uniform(5)},
		    PID_AIR = spawn_link(fun() -> aircraft(PID_FCT, TimeRequest)end),
		    PID_FCT ! {ready, PID_AIR},
		    timer:sleep(1000),
		    aircraftHandler(PID_FCT, Freq)
	    end;
	_ ->
	    timer:sleep(1000),
	    aircraftHandler(PID_FCT, Freq)
    end.

%% @doc Simulation of the aircraft.
% -spec(aircraft(pid(), tuple(), tuple()).
aircraft(PID_FCT, {H1, M1, S1}) ->
    PID_AIR = self(),
    {H2, M2, S2} = erlang:time(),
    case {H2, M2, S2} of
	{H1, M1, S1} when S1 >= S2, M1 >= M2, H1 >= H2 ->
	    PID_FCT ! {request, PID_AIR},
	    receive
		{land, accept} ->
		    timer:sleep(5000),
		    PID_FCT ! {landed, PID_AIR},
		    {PID_AIR, landed};
		{land, deny} -> 
		    {H, M, S} = erlang:time(),
		    case {H, M, S} of
			{H, M, S} when S > 55 ->
			    TimeRequest = {H, M+1, 0 + random:uniform(5)},
			    aircraft(PID_FCT, TimeRequest);
			_ ->
			    TimeRequest = {H, M+1, 0 + random:uniform(5)},
			    aircraft(PID_FCT, TimeRequest)
		    end
	    end;
	_ ->
	    aircraft(PID_FCT, {H1, M1, S1})
   end.

%% @doc Terminates the simulation.
%-spec(fctLimit(pid(), tupe())).		    
fctLimit(PID_FCT, {H1, M1, S1}) ->
    {H2, M2, S2} = erlang:time(),
    case {H2, M2, S2} of
	{H2, M2, S2} when H2 >= H1, M2 >= M1, S2 >= S1 ->
	    PID_FCT ! {exit, ok};
	_  -> 
	    fctLimit(PID_FCT, {H1, M1, S1})
    end.

%% @doc Controlls the airtraffic.
%-spec(fct(list(), pid())).
fct(FlightList, PID_LS) ->
    PID_FCT = self(),
    receive 
	{ready, PID_AIR} ->
	    NewFlightList = [PID_AIR|FlightList],
	    io:format("added ~p~n", [PID_AIR]),
	    fct(NewFlightList, PID_LS);
	{request, PID_AIR} ->
	    PID_LS ! {PID_FCT, request},
	    receive
		{request, accepted} ->
		    io:format("accepted ~p~n", [PID_AIR]),
		    PID_AIR ! {land, accept},
		    fct(FlightList, PID_LS);
		{request, denied} ->
		    io:format("denied ~p~n", [PID_AIR]),
		    PID_AIR ! {land, deny},
		    fct(FlightList, PID_LS)
	    end;
	{landed, PID_AIR} ->
	    io:format("landed ~p~n", [PID_AIR]),
	    PID_LS ! {PID_FCT, landed},
	    NewFlightList = lists:delete(PID_AIR, FlightList),
	    fct(NewFlightList, PID_LS);
	{exit, ok} -> 
	    io:format("TERMINATE ~p~n", [PID_FCT]),
	    Reason = "Terminal closed.",
	    exit(Reason)
    end.
	    
%% @doc Checking if the landingStrip is empty.
%-spec(langingStrip(integer()).
landingStrip(State) ->
    receive
	{PID_FCT, request} ->
	    case State of
		State when State =:= 0 ->
		    NewState = 1,
		    PID_FCT ! {request, accepted},
		    landingStrip(NewState);
		_ ->
		    PID_FCT ! {request, denied},
		    landingStrip(State)
            end;
	{PID_FCT, landed} ->
	    NewState = 0,
	    landingStrip(NewState)
    end.
