-module(atest).
-export([init/2, fct/1, aircraft/5, spawnAircraft/3]).

init(Freq, Limit) -> PIDFCT = spawn(fun() -> fct([]) end),
	spawnAircraft(Freq, Limit, PIDFCT).
					  
spawnAircraft(Freq, {H1, M1, S1}, PIDFCT) ->
	timer:sleep(timer:seconds(Freq)),
	{H2, M2, S2} = erlang:time(),
		case {H1, M1, S1} of
			{H2, M2, S2} when S2 >= S1 -> timeOut;
		_ ->
			case {H2, M2, S2} of
				{H2, M2, S2} when S2 >= 56 ->
					TimeToLand = {H2, M2+1, 0+random:uniform(5)},
					PIDAIR = spawn(fun() -> aircraft(Freq, {H1, M1, S1},
					PIDFCT, TimeToLand, {H2, M2, S2}) end),
					spawnAircraft(Freq, {H1, M1, S1}, PIDFCT);
				_ -> 
					TimeToLand = {H2, M2, S2+random:uniform(5)},
					PIDAIR = spawn(fun() -> aircraft(Freq, {H1, M1, S1}, 
					PIDFCT, TimeToLand, {H2, M2, S2}) end),
					spawnAircraft(Freq, {H1, M1, S1}, PIDFCT)
			end
		end.

aircraft(Freq, {H1, M1, S1}, PIDFCT, TimeToLand, {H2, M2, S2}) ->
	PIDAIR = self(),
	case erlang:time() of
		{H2, M2, S2} -> 
			PIDFCT ! {PIDAIR, add},
				receive 
					{PID, added} -> io:format("added ~p~n", [PID]),
						aircraft(Freq, {H1, M1, S1}, PIDFCT, TimeToLand,
						{H2, M2-1, S2})
				end;
		TimeToLand -> 
			PIDFCT ! {PIDAIR, request},
				receive
					{PID, accepted} -> 
						timer:sleep(5000),
						io:format("landed ~p~n", [PID]),
						PIDFCT ! {PIDAIR, landed};
					{PID, denied} ->
						io:format("denied ~p~n", [PID]),
						{H3, M3, S3} = erlang:time(),
						case {H3, M3, S3} of
							{H3, M3, S3} when S3 >= 56 ->
								NewTimeToLand = {H3, M3+1, 0+random:uniform(5)},
								aircraft(Freq, {H1, M1, S1}, PIDFCT, 
								NewTimeToLand, {H3, M3, S3});
							_ -> 
								NewTimeToLand = {H3, M3, S3+random:uniform(5)},
								aircraft(Freq, {H1, M1, S1}, PIDFCT, 
								NewTimeToLand, {H3, M3, S3})
						end
				end;
		_ ->
			aircraft(Freq, {H1, M1, S1}, PIDFCT, TimeToLand, {H2, M2, S2})
	end.
	
fct(FlightList) ->
	%PIDFCT = self(),
	receive
		{PIDAIR, add} ->
			NewFlightList = [{PIDAIR, 0}|FlightList],
			PIDAIR ! {PIDAIR, added},
			fct(NewFlightList);
		{PIDAIR, request} ->
			Check = lists:keyfind(1, 2, FlightList),
			case Check of 
				false -> 
					NewFlightList = [{PIDAIR1, X+1}||{PIDAIR1, X} 
					<- FlightList, PIDAIR == PIDAIR1], 
					PIDAIR ! {PIDAIR, accepted},
					fct(NewFlightList);
				_ -> 
					NewFlightList = lists:keydelete(PIDAIR, 1, FlightList),
					PIDAIR ! {PIDAIR, denied},
					fct(NewFlightList)
			end;
		{PIDAIR, landed} ->
			NewFlightList = lists:keydelete(PIDAIR, 1, FlightList),
			fct(NewFlightList)
	end.
