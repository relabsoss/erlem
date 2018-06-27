-module(nmea0183).
-export([parse/1]).

-define(UNIX_1970, 62167219200).

parse(Line) ->
  case verify_checksum(Line) of
    true ->
      case re:split(Line, ",", [{return,list}]) of
        ["$GPGGA", _UTC, Lat, LatNS, Long, LongEW, Quality, _SatCount | _Various] ->
          case {string_lat_dec(Lat, LatNS), string_long_dec(Long, LongEW)} of
            {LatS, LngS} when (LatS =/= undefined) and (LngS =/= undefined) ->
              #{
                lat => LatS,
                lng => LngS, 
                time => undefined,
                date => undefined,
                speed => undefined,
                quality => Quality 
              };
            _ ->
              undefined
          end;
        ["$GPRMC", UTC, Status, Lat, LatNS, Long, LongEW, Speed, _TrackAngle, Date | _Various] ->
          case Status =:= "A" of
            true -> 
              #{
                lat => string_lat_dec(Lat, LatNS),
                lng => string_long_dec(Long, LongEW), 
                time => string_time(UTC),
                date => string_date(Date),
                speed => string_knot_kmh(Speed),
                quality => undefined
              };
            false ->
              undefined
          end;
        ["$GPGLL", Lat, LatNS, Long, LongEW, UTC, Status | _Various ] -> 
          case Status =:= "A" of
            true -> 
              #{
                lat => string_lat_dec(Lat, LatNS),
                lng => string_long_dec(Long, LongEW), 
                time => string_time(UTC),
                date => undefined,
                speed => undefined,
                quality => undefined
              };
            false ->
              undefined
          end;
        ["$GPGSA",_Select,_Fix | _Various ] -> 
          %% DOP and active satellites
          undefined;
        ["$GPGSV",_Num,_I | _Various ] -> 
          undefined;
        ["$GPVTG" | _Various ] -> 
          undefined;    
        _ ->
          undefined
      end;
    false ->
      undefined
  end.

verify_checksum([$$ | Cs]) ->  checksum(Cs, 0);
verify_checksum(_) -> false.

checksum([$*, X1, X2|_], Sum) -> list_to_integer([X1, X2], 16) =:= Sum;
checksum([$\r, $\n], _Sum) -> true;
checksum([C | Cs], Sum) ->  checksum(Cs, C bxor Sum);
checksum([], _) -> false.

string_lat_dec(Lat, "N") -> string_deg_to_dec(Lat);
string_lat_dec(Lat, "S") -> -string_deg_to_dec(Lat);
string_lat_dec(_Lat,  _) -> undefined.

string_long_dec(Long, "E") -> string_deg_to_dec(Long);
string_long_dec(Long, "W") -> -string_deg_to_dec(Long);
string_long_dec(_Long,  _) -> undefined.

string_date(String) ->
  {D0, _} = string:to_integer(String),
  Year = (D0 rem 100) + 2000,
  D1 = D0 div 100,
  Month = D1 rem 100,
  D2 = D1 div 100,
  Day = (D2 rem 100),
  [Day, Month, Year].

string_time(String) ->
  {T0f, _} = string:to_float(String ++ "."),
  T0 = trunc(T0f),
  Sec = T0 rem 100,
  T1 = T0 div 100,
  Min = T1 rem 100,
  T2 = T1 div 100,
  Hour = T2 rem 100,
  [Hour, Min, Sec].

string_knot_kmh(Speed) ->
  case string:to_float(Speed) of
    {Knot, ""} -> knot_to_kmh(Knot);
    _ -> undefined
  end.

%% convert degree to decimal
string_deg_to_dec(String) ->
  case string:to_float(String) of
    {Deg, ""} -> deg_to_dec(Deg);
    _ -> undefined
  end.

deg_to_dec(Deg) ->
  D = trunc(Deg) div 100,
  Min = Deg - (float(D) * 100.0),
  D + (Min / 60.0).

knot_to_kmh(Knot) ->
  Knot * 1.852.
