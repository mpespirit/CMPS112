/* 
 *
 * Airline Reservation Planner
 * Written by: Spenser Estrada and Paula Espiritu
 *
 */

not( X ) :- X, !, fail.
not( _ ).

/* Convert degrees/minutes to radians */
convert_to_rad( degmin(Deg, Min), Rad ) :-
    Rad is ( Deg + (Min / 60) ) * (pi / 180).

/* Convert time to Hours */
time_in_hrs( time(Hours, Mins), Hoursonly) :-
   Hoursonly is Hours + Mins / 60.  

/* 
 * Haversine Distance formula 
 * Calulate circle path between 2 points
 */
haversine( Lat1, Lon1, Lat2, Lon2, Distance ) :- 
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2 
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt(A), sqrt(1 - A) ),
   Distance is Dist * 3961. 

/*
 * Calculate Distance between 2 values
 * arrival time:
 *     retrieve lat/lon for both airports from database
 *     convert values to radians
 *     calculate haversine value
 *        return Miles
 *     let arrival time be elapsed + departure time
 */
arrival_time( Start, End, Dept_time, Arr_time ) :-
   airport( Start, _, degmin(D_lat1, M_lat1), degmin(D_lon1, M_lon1) ),
   airport( End, _, degmin(D_lat2, M_lat2), degmin(D_lon2, M_lon2) ),

   convert_to_rad( degmin(D_lat1, M_lat1), R_lat1 ),
   convert_to_rad( degmin(D_lat2, M_lat2), R_lat2 ),
   convert_to_rad( degmin(D_lon1, M_lon1), R_lon1 ),
   convert_to_rad( degmin(D_lon2, M_lon2), R_lon2 ), 

   haversine( R_lat1, R_lon1, R_lat2, R_lon2, Distance ),
   Arr_time is Distance / 500 + Dept_time. 



/* If number is from 0-9, add a leading zero */
leading_zero( Num ) :- 
   Num < 10,
      print( 0 ),
      print( Num ).

/* Else, do nothing */
leading_zero( Num ) :-
   Num > 9, 
      print( Num ). 

/* 
 * Format time for printing
 */

format_time( Hoursonly ) :-
   Hours is floor( Hoursonly ),
   Mins is ( Hoursonly - Hours ) * 60,
   Temp is floor( Mins / 60 ), 
   New_hours is Temp + Hours,
   New_mins is round( Mins - ( Temp * 60 ) ),

   leading_zero( New_hours ), print( ':' ), leading_zero( New_mins ).



/* Base Case: nothing to write */
writepath( [] ) :-
   nl.

/* 
 * Case 1: Direct flight to/from airport 
 *    Retrive airport names
 *    Print in format:
 *        Origin_Code      - Origin_Name      - Departure_Time
 *        Destination_Code - Destination_Name - Arrival_Time
 */
writepath( [[ Dept, Last_dept_time, Last_arr_time ], Arr | []] ) :-
   airport( Dept, Dept_name, _, _ ),
   airport( Arr, Arr_name, _, _ ),

   write( ' depart ' ),
      write( Dept ), write( ' ' ), write( Dept_name ), 
      format_time( Last_dept_time ), nl, 

   write( ' arrive ' ),
      write( Arr ), write( ' ' ), write( Arr_name ),
      format_time(Last_arr_time), nl,

   !, true. 


/* 
 * Case 2: Flight with multiple Stops 
 *
 *
 *
 */
writepath( [[Dept, Last_dept_time, Last_arr_time],
            [Arr, New_dept_time, New_arr_time] | Next_stop]) :- 
   airport( Dept, Dept_name, _, _ ),
   airport( Arr, Arr_name, _, _ ),

   write( ' depart ' ),
      write( Dept ), write( ' ' ), write( Dept_name ),
      format_time( Last_dept_time ), nl,

   write( ' arrive ' ),
      write( Arr ), write( ' ' ), write( Arr_name ),
      format_time( Last_arr_time ), nl,

   !, writepath( [[Arr, New_dept_time, New_arr_time] | Next_stop]).


/* Base Case: Check if successfully reached destination */
listpath( Arr, Arr, _, [Arr], _ ).

/* 
 * Case 1: Find direct path to location
 *    Retrieve flight info from database
 *    Check if node has been visited already
 *    Calculate arrival time to destination 
 *    Recursive call on List path to destination
 *    Error here
 */ 
listpath( Dept, Arr, Stops, [[Dept, Dept_time, Arr_time] | List], T) :-
   flight( Dept, Arr, T ),
   not( member( Arr, Stops ) ),
   time_in_hours( T, Dept_time ),
   arrival_time( Dept, Arr, Dept_time, Arr_time ), 
   Arr_time < 24.0,
      listpath( Arr, Arr, [Arr | Stops], List, _ ).

/* 
 * Case 2: Find path to location with stopovers
 *   Retrieve flight info from database to stopover 
 *   Check if node has been visited already
 *   Calculate arrival time to destination
 *   Retrieve all paths from database from stopover
 *   Check if there is at least 30 Min for a transfer
 *   Recursive call to destination
 */
listpath( Dept, Arr, Stops, [[Dept, Dept_time, Arr_time] | List], T) :-
   flight( Dept, Stop, T ),
   not( member( Stop, Stops ) ),
   time_in_hrs( T, Dept_time ),
   arrival_time( Dept, Stop, Dept_time, Arr_time ),
   Arr_time < 24.0,
      /* Find flight from layover to next location */
      flight( Stop, _, Stop_dept_time ),
      time_in_hrs( Stop_dept_time, New_dept_time ),
      Transfer_time is New_dept_time - Arr_time - 0.5,
      Transfer_time >= 0,
         listpath( Stop, Arr, [Stop | Stops], List, Stop_dept_time). 



/* Case 1: Departing and arriving from same location */
fly( Dept, Dept ) :- 
   write( 'Error: Duplicate departure and arrival airports.' ), nl,
   !, fail.

/* Case 2: Flight path found */
fly( Dept, Arr ) :-
   airport( Dept, _, _, _ ),
   airport( Arr, _ , _, _ ),
   listpath( Dept, Arr, [Dept], List, _ ),
   !, nl,
   writepath( List ),
   true.

/* Case 3: No path from origin to destination */
fly( Dept, Arr ) :- 
   airport( Dept, _, _, _ ), 
   airport( Arr, _, _, _ ),
   write( 'Error: No path found.' ), nl,
   !, fail.

/* Case 4: Invalid airport selection */
fly( _, _ ) :-
   write( 'Error: Airport selected does not exist.' ), nl,
   !, fail.
