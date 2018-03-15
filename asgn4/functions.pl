/* 
 *
 * Airline Reservation Planner
 * Written by: Spenser Estrada and Paula Espiritu
 *
 */

not( X ) :- X, !, fail.
not( _ ).

/* Convert degrees/minutes to radians */
convert_to_rad( degmin(deg, min), result ) :-
    result is ( deg + (min / 60) ) * (pi / 180).

/* Find number of hours based on distance */
convert_to_hrs( miles, hours) :-
   hours is miles / 500.

/* Convert time to hours */
time_in_hrs( time( hours, mins ), hoursonly) :-
   hoursonly is hours + mins / 60.  

/* 
 * Haversine distance formula 
 * Calulate circle path between 2 points
 */
haversine( lat1, lon1, lat2, lon2, distance ) :- 
   dlon is lon2 - lon1,
   dlat is lat2 - lat1,
   a is sin( dlat / 2 ) ** 2 
      + cos( lat1 ) * cos( lat2 ) * sin( dlon / 2 ) ** 2,
   dist is 2 * atan2( sqrt(a), sqrt(1 - a) ),
   distance is dist * 3961. 

/*
 * Calculate distance between 2 values
 * arrival time:
 *     retrieve lat/lon for both airports from database
 *     convert values to radians
 *     calculate haversine value
 *        return miles
 *     let arrival time be elapsed + departure time
 */
arrival_time( start, end, dept_time, arr_time ) :-
   airport( start, _, degmin(d_lat1, m_lat1), degmin(d_lon1, m_lon1) ),
   airport( end, _, degmin(d_lat2, m_lat2), degmin(d_lon2, m_lon2)),

   convert_to_rad( degmin(d_lat1, m_lat1), r_lat1 ),
   convert_to_rad( degmin(d_lon1, m_lon1), r_lon2 ),
   convert_to_rad( degmin(d_lat2, m_lat2), r_lat2 ),
   convert_to_rad( degmin(d_lon2, m_lon2), r_lon2 ), 

   haversine( r_lat1, r_lon1, r_lat2, r_lon2, distance ),
   arr_time is convert_to_hrs( distance ) + dept_time. 



/* If number is from 0-9, add a leading zero */
leading_zero( num ) :- 
   num < 10,
      print( 0 ),
      print( num ).

/* Else, do nothing */
leading_zero( num ) :-
   num > 9, 
      print( num ). 

/* 
 * Format time for printing
 */

format_time( hoursonly ) :-
   hours is floor( hoursonly ),
   mins is ( ( hoursonly - hours ) * 60 ),
   temp is floor( mins / 60 ), 
   new_hours is ( temp + hours ),
   new_mins is round( mins - ( temp * 60 ) ),

   leading_zero( new_hours ), print( ':' ), leading_zero( new_mins ).



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
writepath( [[ dept, last_dept_time, last_arr_time ], arr | []] ) :-
   airport( dept, dept_name, _, _ ),
   airport( arr, arr_name, _, _ ),

   write( ' depart ' ),
      write( dept ), write( ' ' ), write( dept_name ), 
      format_time( last_dept_time ), nl, 

   write( ' arrive ' ),
      write( arr ), write( ' ' ), write( arr_name ),
      format_time(last_arr_time), nl,

   !, true. 


/* 
 * Case 2: Flight with multiple stops 
 *
 *
 *
 */
writepath( [[dept, last_dept_time, last_arr_time], 
           [arr, new_dept_time, new_arrival_time] | next_stop]) :- 
   airport( dept, dept_name, _, _ ),
   airport( arr, arr_name, _, _ ),

   write( ' depart ' ),
      write( dept ), write( ' ' ), write( dept_name ),
      format_time( last_dept_time ), nl,

   write( ' arrive ' ),
      write( arr ), write( ' ' ), write( arr_name ),
      format_time( last_arr_time ), nl,

   !, writepath( [[arr, new_dept_time, new_arr_time] | next_stop]).


/* Base Case: Check if successfully reached destination */
listpath( arr, arr, _, [arr], _ ).

/* 
 * Case 1: Find direct path to location
 *    Retrieve flight info from database
 *    Check if node has been visited already
 *    Calculate arrival time to destination 
 *    Recursive call on list path to destination
 *    Error here
 */ 
listpath( dept, arr, stops, [[dept, dept_time, arr_time] | list], t) :-
   flight( dept, arr, t ),
   not( member( arr, stops ) ),
   time_in_hours( t, dept_time ),
   arrival_time( dept, arr, dept_time, arr_time ), 
   arr_time < 24.0,
      listpath( arr, arr, [arr | stops], list, _ ).

/* 
 * Case 2: Find path to location with stopovers
 *   Retrieve flight info from database to stopover 
 *   Check if node has been visited already
 *   Calculate arrival time to destination
 *   Retrieve all paths from database from stopover
 *   Check if there is at least 30 min for a transfer
 *   Recursive call to destination
 */
listpath( dept, arr, stops, [[dept, dept_time, arr_time] | list], t) :-
   flight( dept, stop, t ),
   not( member( stop, stops) ),
   time_in_hrs( t, dept_time ),
   arrival_time( dept, stop, dept_time, arr_time ),
   arr_time < 24.0,
      /* Find flight from layover to next location */
      flight( stop, _, stop_dept_time),
      time_in_hrs( stop_dept_time, new_dept_time),
      transfer_time is new_dept_time - arr_time - 0.5,
      transfer_time >= 0,
         findpath( stop, arr, [stop | stops], list, stop_dept_time). 



/* Case 1: Departing and arriving from same location */
fly( dept, dept ) :- 
   write( 'Error: Duplicate departure and arrival airports.' ), nl,
   !, fail.

/* Case 2: Flight path found */
fly( dept, arr ) :-
   airport( dept, _, _, _ ),
   airport( arr, _ , _, _ ),

   listpath( dept, arr, [dept], list, _ ),
   !, nl,
   writepath( list ),
   true.

/* Case 3: No path from origin to destination */
fly( dept, arr ) :- 
   airport( dept, _, _, _ ), 
   airport( arr, _, _, _ ),
   write( 'Error: No path found.' ), nl,
   !, fail.

/* Case 4: Invalid airport selection */
/*
fly( _, _ ) :-
   write( 'Error: Airport selected does not exist.' ), nl,
   !, fail.
*/
