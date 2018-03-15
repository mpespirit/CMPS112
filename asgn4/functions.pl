/* 
 *
 * Airline Reservation Planner
 * Written by: Spenser Estrada and Paula Espiritu
 *
 */

not(X) :- X, !, fail.
not(_).

/* Convert degrees and minutes to radians */
convert_to_rad( degmin( Deg, Min ), Rad ) :-
   Rad is ( ( Deg + ( Min / 60 ) ) * ( pi / 180 ) ).
   
/* Convert time to hours */
convert_to_hours_only( time( Hours, Mins ), Hrs_Only ) :-
    Hrs_Only is Hours + Mins / 60.
    
/* 
 * Haversine Distance formula 
 * Calulate circle path between 2 points
 */
haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3961.

/*
 * Calculate Distance between 2 values
 * arrival time:
 *     retrieve lat and lon for both airports from database
 *     convert values to radians
 *     calculate haversine value
 *        return Miles
 *     let arrival time be elapsed + departure time
 */
arrival_time( Start, End, Dept_Time, Arr_Time ) :-
   airport( Start, _, degmin( D_Lat1, M_Lat1 ), 
                      degmin( D_Lon1, M_Lon1 ) ),
   airport(   End, _, degmin( D_Lat2, M_Lat2 ), 
                      degmin( D_Lon2, M_Lon2) ), 
   
   convert_to_rad( degmin( D_Lat1, M_Lat1 ), R_Lat1 ),
   convert_to_rad( degmin( D_Lat2, M_Lat2 ), R_Lat2 ),
   convert_to_rad( degmin( D_Lon1, M_Lon1 ), R_Lon1 ),
   convert_to_rad( degmin( D_Lon2, M_Lon2 ), R_Lon2 ),
   
   haversine_radians( R_Lat1, R_Lon1, R_Lat2, R_Lon2, Dist ),
   Arr_Time is Dist / 500 + Dept_Time. 



/* If number is from 0-9, add a leading zero */
leading_zero( Num ) :-
    Num < 10, 
       print( 0 ), 
       print( Num ).

/* Else, do nothing */
leading_zero( Num ) :-
    Num >= 10, 
       print( Num ).

/* 
 * Print formatted time
 */
print_time( Hrs_Only ) :-
    Hours is floor( Hrs_Only ),
    Mins is ( Hrs_Only - Hours ) * 60 ,
    Temp is ( floor( Mins/ 60) ),
    New_Hours is ( Temp + Hours ),
    New_Mins is round( Mins - ( Temp * 60 ) ),
    
    leading_zero( New_Hours ), print( ':' ), leading_zero( New_Mins ).



/* Base Case: nothing to write */
writepath( [] ) :-
   nl.
   
/* 
 * Case 1: Direct flight to and from airport 
 *    Retrive airport names
 *    Print in format:
 *        Origin_Code      - Origin_Name      - Departure_Time
 *        Destination_Code - Destination_Name - Arrival_Time
 */
writepath( [[Dept, Dept_Time, Arr_Time], Arr | []] ) :-
   airport( Dept, Dept_Name, _, _ ),
   airport( Arr, Arr_Name, _, _ ),
   
   write( '  depart  ' ),
      write( Dept ), write( '  ' ), write( Dept_Name ),
      print_time( Dept_Time ), nl,

   write( '  arrive  ' ),
      write( Arr ), write( '  ' ), write( Arr_Name ),
      print_time( Arr_Time ), nl,
   
   !, true.

/* 
 * Case 2: Flight with multiple Stops 
 */
writepath( [[Dept, Dept_Time, Arr_Time], 
            [Arr, Stop_Dept_Time, Stop_Arr_Time] | Next_Stop]) :-
   airport( Dept, Dept_Name, _, _ ),
   airport( Arr, Arr_Name, _,_ ),
   
   write( '  depart  ' ),
      write( Dept ), write( '  ' ), write( Dept_Name ),
      print_time( Dept_Time ), nl,

   write( '  arrive  ' ),
      write( Arr ), write( '  ' ), write( Arr_Name ),
      print_time( Arr_Time ), nl,
   !, 
      writepath( [[Arr, Stop_Dept_Time, Stop_Arr_Time] | Next_Stop] ).



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
listpath( Dept, Arr, Stops, 
          [[Dept, Dept_Time, Arr_Time] | List], Flight_Time ) :-
   flight( Dept, Arr, Flight_Time),
   not( member( Arr, Stops ) ),
   convert_to_hours_only( Flight_Time, Dept_Time ),
   arrival_time( Dept, Arr, Dept_Time, Arr_Time ),
   Arr_Time < 24.0,
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
listpath( Dept, Arr, Stops, 
          [[Dept, Dept_Time, Arr_Time] | List], Flight_Time ) :-
   flight( Dept, Stop, Flight_Time ),
   not( member( Stop, Stops)),
   convert_to_hours_only( Flight_Time, Dept_Time ),
   arrival_time( Dept, Stop, Dept_Time, Arr_Time ),
   Arr_Time < 24.0,
      flight( Stop, _, Stop_Dept_Time ),
      convert_to_hours_only( Stop_Dept_Time, New_Dept_Time ),
      Transfer_Time is New_Dept_Time - Arr_Time - 0.5,
      Transfer_Time >= 0,
         listpath( Stop, Arr, [Stop | Stops], List, Stop_Dept_Time ).   

   

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
