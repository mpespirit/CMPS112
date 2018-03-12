/* 
 *
 * Airline Reservation Planner
 * Written by: Spenser Estrada and Paula Espiritu
 *
 */

not( X ) :- X, !, fail.
not( _ ).

/* Convert degrees/minutes to radians */
convert_to_rad( degmin(deg, min), result) :-
    result is (deg + (min / 60)) * (pi / 180).

/* Find number of hours based on distance */
convert_to_hrs( miles, hours) :-
   hours is miles / 500.

/* Convert time to hours */
time_in_hrs( time(hours, mins), houronly) :-
   houronly is hours + mins / 60.  

/* 
 * Haversine distance formula 
 * Calulate circle path between 2 points
 */
haversine(lat1, lon1, lat2, lon2, d) :- 
   dlon = lon2 - lon1,
   dlat = lat2 - lat1,
   a is sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2,
   c is 2 * atan2(sqrt(a), sqrt(1 - a)),
   r is 3961, /* Earth's radius in mi */
   d is r * c. 

/*
 * Calculate distance between 2 values
 * arrival time:
 *     retrieve lat/lon for both airports from database
 *     convert values to radians
 *     calculate haversine value
 *        return miles
 *     find elapsed time (mi * hr/mi)
 *     let arrival time be elapsed + departure time
 */
arrival_time(start, end, dept_time, arr_time) :-
   airport( start, _, dm_lat1, dm_lon1 ),
   airport( end, _, dm_lat2, dm_lon2 ),
   convert_to_rad( dm_lat1, r_lat1 ),
   convert_to_rad( dm_lat2, r_lat2 ),
   convert_to_rad( dm_lon1, r_lon1 ),
   convert_to_rad( dm_lon2, r_lon2 ), 
   haversine ( r_lat1, r_lon1, r_lat2, r_lon2, distance ),
   elapsed_time is ( distance / 500), 
   arr_time is elapsed_time + dept_time. 

/* writepath here based on graphpaths.pl 

writeallpaths( Node, Node ) :-
   write( Node ), write( ' is ' ), write( Node ), nl.
writeallpaths( Node, Next ) :-
   listpath( Node, Next, [Node], List ),
   write( Node ), write( ' to ' ), write( Next ), write( ' is ' ),
   writepath( List ),
   fail.

writepath( [] ) :-
   nl.
writepath( [Head|Tail] ) :-
   write( ' ' ), write( Head ), writepath( Tail).

 */

/* 
 * FindPath - Recusive function
 * Parameters: ( Origin, Destination, Stops, [ Origin | List ], time )
 * Base Case : To/From same airport
 * 
 *
 */
listpath( ).
listpath() :- 

.
listpath() :-

.

/*

Helpers from graphpaths.pl

listpath( Node, End, Outlist ) :-
   listpath( Node, End, [Node], Outlist ).

listpath( Node, Node, _, [Node] ).
listpath( Node, End, Tried, [Node|List]) :-
   link( Node, Next),
   not( member( Next, Tried)),
   listpath( Next, End, [Next|Tried], List ).

*/
