/* 
 *
 * Airline Reservation Planner
 * Written by: Spenser Estrada and Paula Espiritu
 *
 */

/* Haversine distance formula */
haversine(lat1, lon1, lat2, lon2, d) :- 
   dlon = lon2 - lon1,
   dlat = lat2 - lat1,
   a is sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2,
   c is 2 * atan2(sqrt(a), sqrt(1-a)),
   r is 3956. /* Earth's radius in mi */
   d is r * c. 
