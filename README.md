# 6e

Common lisp library for converting between 6-element Keplerian orbital
element sets<sup>[1](#note1)</sup> and Cartesian position and velocity
vectors.

Two reader macros are defined: one to convert cartesian vectors to
keplerian elements:

{c**X Y Z dX dY dZ**}

where *X Y Z* represent the position in space (meters) and *dX dY dZ*
represent the velocity (m/s) along each axis.  This returns a vector
of 6 elements: semimajor axis, eccentricity, inclination, longitude of
ascending node, argument of periapsis, and mean anomaly at epoch.

The other reader macro converts keplerian elements to cartesion vectors:

{k**Mu alpha e i omega w M0 dT**}

where:  
*Mu* = mass of the planet (kg) times G (3.986044d14 for Earth)  
*alpha* = semimajor axis  
*e* = eccentricity  
*i* = inclination  
*omega* = longitude of ascending node  
*w* = argument of periapsis  
*M0* = mean anomaly  
*dT* = time from epoch (seconds)  

This returns a 6-element list (*X Y Z dX dY dZ*) containing the
cartesian position and velocity vectors.

The test cases are drawn from the book *Statistical Orbit
Determination* by Tapley et al., ISBN 978-0126836301.

\[<a name="note1">1</a>\]: <https://en.wikipedia.org/wiki/Orbital_elements#Keplerian_elements>
