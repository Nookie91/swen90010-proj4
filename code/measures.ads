-- This package provides some basic declarations for the FatBat system,
package Measures is

   -- The type for user identities
   MAX_USERID : constant Integer := 100;
   type UserID is new Integer range -1 .. MAX_USERID;

   -- The type for heart rate: beats per minute
   MAX_BPM : constant Integer := 300;
   type BPM is new Integer range -1 .. MAX_BPM;

   -- The type for footsteps: footsteps per day
   MAX_FOOTSTEPS : constant Integer := 100000;
   type Footsteps is new Integer range 0 .. MAX_FOOTSTEPS;

   -- The type for GPS location
   type Latitude is new Float range -90.0 .. 90.0;
   type Longitude is new Float range -180.0 .. 180.0;
   type GPSLocation is
      record
	 Lat : Latitude;
	 Long : Longitude;
      end record;

end Measures;
