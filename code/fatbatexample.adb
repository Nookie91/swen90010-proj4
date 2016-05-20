with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;

with Users;
with AccountManagementSystem;
with Measures; use Measures;

package body FatBatExample is

   package AMS renames AccountManagementSystem;

   procedure Run is

      Emergency_Services : constant UserID := Users.EMERGENCY_SERVICES_ID;
      Wearer : UserID;
      Friend : UserID;
      Insurer : UserID;
      Insurer_And_Friend : UserID;

      VITALS_ONE : constant BPM := 5;
      VITALS_TWO : constant BPM := 120;

      LOCATION_ONE : constant GPSLocation :=
         (Long => Longitude(-10), Lat => Latitude(30));
      LOCATION_TWO : constant GPSLocation :=
         (Long => Longitude(70), Lat => Latitude(-60));

      FOOTSTEPS_ONE : constant Footsteps := 5;
      FOOTSTEPS_TWO : constant Footsteps := 120;

   begin
      Put("Initialising... ");
      AMS.Init;
      Put_Line("Done!");

      ------------------------------------------------------------

      Put("Creating users... ");

      -- Make four users; they are all unique.
      Wearer := AMS.CreateUser;
      Assert(Wearer /= Emergency_Services);

      Friend := AMS.CreateUser;
      Assert(Friend /= Emergency_Services);
      Assert(Friend /= Wearer);

      Insurer := AMS.CreateUser;
      Assert(Insurer /= Emergency_Services);
      Assert(Insurer /= Wearer);
      Assert(Insurer /= Friend);

      Insurer_And_Friend := AMS.CreateUser;
      Assert(Insurer_And_Friend /= Emergency_Services);
      Assert(Insurer_And_Friend /= Wearer);
      Assert(Insurer_And_Friend /= Friend);
      Assert(Insurer_And_Friend /= Insurer);

      Put_Line("Done!");

      ------------------------------------------------------------

      Put("Setting/Checking Vitals... ");

      -- Set vitals. The user can always view them.
      AMS.UpdateVitals(Wearer, VITALS_ONE);
      Assert(VITALS_ONE = AMS.ReadVitals(Wearer, Wearer));

      Put_Line("Setting/Checking Friend...");

      -- Make the friend able to view vitals.
      AMS.SetFriend(Wearer, Friend);
      Assert(Friend = AMS.ReadFriend(Wearer));
      AMS.UpdateVitalsPermissions(Wearer, Friend, true);

      Put("    ...Changing Vitals... ");

      -- Change vitals.
      AMS.UpdateVitals(Wearer, VITALS_TWO);
      Assert(VITALS_TWO = AMS.ReadVitals(Friend, Wearer));

      Put_Line("Done!");

      ------------------------------------------------------------
      Put("Setting/Checking Insurer... ");

      -- Set insurer.
      AMS.SetInsurer(Wearer, Insurer);
      Assert(Insurer = AMS.ReadInsurer(Wearer));

      Put_Line("Setting/Checking Footsteps...");

      -- Set footsteps; the wearer's insurer can always read them,
      -- even if they don't have explicit permission.
      AMS.UpdateFootsteps(Wearer, FOOTSTEPS_ONE);
      AMS.UpdateFootstepsPermissions(Wearer, Insurer, false);
      Assert(FOOTSTEPS_ONE = AMS.ReadFootsteps(Insurer, Wearer));

      -- Make emergency services able to view footsteps.
      AMS.UpdateFootstepsPermissions(Wearer, EMERGENCY_SERVICES, true);

      Put("    ...Changing Footsteps... ");

      -- Change footsteps.
      AMS.UpdateFootsteps(Wearer, FOOTSTEPS_TWO);
      Assert(FOOTSTEPS_TWO = AMS.ReadFootsteps(EMERGENCY_SERVICES, Wearer));

      Put_Line("Done!");

      ------------------------------------------------------------
      Put("Changing Friend & Insurer... ");

      -- Change insurer and friend.
      AMS.SetInsurer(Wearer, Insurer_And_Friend);
      AMS.SetFriend(Wearer, Insurer_And_Friend);
      Assert(Insurer_And_Friend = AMS.ReadInsurer(Wearer));
      Assert(Insurer_And_Friend = AMS.ReadFriend(Wearer));

      -- Make insurer/friend able to view location.
      AMS.UpdateLocationPermissions(Wearer, Insurer_And_Friend, true);

      Put_Line("Setting/Checking Location...");

      -- Set location.
      AMS.UpdateLocation(Wearer, LOCATION_ONE);
      Assert(LOCATION_ONE = AMS.ReadLocation(Insurer_And_Friend, Wearer));

      -- Remove Insurer/Friend as a location viewer.
      AMS.UpdateLocationPermissions(Wearer, Insurer_And_Friend, false);

      Put("    ...Changing Location... ");

      -- Change location.
      AMS.UpdateLocation(Wearer, LOCATION_TWO);

      Put("Handling Error... ");

      -- Reading data without permission raises an error.
      declare
         ignored : GPSLocation;
      begin
         ignored := AMS.ReadLocation(Insurer_And_Friend, Wearer);
         raise Assertion_Error;
      exception
         when Assertion_Error => raise;
         when others => null;
      end;

      -- Restore permissions, continue as before.
      AMS.UpdateLocationPermissions(Wearer, Insurer_And_Friend, true);
      Assert(
         LOCATION_TWO = AMS.ReadLocation(Insurer_And_Friend, Wearer));

      Put_Line("Done!");

      ------------------------------------------------------------

      Put_Line("Contacting Emergency Services...");

      Put_Line("    - Without vitals permissions - nothing happens:");

      -- Make an emergency call get ignored without vitals permission.
      AMS.UpdateVitalsPermissions(Wearer, EMERGENCY_SERVICES, false);
      AMS.ContactEmergency(Wearer, LOCATION_ONE, VITALS_ONE);
      Assert(
         VITALS_TWO = AMS.ReadVitals(Wearer, Wearer));
      Assert(
         LOCATION_TWO = AMS.ReadLocation(Wearer, Wearer));

      Put_Line("    - With vitals permissions - message sent, data stored:");

      -- Make an emergency call go through with vitals permission.
      AMS.UpdateVitalsPermissions(Wearer, EMERGENCY_SERVICES, true);
      AMS.ContactEmergency(Wearer, LOCATION_ONE, VITALS_ONE);
      Assert(
         VITALS_ONE = AMS.ReadVitals(Wearer, Wearer));
      Assert(
         LOCATION_ONE = AMS.ReadLocation(Wearer, Wearer));
   end Run;

end FatBatExample;
