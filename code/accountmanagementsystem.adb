with Measures; use Measures;

package body AccountManagementSystem with
   SPARK_Mode => On
is

   function Init return AMS is
      AnAMS : AMS;
   begin
      -- Ensure that the AMS starts out with no users,
      -- bar the emergency user, who is not tracked in AMS.Users.
      for UID in UserID loop
         AnAMS.Users(UID) := False;
      end loop;

      -- TODO fix not initialised stuff
      return AnAMS;
   end Init;

   ---------------------------------------------------------------------

   procedure CreateUser(TheAMS : in out AMS; NewUser : out UserID) is
   begin
      null; -- TODO
   end CreateUser;

   ---------------------------------------------------------------------

   procedure SetInsurer(TheAMS : in out AMS;
                        Requester : in UserID;
                        Wearer : in UserID;
                        Insurer : in UserID) is
   begin
      null; -- TODO
   end SetInsurer;

   function ReadInsurer(TheAMS : in AMS;
                        Requester : in UserID;
                        Wearer : in UserID)
                        return UserID is
   begin
      return -1; -- TODO
   end ReadInsurer;

   procedure RemoveInsurer(TheAMS : in out AMS;
                           Requester : in UserID;
                           Wearer : in UserID) is
   begin
      null; -- TODO
   end RemoveInsurer;

   ---------------------------------------------------------------------

   procedure SetFriend(TheAMS : in out AMS;
                       Requester : in UserID;
                       Wearer : in UserID;
                       NewFriend : in UserID) is
   begin
      null; -- TODO
   end SetFriend;

   function ReadFriend(TheAMS : in AMS;
                       Requester : in UserID;
                       Wearer : in UserID)
                       return UserID is
   begin
      return -1; -- TODO
   end ReadFriend;

   procedure RemoveFriend(TheAMS : in out AMS;
                          Requester : in UserID;
                          Wearer : in UserID) is
   begin
      null; -- TODO
   end RemoveFriend;

   ---------------------------------------------------------------------

   procedure AddVitalsPermission(TheAMS : in out AMS;
                                 Wearer : in UserID;
                                 TypeOfContact : in ContactType) is
   begin
      null; -- TODO
   end AddVitalsPermission;

   procedure RemoveVitalsPermission(TheAMS : in out AMS;
                                    Wearer : in UserID;
                                    TypeOfContact : in ContactType) is
   begin
      null; -- TODO
   end RemoveVitalsPermission;

   procedure AddFootstepsPermission(TheAMS : in out AMS;
                                    Wearer : in UserID;
                                    TypeOfContact : in ContactType) is
   begin
      null; -- TODO
   end AddFootstepsPermission;

   procedure RemoveFootstepsPermission(TheAMS : in out AMS;
                                       Wearer : in UserID;
                                       TypeOfContact : in ContactType) is
   begin
      null; -- TODO
   end RemoveFootstepsPermission;

   procedure AddLocationPermission(TheAMS : in out AMS;
                                   Wearer : in UserID;
                                   TypeOfContact : in ContactType) is
   begin
      null; -- TODO
   end AddLocationPermission;

   procedure RemoveLocationPermission(TheAMS : in out AMS;
                                      Wearer : in UserID;
                                      TypeOfContact : in ContactType) is
   begin
      null; -- TODO
   end RemoveLocationPermission;

   ---------------------------------------------------------------------

   procedure UpdateVitals(TheAMS : in out AMS;
                          Wearer : in UserID;
                          NewVitals : in BPM)
   is
   begin
      null; -- TODO
   end UpdateVitals;

   procedure UpdateFootsteps(TheAMS : in out AMS;
                             Wearer : in UserID;
                             NewFootsteps : in Footsteps) is
   begin
      null; -- TODO
   end UpdateFootsteps;

   procedure UpdateLocation(TheAMS : in out AMS;
                            Wearer : in UserID;
                            NewLocation : in GPSLocation) is
   begin
      null; -- TODO
   end UpdateLocation;

   ---------------------------------------------------------------------

   function ReadVitals(TheAMS : in AMS;
                       Requester : in UserID;
                       TargetUser : in UserID)
                       return BPM is
   begin
      return -1; -- TODO
   end ReadVitals;

   function ReadFootsteps(TheAMS : in AMS;
                          Requester : in UserID;
                          TargetUser : in UserID)
                          return Footsteps is
   begin
      return 0; -- TODO
   end ReadFootsteps;

   function ReadLocation(TheAMS : in AMS;
                         Requester : in UserID;
                         TargetUser : in UserID)
                         return GPSLocation is
   begin
      return (Lat => 0.0, Long => 0.0); -- TODO
   end ReadLocation;

   ---------------------------------------------------------------------

   procedure ContactEmergency(TheAMS : in AMS;
                              Wearer : in UserID;
                              Location : in GPSLocation;
                              Vitals : in BPM) is
   begin
      null; -- TODO
   end ContactEmergency;
end AccountManagementSystem;
