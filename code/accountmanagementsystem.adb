with Measures; use Measures;
with Emergency; use Emergency;

package body AccountManagementSystem with
   SPARK_Mode => On
is

   function Init return AMS is
      AnAMS : AMS;
   begin
      -- Ensure that the AMS starts out with no users,
      -- bar the emergency user, who is not tracked in AMS.Users.

     AnAMS.Users.Exists := (others => False);
     AnAMS.Users.Exists(EMERGENCY_SERVICES) := True;
     AnAMS.Users.Friend := (others => NO_USER);
     AnAMS.Users.Insurer := (others => NO_USER);

     AnAMS.Permissions.Vitals := (others => (others => False));
     AnAMS.Permissions.Location := (others => (others => False));
     AnAMS.Permissions.Footsteps := (others => (others => False));

     AnAMS.Data.Vitals := (others => -1);
     AnAMS.Data.VitalsInitialised := (others => False);
     AnAMS.Data.Location := (others => (Lat => 0.0, Long => 0.0));
     AnAMS.Data.LocationInitialised := (others => False);
     AnAMS.Data.Footsteps := (others => 0);
     AnAMS.Data.FootstepsInitialised := (others => False);


      return AnAMS;
   end Init;

   ---------------------------------------------------------------------

   procedure CreateUser(TheAMS : in out AMS; NewUser : out UserID) is
   begin
      -- Find an unused UserID, if one exists.
      NewUser := NO_USER;
      for UID in UserID loop
         if not TheAMS.Users.Exists(UID) and UID /= EMERGENCY_SERVICES then
            NewUser := UID;
         end if;
         exit when NewUser /= NO_USER;
         pragma Loop_Invariant (not TheAMS.Users.Exists(NewUser) and
                                NewUser /= EMERGENCY_SERVICES);
      end loop;

      -- If we found an unused UserID, mark that user as existing.
      if NewUser /= NO_USER then
         TheAMS.Users.Exists(NewUser) := True;
         TheAMS.Permissions.Footsteps(NewUser)(Insurer) := True;
      end if;
   end CreateUser;

   ---------------------------------------------------------------------

   procedure SetInsurer(TheAMS : in out AMS;
                        Requester : in UserID;
                        Wearer : in UserID;
                        NewInsurer : in UserID) is
   begin
      TheAMS.Users.Insurer(Wearer) := NewInsurer;
   end SetInsurer;

   function ReadInsurer(TheAMS : in AMS;
                        Requester : in UserID;
                        Wearer : in UserID)
                        return UserID is
   begin
      return TheAMS.Users.Insurer(Wearer);
   end ReadInsurer;

   procedure RemoveInsurer(TheAMS : in out AMS;
                           Requester : in UserID;
                           Wearer : in UserID) is
   begin
       TheAMS.Users.Insurer(Wearer) := NO_USER;
   end RemoveInsurer;

   ---------------------------------------------------------------------

   procedure SetFriend(TheAMS : in out AMS;
                       Requester : in UserID;
                       Wearer : in UserID;
                       NewFriend : in UserID) is
   begin
      TheAMS.Users.Friend(Wearer) := NewFriend;
   end SetFriend;

   function ReadFriend(TheAMS : in AMS;
                       Requester : in UserID;
                       Wearer : in UserID)
                       return UserID is
   begin
       return TheAMS.Users.Friend(Wearer);
   end ReadFriend;

   procedure RemoveFriend(TheAMS : in out AMS;
                          Requester : in UserID;
                          Wearer : in UserID) is
   begin
       TheAMS.Users.Friend(Wearer) := NO_USER;
   end RemoveFriend;

   ---------------------------------------------------------------------

   procedure AddVitalsPermission(TheAMS : in out AMS;
                                 Wearer : in UserID;
                                 Contact : in ContactType) is
   begin
        TheAMS.Permissions.Vitals(Wearer)(Contact) := True;
   end AddVitalsPermission;

   procedure RemoveVitalsPermission(TheAMS : in out AMS;
                                    Wearer : in UserID;
                                    Contact : in ContactType) is
   begin
       TheAMS.Permissions.Vitals(Wearer)(Contact) := False;
   end RemoveVitalsPermission;

   procedure AddFootstepsPermission(TheAMS : in out AMS;
                                    Wearer : in UserID;
                                    Contact : in ContactType) is
   begin
      TheAMS.Permissions.Footsteps(Wearer)(Contact) := True;
   end AddFootstepsPermission;

   procedure RemoveFootstepsPermission(TheAMS : in out AMS;
                                       Wearer : in UserID;
                                       Contact : in ContactType) is
   begin
      TheAMS.Permissions.Footsteps(Wearer)(Contact) := False;
   end RemoveFootstepsPermission;

   procedure AddLocationPermission(TheAMS : in out AMS;
                                   Wearer : in UserID;
                                   Contact : in ContactType) is
   begin
      TheAMS.Permissions.Location(Wearer)(Contact) := True;
   end AddLocationPermission;

   procedure RemoveLocationPermission(TheAMS : in out AMS;
                                      Wearer : in UserID;
                                      Contact : in ContactType) is
   begin
      TheAMS.Permissions.Location(Wearer)(Contact) := False;
   end RemoveLocationPermission;

   ---------------------------------------------------------------------

   procedure UpdateVitals(TheAMS : in out AMS;
                          Wearer : in UserID;
                          NewVitals : in BPM)
   is
   begin
      TheAMS.Data.Vitals(Wearer) := NewVitals;
      TheAMS.Data.VitalsInitialised(Wearer) := True;
   end UpdateVitals;

   procedure UpdateFootsteps(TheAMS : in out AMS;
                             Wearer : in UserID;
                             NewFootsteps : in Footsteps) is
   begin
      TheAMS.Data.Footsteps(Wearer) := NewFootsteps;
      TheAMS.Data.FootstepsInitialised(Wearer) := True;
   end UpdateFootsteps;

   procedure UpdateLocation(TheAMS : in out AMS;
                            Wearer : in UserID;
                            NewLocation : in GPSLocation) is
   begin
      TheAMS.Data.Location(Wearer) := NewLocation;
      TheAMS.Data.LocationInitialised(Wearer) := True;
   end UpdateLocation;

   ---------------------------------------------------------------------

   function ReadVitals(TheAMS : in AMS;
                       Requester : in UserID;
                       Wearer : in UserID)
                       return BPM is
   begin
      return TheAMS.Data.Vitals(Wearer);
   end ReadVitals;

   function ReadFootsteps(TheAMS : in AMS;
                          Requester : in UserID;
                          Wearer : in UserID)
                          return Footsteps is
   begin
      return TheAMS.Data.Footsteps(Wearer);
   end ReadFootsteps;

   function ReadLocation(TheAMS : in AMS;
                         Requester : in UserID;
                         Wearer : in UserID)
                         return GPSLocation is
   begin
      return TheAMS.Data.Location(Wearer);
   end ReadLocation;

   ---------------------------------------------------------------------

   procedure HandleCardiacArrest(TheAMS : in out AMS;
                                 Wearer : in UserID;
                                 Location : in GPSLocation;
                                 Vitals : in BPM) is
   begin
        if TheAMS.Permissions.Vitals(Wearer)(Emergency) then
            TheAMS.Data.Vitals(Wearer) := Vitals;
            TheAMS.Data.Location(Wearer) := Location;
            ContactEmergency(Wearer, Vitals, Location);
        end if;
   end HandleCardiacArrest;
end AccountManagementSystem;
