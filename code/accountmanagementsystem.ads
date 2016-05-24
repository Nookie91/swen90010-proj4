with Measures; use Measures;

package AccountManagementSystem with
   SPARK_Mode => On
is
   -- AccountManagementSystem is a package to manage user accounts
   -- for the FatBat system.
   --
   -- It maintains a collection of users, tracking for each:
   --
   -- * their friend and insurance provider
   -- * their vital signs
   -- * their location
   -- * their footstep count
   --
   -- It also tracks the emergency services user, alerting them when
   -- appropriate.
   --
   -- To ensure correct behaviour, the Init function must be called to
   -- initialise an AMS object.

   ---------------------------------------------------------------------

   -- Users can have contacts of three types:
   -- * emergency services
   -- * friends
   -- * insurers
   --
   -- Note: a user's emergency services contact is always user 0.
   type ContactType is (Friend, Insurer, Emergency);

   -- A type to record which users exist.
   type UserSet is array (UserID) of Boolean;

   -- A type to record relationships between users.
   type UserMap is array (UserID) of UserID;

   -- Types to record data for users.
   type FootstepsMap is array (UserID) of Footsteps;
   type VitalsMap is array (UserID) of BPM;
   type LocationMap is array (UserID) of GPSLocation;

   -- Types to record permissions given by each user to each type of
   -- contact.
   type ContactTypeSet is array (ContactType) of Boolean;
   type PermissionSet is array (UserID) of ContactTypeSet;

   -- Users stores data about which users exist and how they're
   -- related to each other.
   type UsersRecord is
      record
         -- The set of current users, excluding the emergency user.
         Exists : UserSet;

         -- Records each user's friend, insurer
         Friend : UserMap;
         Insurer : UserMap;
      end record;

   -- Data stores the relevant data for all users, including
   -- whether it is initialised.
   type DataRecord is
      record
         Footsteps : FootstepsMap;
         FootstepsInitialised : UserSet;

         Vitals : VitalsMap;
         VitalsInitialised : UserSet;

         Location : LocationMap;
         LocationInitialised : UserSet;
      end record;

   -- Permissions stores the the permissions data for all
   -- users, data types, and contact types.
   type PermissionsRecord is
      record
         Footsteps : PermissionSet;
         Vitals : PermissionSet;
         Location : PermissionSet;
      end record;

   -- An AMS instance holds all the state tracked by the
   -- account management system.
   type AMS is
      record
         Users : UsersRecord;
         Data : DataRecord;
         Permissions : PermissionsRecord;
      end record;

   ---------------------------------------------------------------------

   function Init return AMS
   -- Init initialises an AMS instance, preparing it for future use, including
   -- creating the emergency services user.
   with
      Post =>
         -- The AMS must initially be empty.
         (for all uid in UserID => (
            (uid /= EMERGENCY_SERVICES xor Init'Result.Users.Exists(uid)) and
            -- The Emergency Services user must exist.

            -- Most of these predicates aren't explicit in the Alloy model,
            -- but we can't use
            -- AMS.Users as an array range in Ada, so they other arrays
            -- can't be implicitly 'empty', so we need to
            -- specify the default value of the other arrays.

            -- Initially, users have no friend or insurer, ...
            Init'Result.Users.Friend(uid) = NO_USER and
            Init'Result.Users.Insurer(uid) = NO_USER and

            -- ... they have no initialised data, and...
            not Init'Result.Data.FootstepsInitialised(uid) and
            not Init'Result.Data.VitalsInitialised(uid) and
            not Init'Result.Data.LocationInitialised(uid) and

            -- ... they have given no permissions to anyone, except all
            -- users give their insurer permission to read footsteps
            -- by default.
            (for all ct in ContactType => (
               not Init'Result.Permissions.Vitals(uid)(ct) and
               not Init'Result.Permissions.Location(uid)(ct) and
               not Init'Result.Permissions.Footsteps(uid)(ct)
            ))
         ));

   ---------------------------------------------------------------------

   procedure CreateUser(TheAMS : in out AMS; NewUser : out UserID)
      -- Creates a new user, returning their UserID.
      --
      -- If a new user cannot be created with a unique UserID,
      -- Measures.NO_USER will be returned.
   with
      Pre => (not TheAMS.Users.Exists(NO_USER)),
      Post =>
         -- The new user must not have existed before.
         (not TheAMS'Old.Users.Exists(NewUser)) and

         -- The new user may not be EMERGENCY_SERVICES.
         (NewUser /= EMERGENCY_SERVICES) and

         -- The new user should be registered, iff they were
         -- successfully added.
         (if NewUser = NO_USER then (
            TheAMS = TheAMS'Old
         ) else (
            -- The user should now exist and should allow their insurer
            -- to access their footsteps.
            (TheAMS.Users.Exists(NewUser)) and
            (TheAMS.Permissions.Footsteps(NewUser)(Insurer)) and

            -- The user's other permissions should be unchanged.
            (for all ct in ContactType => (
               if ct /= Insurer then
                  (TheAMS.Permissions.Footsteps(NewUser)(ct) =
                     TheAMS'Old.Permissions.Footsteps(NewUser)(ct))
            )) and

            -- TheAMS should otherwise be unchanged.
            (for all uid in UserID => (
               -- Only the new user's existence and footsteps permissions
               -- may have changed.
               (if (NewUser /= uid) then
                  (TheAMS.Users.Exists(uid) = TheAMS'Old.Users.Exists(uid)) and
                  (TheAMS.Permissions.Footsteps(uid) =
                     TheAMS'Old.Permissions.Footsteps(uid))
               ) and
               TheAMS.Users.Insurer(uid) = TheAMS'Old.Users.Insurer(uid)
            )) and
            (TheAMS.Users.Friend = TheAMS'Old.Users.Friend) and
            (TheAMS.Data = TheAMS'Old.Data) and
            (TheAMS.Permissions.Location = TheAMS'Old.Permissions.Location) and
            (TheAMS.Permissions.Vitals = TheAMS'Old.Permissions.Vitals)
         ));

   ---------------------------------------------------------------------

   procedure SetInsurer(TheAMS : in out AMS;
                        Requester : in UserID;
                        Wearer : in UserID;
                        NewInsurer : in UserID)
      -- Sets the specified NewInsurer as the insurer of the Wearer.
   with
      Pre =>
         -- We can only set the insurer for existing wearers,
         -- and they can only set their own insurer.
         (TheAMS.Users.Exists(Wearer)) and
         (TheAMS.Users.Exists(NewInsurer)) and
         (Wearer = Requester),

      Post =>
         -- The Wearer's insurer must have been set.
         (TheAMS.Users.Insurer(Wearer) = NewInsurer) and
         (for all uid in UserID => (
            -- Only the wearer's insurer may have changed.
            (if (Wearer /= uid) then
               TheAMS.Users.Insurer(uid) = TheAMS'Old.Users.Insurer(uid))
         )) and

         -- TheAMS should otherwise be unchanged.
         (TheAMS.Users.Exists = TheAMS'Old.Users.Exists) and
         (TheAMS.Users.Friend = TheAMS'Old.Users.Friend) and
         (TheAMS.Data = TheAMS'Old.Data) and
         (TheAMS.Permissions = TheAMS'Old.Permissions);

   function ReadInsurer(TheAMS : in AMS;
                        Requester : in UserID;
                        Wearer : in UserID)
                        return UserID
      -- Returns the insurer of the specified Wearer.
   with
      Pre => (TheAMS.Users.Exists(Wearer)) and (Wearer = Requester),
      -- We can only read the insurer of existing wearers,
      -- and they can only read their own insurer.

      Post => (TheAMS.Users.Insurer(Wearer) = ReadInsurer'Result);
      -- Note: TheAMS should implicitly be unchanged since it's
      -- an in parameter.

   procedure RemoveInsurer(TheAMS : in out AMS;
                           Requester : in UserID;
                           Wearer : in UserID)
      -- Sets the specified Wearer to having no insurer.
   with
      Pre =>
         -- We can only remove the insurer of existing wearers
         -- with insurers, and they can only remove their own insurer.
         (TheAMS.Users.Exists(Wearer)) and
         (Wearer = Requester) and
         (TheAMS.Users.Insurer(Wearer) /= NO_USER),

      Post =>
         -- The Wearer's insurer must be removed.
         (TheAMS.Users.Insurer(Wearer) = NO_USER) and
         (for all uid in UserID => (
            -- Only the wearer's insurer may have changed.
            (if (Wearer /= uid) then
               TheAMS.Users.Insurer(uid) = TheAMS'Old.Users.Insurer(uid))
         )) and

         -- TheAMS should otherwise be unchanged.
         (TheAMS.Users.Exists = TheAMS'Old.Users.Exists) and
         (TheAMS.Users.Friend = TheAMS'Old.Users.Friend) and
         (TheAMS.Data = TheAMS'Old.Data) and
         (TheAMS.Permissions = TheAMS'Old.Permissions);

   ---------------------------------------------------------------------

   procedure SetFriend(TheAMS : in out AMS;
                       Requester : in UserID;
                       Wearer : in UserID;
                       NewFriend : in UserID)
      -- Sets the specified Friend as the friend of the Wearer.
   with
      Pre =>
         -- We can only set the friend for existing wearers,
         -- and they can only set their own friend.
         (TheAMS.Users.Exists(Wearer)) and
         (TheAMS.Users.Exists(NewFriend)) and
         (Wearer = Requester),

      Post =>
         -- The Wearer's friend must have been set.
         (TheAMS.Users.Friend(Wearer) = NewFriend) and
         (for all uid in UserID => (
            -- Only the wearer's insurer may have changed.
            (if (Wearer /= uid) then
               TheAMS.Users.Friend(uid) = TheAMS'Old.Users.Friend(uid))
         )) and

         -- TheAMS should otherwise be unchanged.
         (TheAMS.Users.Exists = TheAMS'Old.Users.Exists) and
         (TheAMS.Users.Insurer = TheAMS'Old.Users.Insurer) and
         (TheAMS.Data = TheAMS'Old.Data) and
         (TheAMS.Permissions = TheAMS'Old.Permissions);

   function ReadFriend(TheAMS : in AMS;
                       Requester : in UserID;
                       Wearer : in UserID)
                       return UserID
   -- Returns the friend of the specified Wearer.
   with
      Pre => (TheAMS.Users.Exists(Wearer)) and (Wearer = Requester),
      -- We can only read the friend of existing wearers,
      -- and they can only read their own friend.

      Post => (TheAMS.Users.Friend(Wearer) = ReadFriend'Result);
      -- Note: TheAMS should implicitly be unchanged since it's
      -- an in parameter.

   procedure RemoveFriend(TheAMS : in out AMS;
                          Requester : in UserID;
                          Wearer : in UserID)
   -- Sets the specified Wearer to having no friend.
   with
      Pre =>
         -- We can only remove the friend of existing wearers
         -- with friends, and they can only remove their own friend.
         (TheAMS.Users.Exists(Wearer)) and
         (Wearer = Requester) and
         (TheAMS.Users.Friend(Wearer) /= NO_USER),

      Post =>
         -- The Wearer's friend must be removed.
         (TheAMS.Users.Friend(Wearer) = NO_USER) and
         (for all uid in UserID => (
            -- Only the wearer's friend may have changed.
            (if (Wearer /= uid) then
               TheAMS.Users.Friend(uid) = TheAMS'Old.Users.Friend(uid))
         )) and

         -- TheAMS should otherwise be unchanged.
         (TheAMS.Users.Exists = TheAMS'Old.Users.Exists) and
         (TheAMS.Users.Insurer = TheAMS'Old.Users.Insurer) and
         (TheAMS.Data = TheAMS'Old.Data) and
         (TheAMS.Permissions = TheAMS'Old.Permissions);

   ---------------------------------------------------------------------

   procedure AddVitalsPermission(TheAMS : in out AMS;
                                 Wearer : in UserID;
                                 Contact : in ContactType)
      -- Allows the specified type of contact to access the wearer's vitals.
   with
      Pre => (TheAMS.Users.Exists(Wearer)),
      -- The Wearer must exist.

      Post =>
         -- The specified vitals permission must be present.
         (TheAMS.Permissions.Vitals(Wearer)(Contact)) and

         -- No other vitals permission may have been changed.
         (for all uid in UserID => (
            if uid /= Wearer then
               (TheAMS.Permissions.Vitals(uid) =
                  TheAMS'Old.Permissions.Vitals(uid))
            else (
               for all ct in ContactType => (
                  if ct /= Contact then
                     (TheAMS.Permissions.Vitals(uid)(ct) =
                        TheAMS'Old.Permissions.Vitals(uid)(ct))
               )
            )
         )) and

         -- TheAMS should otherwise be unchanged.
         (TheAMS.Permissions.Location = TheAMS'Old.Permissions.Location) and
         (TheAMS.Permissions.Footsteps = TheAMS'Old.Permissions.Footsteps) and
         (TheAMS.Users = TheAMS'Old.Users) and
         (TheAMS.Data = TheAMS'Old.Data);

   procedure RemoveVitalsPermission(TheAMS : in out AMS;
                                    Wearer : in UserID;
                                    Contact : in ContactType)
      -- Prevents the specified type of contact from accessing the wearer's
      -- vitals.
   with
      Pre => (TheAMS.Users.Exists(Wearer)),
      -- The Wearer must exist.

      Post =>
         -- The specified Vitals permission must not be present.
         (not TheAMS.Permissions.Vitals(Wearer)(Contact)) and

         -- No other Vitals permission may have been changed.
         (for all uid in UserID => (
            if uid /= Wearer then
               (TheAMS.Permissions.Vitals(uid) =
                  TheAMS'Old.Permissions.Vitals(uid))
            else (
               for all ct in ContactType => (
                  if ct /= Contact then
                     (TheAMS.Permissions.Vitals(uid)(ct) =
                        TheAMS'Old.Permissions.Vitals(uid)(ct))
               )
            )
         )) and

         -- TheAMS should otherwise be unchanged.
         (TheAMS.Permissions.Location = TheAMS'Old.Permissions.Location) and
         (TheAMS.Permissions.Footsteps = TheAMS'Old.Permissions.Footsteps) and
         (TheAMS.Users = TheAMS'Old.Users) and
         (TheAMS.Data = TheAMS'Old.Data);

   procedure AddFootstepsPermission(TheAMS : in out AMS;
                                    Wearer : in UserID;
                                    Contact : in ContactType)
      -- Allows the specified type of contact to access the wearer's footstep
      -- data.
   with
      Pre => (TheAMS.Users.Exists(Wearer)),
      -- The Wearer must exist.

      Post =>
         -- The specified footsteps permission must be present.
         (TheAMS.Permissions.Footsteps(Wearer)(Contact)) and

         -- No other footsteps permission may have been changed.
         (for all uid in UserID => (
            if uid /= Wearer then
               (TheAMS.Permissions.Footsteps(uid) =
                  TheAMS'Old.Permissions.Footsteps(uid))
            else (
               for all ct in ContactType => (
                  if ct /= Contact then
                     (TheAMS.Permissions.Footsteps(uid)(ct) =
                        TheAMS'Old.Permissions.Footsteps(uid)(ct))
               )
            )
         )) and

         -- TheAMS should otherwise be unchanged.
         (TheAMS.Permissions.Location = TheAMS'Old.Permissions.Location) and
         (TheAMS.Permissions.Vitals = TheAMS'Old.Permissions.Vitals) and
         (TheAMS.Users = TheAMS'Old.Users) and
         (TheAMS.Data = TheAMS'Old.Data);

   procedure RemoveFootstepsPermission(TheAMS : in out AMS;
                                       Wearer : in UserID;
                                       Contact : in ContactType)
      -- Prevents the specified type of contact from accessing the wearer's
      -- footstep data.
   with
      Pre => (TheAMS.Users.Exists(Wearer)) and (Contact /= Insurer),
      -- The Wearer must exist and the insurer must always be able to
      -- read footsteps.

      Post =>
         -- The specified footsteps permission must not be present.
         (not TheAMS.Permissions.Footsteps(Wearer)(Contact)) and

         -- No other footsteps permission may have been changed.
         (for all uid in UserID => (
            if uid /= Wearer then
               (TheAMS.Permissions.Footsteps(uid) =
                  TheAMS'Old.Permissions.Footsteps(uid))
            else (
               for all ct in ContactType => (
                  if ct /= Contact then
                     (TheAMS.Permissions.Footsteps(uid)(ct) =
                        TheAMS'Old.Permissions.Footsteps(uid)(ct))
               )
            )
         )) and

         -- TheAMS should otherwise be unchanged.
         (TheAMS.Permissions.Location = TheAMS'Old.Permissions.Location) and
         (TheAMS.Permissions.Vitals = TheAMS'Old.Permissions.Vitals) and
         (TheAMS.Users = TheAMS'Old.Users) and
         (TheAMS.Data = TheAMS'Old.Data);

   procedure AddLocationPermission(TheAMS : in out AMS;
                                   Wearer : in UserID;
                                   Contact : in ContactType)
      -- Allows the specified type of contact to access the wearer's location
      -- data.
   with
      Pre => (TheAMS.Users.Exists(Wearer)),
      -- The Wearer must exist.

      Post =>
         -- The specified location permission must be present.
         (TheAMS.Permissions.Location(Wearer)(Contact)) and

         -- No other location permission may have been changed.
         (for all uid in UserID => (
            if uid /= Wearer then
               (TheAMS.Permissions.Location(uid) =
                  TheAMS'Old.Permissions.Location(uid))
            else (
               for all ct in ContactType => (
                  if ct /= Contact then
                     (TheAMS.Permissions.Location(uid)(ct) =
                        TheAMS'Old.Permissions.Location(uid)(ct))
               )
            )
         )) and

         -- TheAMS should otherwise be unchanged.
         (TheAMS.Permissions.Footsteps = TheAMS'Old.Permissions.Footsteps) and
         (TheAMS.Permissions.Vitals = TheAMS'Old.Permissions.Vitals) and
         (TheAMS.Users = TheAMS'Old.Users) and
         (TheAMS.Data = TheAMS'Old.Data);

   procedure RemoveLocationPermission(TheAMS : in out AMS;
                                      Wearer : in UserID;
                                      Contact : in ContactType)
      -- Prevents the specified type of contact from accessing the wearer's
      -- location data.
   with
      Pre => (TheAMS.Users.Exists(Wearer)),
      -- The Wearer must exist.

      Post =>
         -- The specified location permission must not be present.
         (not TheAMS.Permissions.Location(Wearer)(Contact)) and

         -- No other location permission may have been changed.
         (for all uid in UserID => (
            if uid /= Wearer then
               (TheAMS.Permissions.Location(uid) =
                  TheAMS'Old.Permissions.Location(uid))
            else (
               for all ct in ContactType => (
                  if ct /= Contact then
                     (TheAMS.Permissions.Location(uid)(ct) =
                        TheAMS'Old.Permissions.Location(uid)(ct))
               )
            )
         )) and

         -- TheAMS should otherwise be unchanged.
         (TheAMS.Permissions.Footsteps = TheAMS'Old.Permissions.Footsteps) and
         (TheAMS.Permissions.Vitals = TheAMS'Old.Permissions.Vitals) and
         (TheAMS.Users = TheAMS'Old.Users) and
         (TheAMS.Data = TheAMS'Old.Data);

   ---------------------------------------------------------------------

   procedure UpdateVitals(TheAMS : in out AMS;
                          Wearer : in UserID;
                          NewVitals : in BPM)
      -- Updates the vital statistics of the specified user.
   with
      Pre => (TheAMS.Users.Exists(Wearer)),
      -- The Wearer must exist.

      Post =>
         -- The vitals value must be stored.
         (TheAMS.Data.VitalsInitialised(Wearer)) and 
         (TheAMS.Data.Vitals(Wearer) = NewVitals) and

         -- No other vitals data may have been changed.
         (for all uid in UserID => (
            if uid /= Wearer then
               (TheAMS.Data.Vitals(uid) = TheAMS'Old.Data.Vitals(uid))
         )) and

         -- TheAMS should otherwise be unchanged.
         (TheAMS.Data.Footsteps = TheAMS'Old.Data.Footsteps) and
         (TheAMS.Data.Location = TheAMS'Old.Data.Location) and
         (TheAMS.Users = TheAMS'Old.Users) and
         (TheAMS.Permissions = TheAMS'Old.Permissions);

   procedure UpdateFootsteps(TheAMS : in out AMS;
                             Wearer : in UserID;
                             NewFootsteps : in Footsteps)
   -- Updates the footstep count of the specified user.
   with
      Pre => (TheAMS.Users.Exists(Wearer)),
      -- The Wearer must exist.

      Post =>
         -- The footsteps value must be stored.
         (TheAMS.Data.FootstepsInitialised(Wearer)) and
         (TheAMS.Data.Footsteps(Wearer) = NewFootsteps) and

         -- No other footsteps data may have been changed.
         (for all uid in UserID => (
            if uid /= Wearer then
               (TheAMS.Data.Footsteps(uid) = TheAMS'Old.Data.Footsteps(uid))
         )) and

         -- TheAMS should otherwise be unchanged.
         (TheAMS.Data.Location = TheAMS'Old.Data.Location) and
         (TheAMS.Data.Vitals = TheAMS'Old.Data.Vitals) and
         (TheAMS.Users = TheAMS'Old.Users) and
         (TheAMS.Permissions = TheAMS'Old.Permissions);

   procedure UpdateLocation(TheAMS : in out AMS;
                            Wearer : in UserID;
                            NewLocation : in GPSLocation)
   -- Updates the location of the specified user.
   with
      Pre => (TheAMS.Users.Exists(Wearer)),
      -- The Wearer must exist.

      Post =>
         -- The location value must be stored.
         (TheAMS.Data.LocationInitialised(Wearer)) and
         (TheAMS.Data.Location(Wearer) = NewLocation) and

         -- No other location data may have been changed.
         (for all uid in UserID => (
            if uid /= Wearer then
               (TheAMS.Data.Location(uid) = TheAMS'Old.Data.Location(uid))
         )) and

         -- TheAMS should otherwise be unchanged.
         (TheAMS.Data.Footsteps = TheAMS'Old.Data.Footsteps) and
         (TheAMS.Data.Vitals = TheAMS'Old.Data.Vitals) and
         (TheAMS.Users = TheAMS'Old.Users) and
         (TheAMS.Permissions = TheAMS'Old.Permissions);

   ---------------------------------------------------------------------
   -- Each of these functions interprets conflicting requirements with the
   -- following priority:
   --
   -- R3.1 > R3.9 > R3.6 > (R3.8 and R3.7).
   --
   -- Therefore:
   -- * a user is always allowed to access their own data,
   -- * a reader who is not related to a user in any way is not allowed to
   --   access any of that user's data,
   -- * an insurance provider of a user is allowed to access that user's
   --   footsteps no matter what,
   -- * a user is allowed to choose any permission scheme they wish for
   --   reading their data, subject to the above constraints.

   function ReadVitals(TheAMS : in AMS;
                       Requester : in UserID;
                       Wearer : in UserID)
                       return BPM
      -- Reads the vital statistics of the specified target user,
      -- if the requester has permission to read that statistic.
   with
      Pre =>
         -- We can only read the vitals of existing wearers,
         (TheAMS.Data.VitalsInitialised(Wearer)) and
         (TheAMS.Users.Exists(Wearer)) and
         ((
            TheAMS.Users.Insurer(Wearer) = Requester and
            TheAMS.Permissions.Vitals(Wearer)(Insurer)
         ) or (
            TheAMS.Users.Friend(Wearer) = Requester and
            TheAMS.Permissions.Vitals(Wearer)(Friend)
         ) or (
            EMERGENCY_SERVICES = Requester and
            TheAMS.Permissions.Vitals(Wearer)(Emergency)
         )),

      Post => (TheAMS.Data.Vitals(Wearer) = ReadVitals'Result);
      -- Note: TheAMS should implicitly be unchanged since it's
      -- an in parameter.

   function ReadFootsteps(TheAMS : in AMS;
                          Requester : in UserID;
                          Wearer : in UserID)
                          return Footsteps
      -- Reads the footstep count of the specified target user,
      -- if the requester has permission to read that statistic.
      --
      -- Assumes the intended interpretation of requirement R3.6 is
      -- "The system must allow a user with (permission as a user's insurance
      -- provider) to read that user's footsteps."
      -- This means a user's insurance provider is **always** allowed to read
      -- their footsteps, regardless of the user's permission preferences.
   with
      Pre =>
         -- We can only read the footsteps of existing wearers,
         (TheAMS.Data.FootstepsInitialised(Wearer)) and
         (TheAMS.Users.Exists(Wearer)) and
         ((
            TheAMS.Users.Insurer(Wearer) = Requester
         ) or (
            TheAMS.Users.Friend(Wearer) = Requester and
            TheAMS.Permissions.Footsteps(Wearer)(Friend)
         ) or (
            EMERGENCY_SERVICES = Requester and
            TheAMS.Permissions.Footsteps(Wearer)(Emergency)
         )),

      Post => (TheAMS.Data.Footsteps(Wearer) = ReadFootsteps'Result);
      -- Note: TheAMS should implicitly be unchanged since it's
      -- an in parameter.

   function ReadLocation(TheAMS : in AMS;
                         Requester : in UserID;
                         Wearer : in UserID)
                         return GPSLocation
      -- Reads the location of the specified target user,
      -- if the requester has permission to read that statistic.
   with
      Pre =>
         -- We can only read the location of existing wearers,
         (TheAMS.Data.LocationInitialised(Wearer)) and
         (TheAMS.Users.Exists(Wearer)) and
         ((
            TheAMS.Users.Insurer(Wearer) = Requester and
            TheAMS.Permissions.Location(Wearer)(Insurer)
         ) or (
            TheAMS.Users.Friend(Wearer) = Requester and
            TheAMS.Permissions.Location(Wearer)(Friend)
         ) or (
            EMERGENCY_SERVICES = Requester and
            TheAMS.Permissions.Location(Wearer)(Emergency)
         )),

      Post => (TheAMS.Data.Location(Wearer) = ReadLocation'Result);
      -- Note: TheAMS should implicitly be unchanged since it's
      -- an in parameter.

   ---------------------------------------------------------------------

   procedure HandleCardiacArrest(TheAMS : in out AMS;
                                 Wearer : in UserID;
                                 Location : in GPSLocation;
                                 Vitals : in BPM)
   -- Checks if the Wearer has given the emergency services user permission
   -- to read their vitals.
   -- If and only if so, stores the vitals and location data within
   -- this user, and sends a message via the Emergency package.
   with
      Pre => (TheAMS.Users.Exists(Wearer)),
      -- The Wearer must exist.

      Post =>
         -- Emergency services must be contacted and the location
         -- and vitals must be saved, all if and only if the wearer
         -- has given Emergency services permission to read their vitals.
         -- Note: I'm not sure how to express the calling of emergency
         -- services in a SPARK contract, so I'm leaving it out.
         (if TheAMS.Permissions.Vitals(Wearer)(Emergency) then (
            (TheAMS.Data.Location =
               TheAMS'Old.Data.Location'Update(Wearer => Location)) and
            (TheAMS.Data.Vitals =
               TheAMS'Old.Data.Vitals'Update(Wearer => Vitals)) and
            (TheAMS.Data.Footsteps = TheAMS'Old.Data.Footsteps)
         ) else (
            TheAMS.Data = TheAMS'Old.Data
         )) and

         -- No other changes may take place.
         (TheAMS.Users = TheAMS'Old.Users) and
         (TheAMS.Permissions = TheAMS'Old.Permissions);

end AccountManagementSystem;
