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
   --
   -- All functions and procedures assume that all UserIDs passed in as
   -- arguments have previously been returned by CreateUser, with the
   -- exception of Users.EMERGENCY_SERVICES_ID, which may always be passed in.
   -- If this assumption is violated, the result is unspecified.

   ---------------------------------------------------------------------

   -- Users can have contacts of two types:
   -- * friends
   -- * insurers
   --
   -- Emergency services do not count as a contact type,
   -- since the emergency services user is the same for everyone.
   type ContactType is (Friend, Insurer);

   -- A type to record which users exist.
   type UserSet is array (UserID) of Boolean;

   -- A type to record relationships between users.
   type UserMap is array (UserID) of UserID;

   -- Types to record data for users.
   type FootstepsMap is array (UserID) of Footsteps;
   type VitalsMap is array (UserID) of BPM;
   type LocationsMap is array (UserID) of GPSLocation;

   -- Types to record permissions given by each user to each type of
   -- contact.
   type ContactTypeSet is array (ContactType) of Boolean;
   type PermissionSet is array (UserID) of ContactTypeSet;

   -- An AMS instance holds all the state tracked by the
   -- account management system.
   type AMS is
      record
         -- The set of current users, excluding the emergency user.
         Users : UserSet;

         -- Records each user's friend, insurer
         Friends : UserMap;
         Insurers : UserMap;

         -- Records each user's relevant data
         Footsteps : FootstepsMap;
         Vitals : VitalsMap;
         Locations : LocationsMap;

         -- Records for each user the permissions given for each
         -- data type and role. Many to many.
         footstepsPermissions : PermissionSet;
         vitalsPermissions : PermissionSet;
         locationPermissions : PermissionSet;
      end record;

   ---------------------------------------------------------------------

   function Init return AMS with
      Post => (for all uid in UserID => not Init'Result.Users(uid));

   -- Init initialises an AMS instance, preparing it for future use, including
   -- creating the emergency services user.

   ---------------------------------------------------------------------

   procedure CreateUser(TheAMS : in out AMS; NewUser : out UserID);
   -- Creates a new user, returning their UserID.
   --
   -- If a new user cannot be created with a unique UserID,
   -- Measures.NO_USER will be returned.

   ---------------------------------------------------------------------

   procedure SetInsurer(TheAMS : in out AMS;
                        Requester : in UserID;
                        Wearer : in UserID;
                        Insurer : in UserID);
   -- Sets the specified Insurer as the insurer of the Wearer.

   function ReadInsurer(TheAMS : in AMS;
                        Requester : in UserID;
                        Wearer : in UserID)
                        return UserID;
   -- Returns the insurer of the specified Wearer.

   procedure RemoveInsurer(TheAMS : in out AMS;
                           Requester : in UserID;
                           Wearer : in UserID);
   -- Sets the specified Wearer to having no insurer.

   ---------------------------------------------------------------------

   procedure SetFriend(TheAMS : in out AMS;
                       Requester : in UserID;
                       Wearer : in UserID;
                       NewFriend : in UserID);
   -- Sets the specified Friend as the friend of the Wearer.

   function ReadFriend(TheAMS : in AMS;
                       Requester : in UserID;
                       Wearer : in UserID)
                       return UserID;
   -- Returns the friend of the specified Wearer.

   procedure RemoveFriend(TheAMS : in out AMS;
                          Requester : in UserID;
                          Wearer : in UserID);
   -- Sets the specified Wearer to having no friend.

   ---------------------------------------------------------------------

   procedure AddVitalsPermission(TheAMS : in out AMS;
                                 Wearer : in UserID;
                                 TypeOfContact : in ContactType);
   -- Allows the specified type of contact to access the wearer's vitals.

   procedure RemoveVitalsPermission(TheAMS : in out AMS;
                                    Wearer : in UserID;
                                    TypeOfContact : in ContactType);
   -- Prevents the specified type of contact from accessing the wearer's
   -- vitals.

   procedure AddFootstepsPermission(TheAMS : in out AMS;
                                    Wearer : in UserID;
                                    TypeOfContact : in ContactType);
   -- Allows the specified type of contact to access the wearer's footstep
   -- data.

   procedure RemoveFootstepsPermission(TheAMS : in out AMS;
                                       Wearer : in UserID;
                                       TypeOfContact : in ContactType);
   -- Prevents the specified type of contact from accessing the wearer's
   -- footstep data.

   procedure AddLocationPermission(TheAMS : in out AMS;
                                   Wearer : in UserID;
                                   TypeOfContact : in ContactType);
   -- Allows the specified type of contact to access the wearer's location
   -- data.

   procedure RemoveLocationPermission(TheAMS : in out AMS;
                                      Wearer : in UserID;
                                      TypeOfContact : in ContactType);
   -- Prevents the specified type of contact from accessing the wearer's
   -- location data.

   ---------------------------------------------------------------------

   procedure UpdateVitals(TheAMS : in out AMS;
                          Wearer : in UserID;
                          NewVitals : in BPM);
   -- Updates the vital statistics of the specified user.

   procedure UpdateFootsteps(TheAMS : in out AMS;
                             Wearer : in UserID;
                             NewFootsteps : in Footsteps);
   -- Updates the footstep count of the specified user.

   procedure UpdateLocation(TheAMS : in out AMS;
                            Wearer : in UserID;
                            NewLocation : in GPSLocation);
   -- Updates the location of the specified user.

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
                       TargetUser : in UserID)
                       return BPM;
   -- Reads the vital statistics of the specified target user,
   -- if the requester has permission to read that statistic.

   function ReadFootsteps(TheAMS : in AMS;
                          Requester : in UserID;
                          TargetUser : in UserID)
                          return Footsteps;
   -- Reads the footstep count of the specified target user,
   -- if the requester has permission to read that statistic.
   --
   -- Assumes the intended interpretation of requirement R3.6 is
   -- "The system must allow a user with (permission as a user's insurance
   -- provider) to read that user's footsteps."
   -- This means a user's insurance provider is **always** allowed to read
   -- their footsteps, regardless of the user's permission preferences.

   function ReadLocation(TheAMS : in AMS;
                         Requester : in UserID;
                         TargetUser : in UserID)
                         return GPSLocation;
   -- Reads the location of the specified target user,
   -- if the requester has permission to read that statistic.

   ---------------------------------------------------------------------

   procedure ContactEmergency(TheAMS : in AMS;
                              Wearer : in UserID;
                              Location : in GPSLocation;
                              Vitals : in BPM);
   -- Checks if the Wearer has given the emergency services user permission
   -- to read their vitals.
   -- If and only if so, stores the vitals and location data within
   -- this user, and sends a message via the Emergency package.
end AccountManagementSystem;
