/** Data types **/
sig UserID {}
// There is exactly one emergency userID
one sig EmergencyUser extends UserID {}

// There are three types of contacts, Friends, Insurer, and Emergency.
// Users can give different permissions to different contact types.
abstract sig ContactTypes {}
one sig Friend, Insurer, Emergency extends ContactTypes {}

// Three types of data stored: number of foosteps, beats per minute,
// and location
sig Footsteps {}
sig BPM {}
sig GPSLocation {}

// A simple Boolean data type
abstract sig Boolean {}
one sig True, False extends Boolean {}

/** The system state **/
sig AMS {
    // The set of current users
    users : set UserID-EmergencyUser,

    // Records each user's friend, insurer
    friends : users->users,
    insurers : users->users,

    // Records each user's relevant data
    footsteps : users->Footsteps,
    vitals : users->BPM,
    locations : users->GPSLocation,

    // Records for each user the permissions given for each data type
    // and role. Many to many.
    footstepsPermissions : users->ContactTypes,
    vitalsPermissions : users->ContactTypes,
    locationPermissions : users->ContactTypes,
}
{
    // Since every user has the single Emergency user as the
    // emergency contact, there's no reason to have it as a relation.

    // Nobody can be their own friend or insurer.
    all u, v : UserID | u = v implies (u->v) not in insurers+friends

    // The insurer always has permissions to read footsteps.
    all u : users | (u->Insurer) in footstepsPermissions
}

/** Initial state **/
pred init [ams : AMS] {
    no ams.users
}

/** Users and their social network **/
// Create a new user
pred CreateUser [ams, ams' : AMS, userID: one UserID] {
    userID not in ams.users
    ams'.users = ams.users + userID

    // Unchanged
    ams'.friends = ams.friends
    ams'.insurers = ams.insurers
    DataUnchanged [ams, ams']
    ams'.footstepsPermissions = ams.footstepsPermissions ++ (userID->Insurer)
    ams'.vitalsPermissions = ams.vitalsPermissions
    ams'.locationPermissions = ams.locationPermissions
}

// Update, remove, and read insurer information for a specific user
pred SetInsurer [ams, ams' : AMS, wearer, insurer, requester : UserID] {
    wearer+insurer in ams.users
    ams'.insurers = ams.insurers + (wearer->insurer)
    wearer = requester

    // Unchanged
    ams'.users = ams.users
    ams'.friends = ams.friends
    DataUnchanged [ams, ams']
    PermissionsUnchanged [ams, ams']
}

pred RemoveInsurer [ams, ams' : AMS, wearer, requester : UserID] {
    some ams.insurers[wearer]
    ams'.insurers = ams.insurers - (wearer->UserID)
    wearer = requester

    // Unchanged
    ams'.users = ams.users
    ams'.friends = ams.friends
    DataUnchanged [ams, ams']
    PermissionsUnchanged [ams, ams']
}

pred ReadInsurer [ams : AMS, wearer, requester : UserID, insurer : lone UserID] {
    wearer = requester

    insurer = ams.insurers[wearer]
}

// Update, remove, and read friend information for a specific user
pred SetFriend [ams, ams' : AMS, wearer, friend, requester: UserID] {
    wearer = requester
    (wearer + friend) in ams.users
    ams'.friends = ams.friends ++ (wearer->friend)

    // Unchanged:
    ams'.users = ams.users
    ams'.insurers = ams.insurers
    DataUnchanged [ams, ams']
    PermissionsUnchanged [ams, ams']
}

pred RemoveFriend [ams, ams' : AMS, wearer, requester : UserID] {
    wearer = requester
    some ams.friends[wearer]
    ams'.friends = ams.friends - (wearer->UserID)

    // Unchanged:
    ams'.users = ams.users
    ams'.insurers = ams.insurers
    DataUnchanged [ams, ams']
    PermissionsUnchanged [ams, ams']
}

pred ReadInsurer [ams : AMS, wearer, requester : UserID, friend : lone UserID] {
    wearer = requester

    friend = ams.friends[wearer]
}

/** Data management **/
// Update relevant data
pred UpdateFootsteps [ams, ams' : AMS,
                      wearer : UserID,
                      newFootsteps : Footsteps] {
    wearer in ams.users
    ams'.footsteps = ams.footsteps ++ (wearer->newFootsteps)

    // Unchanged:
    ams'.vitals = ams.vitals
    ams'.locations = ams.locations
    UsersUnchanged [ams, ams']
    PermissionsUnchanged [ams, ams']
}

pred ReadFootsteps [ams : AMS,
                    wearer, reader : UserID,
                    footstepsValue : Footsteps] {
    wearer in ams.users

    (
        wearer->reader in ams.insurers
    || (
        wearer->reader in ams.friends
        && wearer->Friend in ams.footstepsPermissions
    ) || (
        reader = EmergencyUser
        && wearer->Emergency in ams.footstepsPermissions
    ))

    footstepsValue = ams.footsteps[wearer]
}

pred UpdateVitals [ams, ams' : AMS, wearer : UserID, newVitals : BPM] {
    wearer in ams.users
    ams'.vitals = ams.vitals ++ (wearer->newVitals)

    // Unchanged:
    ams'.footsteps = ams.footsteps
    ams'.locations = ams.locations
    UsersUnchanged [ams, ams']
    PermissionsUnchanged [ams, ams']
}

pred ReadVitals [ams : AMS, wearer, reader : UserID, vitalsValue : BPM] {
    wearer in ams.users

    ((
        wearer->reader in ams.insurers
        && wearer->Insurer in ams.vitalsPermissions
    ) || (
        wearer->reader in ams.friends
        && wearer->Friend in ams.vitalsPermissions
    ) || (
        reader = EmergencyUser
        && wearer->Emergency in ams.vitalsPermissions
    ))

    vitalsValue = ams.vitals[wearer]
}

pred UpdateLocation [ams, ams' : AMS,
                     wearer : UserID,
                     newLocation : GPSLocation] {
    wearer in ams.users
    ams'.locations = ams.locations ++ (wearer->newLocation)

    // Unchanged:
    ams'.footsteps = ams.footsteps
    ams'.vitals = ams.vitals
    UsersUnchanged [ams, ams']
    PermissionsUnchanged [ams, ams']
}

pred ReadLocation [ams : AMS,
                   wearer, reader : UserID,
                   locationValue : GPSLocation] {
    wearer in ams.users

    ((
        wearer->reader in ams.insurers
        && wearer->Insurer in ams.locationPermissions
    ) || (
        wearer->reader in ams.friends
        && wearer->Friend in ams.locationPermissions
    ) || (
        reader = EmergencyUser
        && wearer->Emergency in ams.locationPermissions
    ))

    locationValue = ams.locations[wearer]
}

/** Permission management **/

pred AddFootstepsPermission [ams, ams' : AMS,
                             wearer : UserID,
                             contact : ContactTypes] {
    // Use `+` not `++` because multiple contacts may have permission
    // to view the same data.
    ams'.footstepsPermissions = ams.footstepsPermissions + (wearer->contact)

    ams'.vitalsPermissions = ams.vitalsPermissions
    ams'.locationPermissions = ams.locationPermissions
    UsersUnchanged [ams, ams']
    DataUnchanged [ams, ams']
}

pred RemoveFootstepsPermission [ams, ams' : AMS,
                                wearer : UserID,
                                contact : ContactTypes] {
    contact != Insurer

    ams'.footstepsPermissions = ams.footstepsPermissions - (wearer->contact)

    ams'.vitalsPermissions = ams.vitalsPermissions
    ams'.locationPermissions = ams.locationPermissions
    UsersUnchanged [ams, ams']
    DataUnchanged [ams, ams']
}

pred AddVitalsPermission [ams, ams' : AMS,
                          wearer : UserID,
                          contact : ContactTypes] {
    // Use `+` not `++` because multiple contacts may have permission
    // to view the same data.
    ams'.vitalsPermissions = ams.vitalsPermissions + (wearer->contact)

    ams'.footstepsPermissions = ams.footstepsPermissions
    ams'.locationPermissions = ams.locationPermissions
    UsersUnchanged [ams, ams']
    DataUnchanged [ams, ams']
}

pred RemoveVitalsPermission [ams, ams' : AMS,
                             wearer : UserID,
                             contact : ContactTypes] {
    ams'.vitalsPermissions = ams.vitalsPermissions - (wearer->contact)

    ams'.footstepsPermissions = ams.footstepsPermissions
    ams'.locationPermissions = ams.locationPermissions
    UsersUnchanged [ams, ams']
    DataUnchanged [ams, ams']
}

pred AddLocationPermission [ams, ams' : AMS,
                            wearer : UserID,
                            contact : ContactTypes] {
    // Use `+` not `++` because multiple contacts may have permission
    // to view the same data.
    ams'.locationPermissions = ams.locationPermissions + (wearer->contact)

    ams'.footstepsPermissions = ams.footstepsPermissions
    ams'.vitalsPermissions = ams.vitalsPermissions
    UsersUnchanged [ams, ams']
    DataUnchanged [ams, ams']
}

pred RemoveLocationPermission [ams, ams' : AMS,
                               wearer : UserID,
                               contact : ContactTypes] {
    ams'.locationPermissions = ams.locationPermissions - (wearer->contact)

    ams'.footstepsPermissions = ams.footstepsPermissions
    ams'.vitalsPermissions = ams.vitalsPermissions
    UsersUnchanged [ams, ams']
    DataUnchanged [ams, ams']
}

/** Handling cardiac arrest **/
pred CardiacArrestOccured [ams, ams' : AMS,
                           wearer : UserID,
                           wearerLocation : GPSLocation,
                           wearerVitals : BPM] {
    (wearer->Emergency in ams.vitalsPermissions) => (
        ams'.locations = ams.locations ++ (wearer->wearerLocation)
        && ams'.vitals = ams.vitals ++ (wearer->wearerVitals)
        && ams'.footsteps = ams.footsteps
        && UsersUnchanged [ams, ams']
        && PermissionsUnchanged [ams, ams']
        && EmergencyServicesContacted [wearer, wearerLocation, wearerVitals]
    ) else (
        UsersUnchanged [ams, ams']
        && DataUnchanged [ams, ams']
        && PermissionsUnchanged [ams, ams']
        && !EmergencyServicesContacted [wearer, wearerLocation, wearerVitals]
    )
}


/** Models of "external" API calls **/
// ContactEmergency -- The external call specified in the 'Emergency'
// package in Assignment 1
pred EmergencyServicesContacted [wearer : UserID,
                                 wearerLocation : GPSLocation,
                                 wearerVitals : BPM] {
    // Contact emergency services
}

/** Helper predicates **/
// Users and their social network are unchanged.
pred UsersUnchanged [ams, ams' : AMS] {
    ams'.users = ams.users
    ams'.friends = ams.friends
    ams'.insurers = ams.insurers
}

// Data about users isunchanged.
pred DataUnchanged [ams, ams' : AMS] {
    ams'.footsteps = ams.footsteps
    ams'.vitals = ams.vitals
    ams'.locations = ams.locations
}

// Permissions for users are unchanged.
pred PermissionsUnchanged [ams, ams' : AMS] {
    ams'.footstepsPermissions = ams.footstepsPermissions
    ams'.vitalsPermissions = ams.vitalsPermissions
    ams'.locationPermissions = ams.locationPermissions
}

run CreateUser for 2

// Creating a new user does not add any friends/insurers
assert NoUserChange {
    all ams, ams' : AMS, userID : UserID |
        CreateUser[ams, ams', userID]
            => ams.friends = ams'.friends and ams.insurers = ams'.insurers
}
check NoUserChange for 3

// Hazard 3.1.8 -- system allows wearer's data to be read before it is set,
//
assert VitalsDataCannotBeReadBeforeBeingSet {
    all ams, ams', ams'', ams''', ams'''' : AMS,
    wearer, friend : UserID,
    anyVitals : BPM | (
        (
            init [ams]
            && CreateUser [ams, ams', wearer]
            && CreateUser [ams', ams'', friend]
            && SetFriend [ams'', ams''', wearer, friend, wearer]
            && AddVitalsPermission [ams''', ams'''', wearer, Friend]
        ) => !ReadVitals [ams, wearer, friend, anyVitals]
    )
}
check VitalsDataCannotBeReadBeforeBeingSet for 5

// Hazard 3.3.2 -- system allows a wearer's insurer to be removed by
// someone other than that wearer.

assert InsurerMayNotBeRemovedByOthers {
    all ams, ams' : AMS, wearer, insurer, requester : UserID | (
        (
            requester in ams.users
            && (wearer->insurer) in ams.insurers
            && wearer != requester
        ) => !RemoveInsurer [ams, ams', wearer, requester]
    )
}
check InsurerMayNotBeRemovedByOthers for 3

// Hazard 3.9.1 -- system doesn't allow anyone to read data,
// even if they have permission.
assert VitalsReadingAllowedIfPermitted {
    all ams : AMS, wearer, friend : UserID, wearerVitals : BPM |
        ((wearer->friend in ams.friends
         && ams.vitals[wearer] = wearerVitals
         && (wearer->Friend) in ams.vitalsPermissions) =>
        ReadVitals [ams, wearer, friend, wearerVitals])
}
check VitalsReadingAllowedIfPermitted for 3
