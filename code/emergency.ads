with Measures; use Measures;

-- This package defines a procedure for contacting emergency services
-- in the case of a FatBat user sufferin a cardiac arrest.
package Emergency is
  -- Send a message to emergency services informing them of the
  --  location and vitals of a patient who has suffered a cardiac
  --  arrest
   procedure ContactEmergency(Wearer : in UserID;
                              Vitals : in BPM;
                              Location : in GPSLocation);
end Emergency;
