with Measures; use Measures;
with Ada.Text_IO;

package body Emergency is

   procedure ContactEmergency(Wearer : in UserID;
			      Vitals : in BPM;
			      Location : in GPSLocation) is
   begin
      Ada.Text_IO.Put_Line("Emergency call placed ");
      Ada.Text_IO.Put_Line("  Wearer: " & UserID'Image(Wearer));
      Ada.Text_IO.Put_Line("  Vitals: " & BPM'Image(Vitals));
      Ada.Text_IO.Put_Line("  Location: (" &
			     Latitude'Image(Location.Lat) & ", " &
			     Longitude'Image(Location.Long) & " )");
   end ContactEmergency;
end Emergency;
