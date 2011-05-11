--
package body Venit_Subcriptio is
--
   --- Creates a ping constant header
   function Ping (Version : Integer := 0;
                  Ack     : Boolean := False;
                  Req     : Boolean := True) return Constant_Header is
      Temp : Constant_Header;
   begin
      Temp.Version := Header_Version (Version);
      Temp.Ack := Boolean'Pos (Ack);
      Temp.Req := Boolean'Pos (Req);
      Temp.Eof := 1;
      Temp.Flags := 2#0001#;
      return Temp;
   end Ping;
end Venit_Subcriptio;
