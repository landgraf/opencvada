
--
package body Debug is
   procedure Put_Debug (S     : String;
                        Debug : Boolean := True) is
   begin
      if Debug then
         Put_Line (S);
      end if;
   end Put_Debug;


end Debug;
