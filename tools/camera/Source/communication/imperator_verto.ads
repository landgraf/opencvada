--
--
--
limited with Imperium_Protocol;
package Imperator_Verto is
--

   ---
   --* Source_T : type that is copied from
   --* Destination_T : type that will contain all bytes from source_T
   --* Copies all bytes from Source_T to Destination_T
   generic
      type Source_T is private;
      type Destination_T is private;
   function Generic_To_Generic (Source : Source_T;
                                Length : Integer := Source_T'Size / 8) return Destination_T;

end Imperator_Verto;
