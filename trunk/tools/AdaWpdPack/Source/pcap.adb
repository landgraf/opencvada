
package body Pcap is
   function Pcap_Open_Live (Device   : String;
                            Snap_Len : Integer;
                            Promisc : Integer;
                            To_Ms    : Integer;
                            Err_Buf  : Pcap_Error_String)
                            return Pcap_Ptr is
      function W_Pcap_Open_Live (Device   : String;
                                 Snap_Len : Integer;
                                 Promisc : Integer;
                                 To_Ms    : Integer;
                                 Err_Buf  : Pcap_Error_String)
                                 return Pcap_Ptr;
      pragma Import (C, W_Pcap_Open_Live, "pcap_open_live");
   begin
      return W_Pcap_Open_Live (Null_Terminate (Device),
                               Snap_Len,
                               Promisc,
                               To_Ms,
                               Err_Buf);
   end Pcap_Open_Live;

   function Pcap_Open_Offline (Fname   : String;
                               Err_Buf : Pcap_Error_String)
                               return Pcap_Ptr is
      function W_Pcap_Open_Offline (Fname   : String;
                                    Err_Buf : Pcap_Error_String)
                                    return Pcap_Ptr;
      pragma Import (C, W_Pcap_Open_Offline, "pcap_open_offline");
   begin
      return W_Pcap_Open_Offline (Null_Terminate (Fname),
                                  Err_Buf);
   end Pcap_Open_Offline;


   function Pcap_Next (Handle : Pcap_Ptr;
                       Header : access Pcap_Packet_Header)
                       return Byte_Array_Ptr is
      function W_Pcap_Next (Handle : Pcap_Ptr;
                            Header : access Pcap_Packet_Header)
                            return Byte_Array_Pointer;
      pragma Import (C, W_Pcap_Next, "pcap_next");

      Packet_Pointer : Byte_Array_Pointer := null;
      Packet_Ptr : Byte_Array_Ptr := null;
   begin
      Packet_Pointer := W_Pcap_Next (Handle, Header);

      if Packet_Pointer = null then
         return null;
      end if;

      Packet_Ptr := new Byte_Array (1 .. Integer (Header.all.Length));
      Packet_Ptr.all := Byte_Array_Pointer_Pkg.Value (Packet_Pointer, Ptrdiff_T (Header.all.Length));
      return Packet_Ptr;
   end Pcap_Next;


   function Get_MAC (Name : String;
                     Desc : String)
                     return Mac_Address is
      function W_Get_Mac (Name : String;
                          Desc : String;
                          Addr : Mac_Address)
                          return Integer;
      pragma Import (C, W_Get_MAC, "GetMAC");

      Ret : Integer;
      Addr : aliased Mac_Address := (others => 16#FF#);
   begin
      Ret := W_Get_Mac (Null_Terminate (Name), Null_Terminate (Desc), Addr);

      if Ret /= 0 then
         Addr := (others => 16#00#);
      end if;

      return Addr;
   end Get_MAC;

   function Pcap_Find_All_Devs (P       : access Pcap_If_Ptr;
                                Err_Buf : Pcap_Error_String)
                                return Integer is
      function Find_Devs (P       : access Pcap_If_Ptr;
                          Err_Buf : Pcap_Error_String)
                          return Integer;
      pragma Import (C, Find_Devs, "pcap_findalldevs");

      It : Pcap_If_Ptr;
      Ret : Integer;
   begin
      Ret := Find_Devs (P, Err_Buf);
      Ret := 0;
      It := P.all;

      while It /= null loop
         Ret := Ret + 1;
         It := It.all.Next;
      end loop;

      return Ret;
   end Pcap_Find_All_Devs;

   function Null_Terminate (Str : String)
                            return String is
   begin
      return String (Str & Ascii.Nul);
   end Null_Terminate;
end Pcap;
