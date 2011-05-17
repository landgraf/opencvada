
with Interfaces;
with Interfaces.C.Strings;
with Interfaces.C.Pointers;
with Interfaces.C;
with System; use System;

package Pcap is
   use Interfaces;
   use Interfaces.C;
   use Interfaces.C.Strings;

   type Mac_Address is array (Integer range 0 .. 5) of Unsigned_8;

   -- Descriptor of an open capture instance. This structure is opaque to the
   -- user, that handles its content through the functions provided by wpcap.dll
   type Pcap is null record;
   pragma Convention (C_Pass_By_Copy, Pcap);
   type Pcap_Ptr is access all Pcap;

   -- Libpcap savefile descriptor.
   type Pcap_Dumper is null record;
   pragma Convention (C_Pass_By_Copy, Pcap_Dumper);
   type Pcap_Dumper_Ptr is access all Pcap_Dumper;

   type Pcap_Addr is null record;
   type Pcap_Addr_Ptr is access all Pcap_Addr;

   -- Item in a list of interfaces
   type Pcap_If;
   type Pcap_If_Ptr is access all Pcap_If;
   type Pcap_If is record
      Next        : Pcap_If_Ptr;
      Name        : Chars_Ptr; -- Name to hand to "Pcap_Open_Live()".
      Description : Chars_Ptr; -- Textual description of interface, or NULL.
      Addresses   : Pcap_Addr_Ptr;
      Flags       : Unsigned_32; -- Pcap_If_ interface flags.
   end record;

   -- Header of a libpcap dump file.
   type Pcap_File_Header is record
      Magic         : Integer_32;
      Version_Major : Unsigned_8;
      Version_Minor : Unsigned_8;
      This_Zone     : Unsigned_32; -- GMT to local correction.
      Sig_Figs      : Unsigned_32; -- Accuracy of timestamps.
      Snap_Len      : Unsigned_32; -- Max length saved portion of each pkt.
      Link_Type     : Unsigned_32; -- Data link type (Linktype_*)
   end record;

   type Byte_Array is array (Integer range <>) of aliased Unsigned_8;
   type Byte_Array_Ptr is access all Byte_Array;
   Null_Byte_Array : constant Byte_Array (1 .. 0) := (others => 0);

--     subtype Mac_Address is Byte_Array (0 .. 5);
--     type Mac_Address_Ptr is access all Mac_Address;

   -- Equivalent to the timeval struct in C.
   type C_Timeval is record
      Tv_Sec  : Long_Integer;
      Tv_Usec : Long_Integer;
   end record;
   pragma Convention (C_Pass_By_Copy, C_Timeval);

   -- Header of a packet in the dump file. Each packet in the dump file is
   -- prepended with this generic header. This gets around the problem of
   -- different headers for different packet interfaces.
   type Pcap_Packet_Header is record
      Time_Stamp : C_Timeval;
      Cap_Length : Unsigned_32;
      Length     : Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Pcap_Packet_Header);
   type Pcap_Packet_Header_Ptr is access all Pcap_Packet_Header;


   type Pcap_Stat is record
      Received   : Unsigned_32; -- Packets transited on the network.
      Dropped    : Unsigned_32; -- Packets dropped by the driver.
      If_Dropped : Unsigned_32; -- Packets dropped by the interface. (N/A)
      Captured   : Unsigned_32; -- Packets captured by filter. (Win32 only)
   end record;
   type Pcap_Stat_Ptr is access all Pcap_Stat;
   pragma Convention (C_Pass_By_Copy, Pcap_Stat);


   Pcap_Error_Size           : constant := 256;

   -- Error codes for the pcap API. These will all be negative, so you can check
   -- for the success or failure of a call that returns these codes by checking
   -- for a negative value.
   Pcap_Error                : constant := -1;
   Pcap_Error_Break          : constant := -2;
   Pcap_Error_Not_Activated  : constant := -3;
   Pcap_Error_Activated      : constant := -4;
   Pcap_Error_No_Such_Device : constant := -5;
   Pcap_Error_Rfmon_Notsup   : constant := -6;
   Pcap_Error_Not_Rfmon      : constant := -7;
   Pcap_Error_Perm_Denied    : constant := -8;
   Pcap_Error_Iface_Not_Up   : constant := -9;

   subtype Pcap_Error_String is String (1 .. Pcap_Error_Size);


   -- Prototype of the callback function that receives the packets.
   type Pcap_Handler is access procedure (User          : System.Address;
                                          Packet_Header : Pcap_Packet_Header_Ptr;
                                          Packet_Data   : System.Address);
   pragma Convention (C, Pcap_Handler);

   -- Open a live capture from the network.
   function Pcap_Open_Live (Device   : String;
                            Snap_Len : Integer;
                            Promisc  : Integer;
                            To_Ms    : Integer;
                            Err_Buf  : Pcap_Error_String)
                            return Pcap_Ptr;

   -- Switch between blocking and nonblocking mode.
   function Pcap_Set_Non_Block (Handle    : Pcap_Ptr;
                                Non_Block : Integer;
                                Err_Buf   : Pcap_Error_String)
                                return Integer;

   -- Get the "non-blocking" state of an interface
   function Pcap_Get_Non_Block (Handle  : Pcap_Ptr;
                                Err_Buf : Pcap_Error_String)
                                return Integer;

   -- Construct a list of network devices that can be opened with Pcap_Open_Live().
   function Pcap_Find_All_Devs (P       : access Pcap_If_Ptr;
                                Err_Buf : Pcap_Error_String)
                                return Integer;

   -- Free an interface list return by Pcap_Find_All_Devs().
   procedure Pcap_Free_All_Devs (Alldevsp : Pcap_If_Ptr);

   -- Collect a group of packets
   function Pcap_Dispatch (Handle   : Pcap_Ptr;
                           Cnt      : Integer;
                           Callback : Pcap_Handler)
--                             User     : access Unsigned_8 := null)
                           return Integer;

   -- Collect a group of packets
   function Pcap_Loop (Handle   : Pcap_Ptr;
                       Cnt      : Integer;
                       Callback : Pcap_Handler)
--                         User     : access Unsigned_8 := null)
                       return Integer;

   -- Return the next available packet.
   function Pcap_Next (Handle : Pcap_Ptr;
                       Header : access Pcap_Packet_Header)
                       return Byte_Array_Ptr;

   -- Read a packet from an interface or from an offline capture
   function Pcap_Next_Ex (Handle        : Pcap_Ptr;
                          Packet_Header : access Pcap_Packet_Header_Ptr;
                          Packet_Data   : access Byte_Array_Ptr)
                          return Integer;


   -- Set a flag that will force Pcap_Dispatch() or Pcap_Loop to return rather than looping.
   procedure Pcap_Break_Loop (Handle : Pcap_Ptr);

   -- Send a raw frame.
   function Pcap_Send_Packet (Handle : Pcap_Ptr;
                              Buf    : Byte_Array;
                              Size   : Integer)
                              return Integer;

   -- Close the files associated with P and deallocates resources.
   procedure Pcap_Close (Handle : Pcap_Ptr);

   -- Return statistics on current capture.
   function Pcap_Stats_Ex (Handle         : Pcap_Ptr;
                           Pcap_Stat_Size : access Integer)
                           return Pcap_Stat_Ptr;

   function Get_MAC (Name : String;
                     Desc : String)
                     return Mac_Address;

private
   package Byte_Array_Pointer_Pkg is
     new Interfaces.C.Pointers (Integer, Unsigned_8, Byte_Array, 0);
   use Byte_Array_Pointer_Pkg;
   subtype Byte_Array_Pointer is Byte_Array_Pointer_Pkg.Pointer;

   function Null_Terminate (Str : String)
                            return String;

   pragma Import (C, Pcap_Set_Non_Block, "pcap_setnonblock");
   pragma Import (C, Pcap_Get_Non_Block, "pcap_getnonblock");
--     pragma Import (C, Pcap_Find_All_Devs, "pcap_findalldevs");
   pragma Import (C, Pcap_Free_All_Devs, "pcap_freealldevs");
   pragma Import (C, Pcap_Dispatch,      "pcap_dispatch");
   pragma Import (C, Pcap_Loop,          "pcap_loop");
   pragma Import (C, Pcap_Next_Ex,       "pcap_next_ex");
   pragma Import (C, Pcap_Break_Loop,    "pcap_breakloop");
   pragma Import (C, Pcap_Send_Packet,   "pcap_sendpacket");
   pragma Import (C, Pcap_Close,         "pcap_close");
   pragma Import (C, Pcap_Stats_Ex,      "pcap_stats_ex");

end Pcap;
