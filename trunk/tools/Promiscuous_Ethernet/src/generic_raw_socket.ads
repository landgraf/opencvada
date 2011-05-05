
with Interfaces.C;
with Linux_If_Packet_H;

generic
   type Data_Item is private;
   with procedure Put(D : Data_Item);
package Generic_Raw_Socket is
   Fail : Natural := 0;
   package C renames Interfaces.C;

   type Byte is new Integer range 0 .. 255;
   for Byte'Size use 8;

   type Id_Type is array (1 .. 2) of Byte;
   pragma Pack (Id_Type);

   -- Mask
   -- (in_id & mask) == in_id
   DEFAULT_DATA_PKG : constant Id_Type := (16#FF#, 16#FF#); -- 1111_1111
   IMAGE_DATA_PKG   : constant Id_Type := (16#1F#, 16#01#); -- 0000_0001
   HARRIS_DATA_PKG  : constant Id_Type := (16#1F#, 16#02#); -- 0000_0010


   GETINDEX_FAILED : exception;
   GETMAC_FAILED : exception;
   SETPROMISC_FAILED : exception;



   Addr_Num_Words   : constant Integer := 6;
   type Addr_Type is array (1 .. Addr_Num_Words) of Byte;
   pragma Pack (Addr_Type);

   Null_Addr        : constant Addr_Type := (16#00#, 16#00#, 16#00#,
                                             16#00#, 16#00#, 16#00#);
   Src_Addr : constant Addr_Type := (16#01#, 16#01#, 16#01#,
                                     16#01#, 16#01#, 16#01#);

   type Socket_Type is private;

   function Create_Socket (Iface : String) return Socket_Type;

   procedure Get (Src : out Addr_Type;
                 Data : out Data_Item;
                 Len : out Natural);
   procedure Start_Tasked_Reading (Sock   : Socket_Type;
                                   Filter : Id_Type);

   procedure Receive (Sock : Socket_Type;
		      Src  : out Addr_Type;
                      Data   : out Data_Item;
                      Len    : out Natural);

   procedure Transmit (Sock     : Socket_Type;
                       Dst      : in Addr_Type;
                       Data     : in Data_Item;
                       Res      : out Integer);
   function Get_Addr (Sock : Socket_Type) return Addr_Type;

private


   Packet_Size      : constant Integer := 512;
   ETH_FRAME_LEN    : constant Integer := 1518;

   type Header_Type is record
      Dst : Addr_Type;
      Src : Addr_Type;
      Id  : Id_Type;
   end record;
   for Header_Type use record
      Dst at 0 range 0 .. 47;
      Src at 0 range 48 .. 95;
      Id  at 0 range 96 .. 111;
   end record;
   for Header_Type'Size use 14 * 8;

   type Data_Type is record
      Header : Header_Type;
      Data   : Data_Item;
   end record;

   for Data_Type use record
      Header at 0 range 0 .. 111;
      -- ...
   end record;
   pragma Pack (Data_Type);

   type Socket_Type is record
      S : C.int;
      Iface : C.int;
      Src_Mac : Linux_If_Packet_H.Sockaddr_Ll_Sll_Addr_Array;
   end record;
end Generic_Raw_Socket;
