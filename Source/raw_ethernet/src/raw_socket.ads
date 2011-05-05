with Interfaces.C;
--  with Bits_Socket_H;
with Linux_If_Packet_H;
package Raw_Socket is
   package C renames Interfaces.C;

   GETINDEX_FAILED : exception;
   GETMAC_FAILED : exception;
   SETPROMISC_FAILED : exception;

   subtype Byte is  Integer range 0 .. 255;
   Addr_Num_Words   : constant Integer := 6;
   Id_Num_Words : constant Integer := 2;
--     Packet_Size      : constant Integer := 512;
   ETH_FRAME_LEN    : constant Integer := 1518;

   type Addr_Type is array (1 .. Addr_Num_Words) of Byte;
   type Data_Type is array (1 .. ETH_FRAME_LEN) of Byte;
   type Data_Array_Type is array (Positive range <>) of Byte;
   type Id_Type is array (1 .. Id_Num_Words) of Byte;

   pragma Pack (Data_Type);

--     type Buffer_Type is array (1 .. Eth_Frame_Len) of Byte;

   Null_Addr        : constant Addr_Type := (16#00#, 16#00#, 16#00#,
                                             16#00#, 16#00#, 16#00#);
   My_Addr          : constant Addr_Type := (16#00#, 16#19#, 16#B9#,
                                             16#79#, 16#AA#, 16#8F#);
   Src_Addr : constant Addr_Type := (16#01#, 16#01#, 16#01#,
                                             16#01#, 16#01#, 16#01#);

   type Socket_Type is private;
   --     procedure Init;
   function Create_Socket(Iface : String) return Socket_Type;

   procedure Receive (Sock : Socket_Type;
                      Src  : in Addr_Type;
                      Data   : out Data_Type;
                      Length : out Integer);

   procedure Transmit (Sock     : Socket_Type;
                       Dst      : in Addr_Type;
                       Data     : in Data_Type;
                       Length   : in Integer);
   function Get_Addr (Sock : Socket_Type) return Addr_Type;

private
--     type Socket_Type is new Interfaces.C.Int;
   type Socket_Type is record
      S : C.int;
      Iface : C.int;
      Src_Mac : Linux_If_Packet_H.Sockaddr_Ll_Sll_Addr_Array;
   end record;
end Raw_Socket;
