
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Sys_Types_H;
with Sys_Socket_H;
with Linux_If_H;
with Linux_If_Ether_H;
with Linux_If_Arp_H;
with Linux_If_Packet_H;
with Bits_Socket_H;
with Interfaces.C;
with Ada.Unchecked_Conversion;
with System.Storage_Elements;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Interfaces;
with Sys_Ioctl_H;
package body Generic_Raw_Socket is

   type Data_Type_Ptr is access all Data_Type;

   Buffer : aliased Data_Type;
   Buff_P : Data_Type_Ptr := Buffer'Access;
   package Sys_Socket is new Sys_Socket_H (Data_Type_Ptr);

   procedure Free is new Ada.Unchecked_Deallocation (Linux_If_H.Ifreq, Linux_If_H.Ifreq_Ptr);
   --     package C renames Interfaces.C;
   ----------
   -- Init --
   ----------

   function Bswap_16 (X : C.Int) return C.Int is
      type Byte is new Integer range 0 .. 255;
      type Byte_Arr is array (1 .. 4) of Byte;
      pragma Pack (Byte_Arr);
      for Byte_Arr'Size use 4 * 8;
      function To_Byte is new Ada.Unchecked_Conversion (C.Int, Byte_Arr);
      function To_Int is new Ada.Unchecked_Conversion (Byte_Arr, C.Int);
      B : Byte_Arr := To_Byte (X);
      T : Byte;
   begin
      T := B (1);
      B (1) := B (2);
      B (2) := T;
      T := B (3);
      B (3) := B (4);
      B (4) := T;
      return To_Int (B);
   end Bswap_16;

   function Htons (X : C.Int) return C.Int is
   begin
      return Bswap_16 (X);
   end Htons;


   function To_Sll_Addr (Data : Bits_Socket_H.Sockaddr_Sa_Data_Array)
                         return Linux_If_Packet_H.Sockaddr_Ll_Sll_Addr_Array is
      use Linux_If_Packet_H;
      Sl : Sockaddr_Ll_Sll_Addr_Array := (others => C.Unsigned_Char (0));
   begin
      for I in 0 .. 5 loop
         Sl (I) := C.Unsigned_Char (Character'Pos (Character (Data (I))));
      end loop;
      return Sl;
   end To_Sll_Addr;


   function Create_Socket (Iface : String) return Socket_Type is
      use type C.Int;
      Sock   : Socket_Type;
      Carr   : C.Char_Array := C.To_C (Iface);
      Ifr    : Linux_If_H.Ifreq_Ptr := new Linux_If_H.Ifreq;

   begin
      Sock.S := Sys_Socket.Socket (Bits_Socket_H.PF_PACKET,
                                   C.Int (Bits_Socket_H.SOCK_RAW),
                                   Htons (Linux_If_Ether_H.ETH_P_ALL));
      -- copy interface name string.
      for I in Iface'Range loop
         Ifr.Ifr_Ifrn.Ifrn_Name (I - 1) := C.Char (Iface (I));
      end loop;
      Ifr.Ifr_Ifrn.Ifrn_Name (Iface'Length .. Ifr.Ifr_Ifrn.Ifrn_Name'Last) := (others => C.Char (Character'Val (0)));
      -- Request the interface index
      if Sys_Ioctl_H.Ioctl (C.Int (Sock.S), Sys_Ioctl_H.SIOCGIFINDEX, Ifr) = -1 then
         Put_Line ("Failed to fetch index of network interface: " & Iface);
         for I in Iface'Range loop
            Put (Character (Ifr.Ifr_Ifrn.Ifrn_Name (I - 1))); -- C-range 0..?
         end loop;
         New_Line;
         Sock.S := -1;
         raise GETINDEX_FAILED;
      else
         Put ("Interface index:" & Ifr.Ifr_Ifru.Ifru_Ivalue'Img);
         Sock.Iface := Ifr.Ifr_Ifru.Ifru_Ivalue;
      end if;
      -- Request MAC address of interface
      if Sys_Ioctl_H.Ioctl (C.Int (Sock.S), Sys_Ioctl_H.SIOCGIFHWADDR, Ifr) = -1 then
         Put_Line ("Failed to fetch MAC of network interface: " & Iface);
         Sock.S := -1;
         raise GETMAC_FAILED;
      else
         for I in 0 .. 5 loop
            Put (Character'Pos (Character (Ifr.Ifr_Ifru.Ifru_Hwaddr.Sa_Data (I))), 7, 16);
            Put (',');
         end loop;
         New_Line;
         Sock.Src_Mac := To_Sll_Addr (Ifr.Ifr_Ifru.Ifru_Hwaddr.Sa_Data);
      end if;

      declare
         use Interfaces;
      begin
         Ifr.Ifr_Ifru.Ifru_Flags := C.Short (Interfaces.Unsigned_16 (Ifr.Ifr_Ifru.Ifru_Flags) or Linux_If_H.IFF_PROMISC);
      end;


      if Sys_Ioctl_H.Ioctl (C.Int (Sock.S), Sys_Ioctl_H.Siocsifflags, Ifr) = -1 then
         Put_Line ("Failed to enable PROMISC-mode on network interface: " & Iface);
         Sock.S := -1;
         raise SETPROMISC_FAILED;
      end if;

      Free (Ifr);
      return Sock;
   end Create_Socket;

   function Get_Addr (Sock : Socket_Type) return Addr_Type is
      Addr : Addr_Type;
   begin
      for I in 1 .. 6 loop
         Addr (I) := Byte (Sock.Src_Mac (I - 1));
      end loop;
      return Addr;
   end Get_Addr;

   -------------
   -- Receive --
   -------------
   function To_Sockaddr (In_Addr : Addr_Type) return Linux_If_Packet_H.Sockaddr_Ll is
      use type C.Unsigned;
      Addr : Linux_If_Packet_H.Sockaddr_Ll;
   begin
      for I in 1 .. 6 loop
         Addr.Sll_Addr (I - 1) := C.Unsigned_Char (In_Addr (I));
      end loop;
      Addr.Sll_Family := Bits_Socket_H.PF_PACKET;
      Addr.Sll_Protocol := 0;
      Addr.Sll_Ifindex := 0; --Sock.Iface;
      Addr.Sll_Hatype := Linux_If_Arp_H.ARPHRD_ETHER;
      Addr.Sll_Pkttype := Linux_If_Packet_H.PACKET_OTHERHOST;
      Addr.Sll_Halen := Linux_If_Ether_H.ETH_ALEN;
      return Addr;
   end To_Sockaddr;

   procedure Receive (Sock   : Socket_Type;
                      Data   : out Data_Type;
                      Len    : out Natural) is
      use type C.Unsigned;
      Addr   : aliased Linux_If_Packet_H.Sockaddr_Ll;
      Addr_Len : aliased C.Unsigned := Linux_If_Packet_H.Sockaddr_Ll'Size / 8;
   begin

      Addr := To_Sockaddr (Src_Addr);
      Addr.Sll_Addr := (16#0#, 16#0#, 16#0#, 16#0#, 16#0#, 16#0#, 16#0#, 16#0#);
      Len := Integer (Sys_Socket.Recvfrom (C.Int (Sock.S),
        Buff_P, -- buffer
        C.Unsigned (Data'Size / 8), -- data length
        0, -- flags
        Addr'Access, -- address of sender
        Addr_Len'Access)); -- actual size of Addr

      Data := Buffer;

   end Receive;

   procedure Receive (Sock   : Socket_Type;
                      Src    : out Addr_Type;
                      Data   : out Data_Item;
                      Len    : out Natural) is
      D : Data_Type;
   begin
      Receive (Sock, D, Len);
      Data := D.Data;
      Src := D.Header.Src;
   end Receive;
   --------------
   -- Transmit --
   --------------

   Tbuff : aliased Data_Type;
   Tbuff_Ptr : Data_Type_Ptr := Tbuff'Access;
   procedure Transmit (Sock     : Socket_Type;
                       Dst      : in Addr_Type;
                       Data     : in Data_Item;
                       Res      : out Integer) is
      use Linux_If_Ether_H;
      use type C.Int;
      use type C.Unsigned;
      Addr   : aliased Linux_If_Packet_H.Sockaddr_Ll;
   begin
      ----------------------------------------------------------------------------------
      -- | DST | SRC | ID |     DATA     | FCS |
      --   6B    6B    2B    46 - 1500B    4B
      ----------------------------------------------------------------------------------

      for I in 1 .. ETH_ALEN loop
         Tbuff.Header.Dst (I) := Dst (I);
         Tbuff.Header.Src (I) := Byte (Sock.Src_Mac (I));
      end loop;
      Tbuff.Header.Id := DEFAULT_DATA_PKG;
      Tbuff.Data := Data;

      Addr.Sll_Addr := Sock.Src_Mac (0 .. 7) ;
      Addr.Sll_Family := Bits_Socket_H.PF_PACKET;
      Addr.Sll_Protocol := 0;
      Addr.Sll_Ifindex := Sock.Iface; -- interface index.
      Addr.Sll_Hatype := Linux_If_Arp_H.ARPHRD_ETHER;
      Addr.Sll_Pkttype := Linux_If_Packet_H.PACKET_BROADCAST;
      declare
         use C;
      begin
         Addr.Sll_Halen := Linux_If_Ether_H.ETH_ALEN + 2;
      end;

      Res := Integer (Sys_Socket.Sendto (C.Int (Sock.S),
        Tbuff_Ptr,
        C.Unsigned (Data'Size / 8),
        Sock.Iface,
        Addr'Access,
        Addr'Size / 8));
      if Res = -1 then
         Put ("**** Failed ****: "); New_Line;
      end if;
   end Transmit;

   type Internal_Buffer_Array is array (Positive range <>) of Data_Type;
   protected Internal_Buffer is
      procedure Put (Packet : Data_Type);
      entry Get (D : out Data_Type);
   private
      Arr   : Internal_Buffer_Array (1 .. 2_000);
      Count : Integer := 0;
      First : Integer := 1;
      Last  : Integer := 1;

   end Internal_Buffer;

   procedure Get (Src  : out Addr_Type;
                  Data : out Data_Item;
                  Len  : out Natural) is
      Item : Data_Type;
   begin
      Internal_Buffer.Get (Item);
      Src  := Item.Header.Src;
      Data := Item.Data;
      Len  := Data'Size / 8;
   end Get;


   protected body Internal_Buffer is
      procedure Put (Packet : Data_Type) is
      begin
         Count := Count + 1;
         if Count > Arr'Length then
            Fail := Fail + 1;
         end if;
         Arr (Last) := Packet;
         Last := Last + 1;
         if Last > Arr'Last then
            Last := Arr'First;
         end if;
      end Put;
      entry Get (D : out Data_Type) when Count > 0 is
      begin
         Count := Count - 1;
         D := Arr (First);
         First := First + 1;
         if First > Arr'Last then
            First := Arr'First;
         end if;
      end Get;
   end Internal_Buffer;

   task Reading_Task is
      entry Start (Sock   : Socket_Type;
                   Filter : Id_Type);
   end Reading_Task;

   procedure Start_Tasked_Reading (Sock          : Socket_Type;
                                   Filter        : Id_Type) is
   begin
      Reading_Task.Start (Sock, Filter);
   end Start_Tasked_Reading;

   function "and" (L, R : Byte) return Byte is
      use Interfaces;
      Ll : Unsigned_8 := Unsigned_8 (L);
      Rr : Unsigned_8 := Unsigned_8 (R);
   begin
      return Byte (LL and RR);
   end "and";
   pragma Inline ("and");
   function "and" (L, R : Id_Type) return Id_Type is
      Res : Id_Type;
   begin
      for I in Id_Type'Range loop
         Res (I) := L (I) and R (I);
      end loop;
      return Res;
   end "and";
   pragma Inline ("and");

   function Filter (Id, Mask : Id_Type) return Boolean is
   begin
      return (Id and Mask) = Id;
   end Filter;
   Hexstr : String (1 .. 16) := "0123456789ABCDEF";
   function To_Hex (Id : Id_Type) return String is
      Str : String (1 .. 6) := "0x0000";
   begin
      Str (3) := Hexstr (Integer (Id (1)) / 16 + 1);
      Str (4) := Hexstr (Integer (Id (1)) mod 16 + 1);
      Str (5) := Hexstr (Integer (Id (2)) / 16 + 1);
      Str (6) := Hexstr (Integer (Id (2)) mod 16 + 1);
      return Str;
   end To_Hex;

   task body Reading_Task is
      Data  : Data_Type;
      Len   : Natural;
      Lsock : Socket_Type;
      Filt  : Id_Type;
   begin
      accept Start (Sock : Socket_Type;
                    Filter : Id_Type) do
         Lsock := Sock;
         Filt := Filter;
      end Start;
      Put_Line ("Reading task started, Mask: " & To_Hex (Filt));
      loop
         begin
            Data.Header.Id := (others => 0);
            Receive (Sock => LSock,
                     Data => Data,
                     Len  => Len);
            if Filter (Data.Header.Id, Filt) then
               Internal_Buffer.Put (Data);
            end if;
         exception
            when E : others => Put_Line (Exception_Information (E));
         end;
      end loop;
   end Reading_Task;

end Generic_Raw_Socket;
