with Interfaces.C; use Interfaces.C;
with linux_types_h;

package linux_if_ether_h is


  ETH_ALEN : constant := 6;  --  /usr/include/linux/if_ether.h:31
  ETH_HLEN : constant := 14;  --  /usr/include/linux/if_ether.h:32
  ETH_ZLEN : constant := 60;  --  /usr/include/linux/if_ether.h:33
  ETH_DATA_LEN : constant := 1500;  --  /usr/include/linux/if_ether.h:34
  ETH_FRAME_LEN : constant := 1514;  --  /usr/include/linux/if_ether.h:35
  ETH_FCS_LEN : constant := 4;  --  /usr/include/linux/if_ether.h:36

  ETH_P_LOOP : constant := 16#0060#;  --  /usr/include/linux/if_ether.h:42
  ETH_P_PUP : constant := 16#0200#;  --  /usr/include/linux/if_ether.h:43
  ETH_P_PUPAT : constant := 16#0201#;  --  /usr/include/linux/if_ether.h:44
  ETH_P_IP : constant := 16#0800#;  --  /usr/include/linux/if_ether.h:45
  ETH_P_X25 : constant := 16#0805#;  --  /usr/include/linux/if_ether.h:46
  ETH_P_ARP : constant := 16#0806#;  --  /usr/include/linux/if_ether.h:47
  ETH_P_BPQ : constant := 16#08FF#;  --  /usr/include/linux/if_ether.h:48
  ETH_P_IEEEPUP : constant := 16#0a00#;  --  /usr/include/linux/if_ether.h:49
  ETH_P_IEEEPUPAT : constant := 16#0a01#;  --  /usr/include/linux/if_ether.h:50
  ETH_P_DEC : constant := 16#6000#;  --  /usr/include/linux/if_ether.h:51
  ETH_P_DNA_DL : constant := 16#6001#;  --  /usr/include/linux/if_ether.h:52
  ETH_P_DNA_RC : constant := 16#6002#;  --  /usr/include/linux/if_ether.h:53
  ETH_P_DNA_RT : constant := 16#6003#;  --  /usr/include/linux/if_ether.h:54
  ETH_P_LAT : constant := 16#6004#;  --  /usr/include/linux/if_ether.h:55
  ETH_P_DIAG : constant := 16#6005#;  --  /usr/include/linux/if_ether.h:56
  ETH_P_CUST : constant := 16#6006#;  --  /usr/include/linux/if_ether.h:57
  ETH_P_SCA : constant := 16#6007#;  --  /usr/include/linux/if_ether.h:58
  ETH_P_TEB : constant := 16#6558#;  --  /usr/include/linux/if_ether.h:59
  ETH_P_RARP : constant := 16#8035#;  --  /usr/include/linux/if_ether.h:60
  ETH_P_ATALK : constant := 16#809B#;  --  /usr/include/linux/if_ether.h:61
  ETH_P_AARP : constant := 16#80F3#;  --  /usr/include/linux/if_ether.h:62
  ETH_P_8021Q : constant := 16#8100#;  --  /usr/include/linux/if_ether.h:63
  ETH_P_IPX : constant := 16#8137#;  --  /usr/include/linux/if_ether.h:64
  ETH_P_IPV6 : constant := 16#86DD#;  --  /usr/include/linux/if_ether.h:65
  ETH_P_PAUSE : constant := 16#8808#;  --  /usr/include/linux/if_ether.h:66
  ETH_P_SLOW : constant := 16#8809#;  --  /usr/include/linux/if_ether.h:67
  ETH_P_WCCP : constant := 16#883E#;  --  /usr/include/linux/if_ether.h:68

  ETH_P_PPP_DISC : constant := 16#8863#;  --  /usr/include/linux/if_ether.h:70
  ETH_P_PPP_SES : constant := 16#8864#;  --  /usr/include/linux/if_ether.h:71
  ETH_P_MPLS_UC : constant := 16#8847#;  --  /usr/include/linux/if_ether.h:72
  ETH_P_MPLS_MC : constant := 16#8848#;  --  /usr/include/linux/if_ether.h:73
  ETH_P_ATMMPOA : constant := 16#884c#;  --  /usr/include/linux/if_ether.h:74
  ETH_P_ATMFATE : constant := 16#8884#;  --  /usr/include/linux/if_ether.h:75

  ETH_P_PAE : constant := 16#888E#;  --  /usr/include/linux/if_ether.h:78
  ETH_P_AOE : constant := 16#88A2#;  --  /usr/include/linux/if_ether.h:79
  ETH_P_TIPC : constant := 16#88CA#;  --  /usr/include/linux/if_ether.h:80
  ETH_P_EDSA : constant := 16#DADA#;  --  /usr/include/linux/if_ether.h:81

  ETH_P_802_3 : constant := 16#0001#;  --  /usr/include/linux/if_ether.h:87
  ETH_P_AX25 : constant := 16#0002#;  --  /usr/include/linux/if_ether.h:88
  ETH_P_ALL : constant := 16#0003#;  --  /usr/include/linux/if_ether.h:89
  ETH_P_802_2 : constant := 16#0004#;  --  /usr/include/linux/if_ether.h:90
  ETH_P_SNAP : constant := 16#0005#;  --  /usr/include/linux/if_ether.h:91
  ETH_P_DDCMP : constant := 16#0006#;  --  /usr/include/linux/if_ether.h:92
  ETH_P_WAN_PPP : constant := 16#0007#;  --  /usr/include/linux/if_ether.h:93
  ETH_P_PPP_MP : constant := 16#0008#;  --  /usr/include/linux/if_ether.h:94
  ETH_P_LOCALTALK : constant := 16#0009#;  --  /usr/include/linux/if_ether.h:95
  ETH_P_CAN : constant := 16#000C#;  --  /usr/include/linux/if_ether.h:96
  ETH_P_PPPTALK : constant := 16#0010#;  --  /usr/include/linux/if_ether.h:97
  ETH_P_TR_802_2 : constant := 16#0011#;  --  /usr/include/linux/if_ether.h:98
  ETH_P_MOBITEX : constant := 16#0015#;  --  /usr/include/linux/if_ether.h:99
  ETH_P_CONTROL : constant := 16#0016#;  --  /usr/include/linux/if_ether.h:100
  ETH_P_IRDA : constant := 16#0017#;  --  /usr/include/linux/if_ether.h:101
  ETH_P_ECONET : constant := 16#0018#;  --  /usr/include/linux/if_ether.h:102
  ETH_P_HDLC : constant := 16#0019#;  --  /usr/include/linux/if_ether.h:103
  ETH_P_ARCNET : constant := 16#001A#;  --  /usr/include/linux/if_ether.h:104
  ETH_P_DSA : constant := 16#001B#;  --  /usr/include/linux/if_ether.h:105
  ETH_P_TRAILER : constant := 16#001C#;  --  /usr/include/linux/if_ether.h:106
  ETH_P_PHONET : constant := 16#00F5#;  --  /usr/include/linux/if_ether.h:107

  -- * INET		An implementation of the TCP/IP protocol suite for the LINUX
  -- *		operating system.  INET is implemented using the  BSD Socket
  -- *		interface as the means of communication with the user level.
  -- *
  -- *		Global definitions for the Ethernet IEEE 802.3 interface.
  -- *
  -- * Version:	@(#)if_ether.h	1.0.1a	02/08/94
  -- *
  -- * Author:	Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
  -- *		Donald Becker, <becker@super.org>
  -- *		Alan Cox, <alan@lxorguk.ukuu.org.uk>
  -- *		Steve Whitehouse, <gw7rrm@eeshack3.swan.ac.uk>
  -- *
  -- *		This program is free software; you can redistribute it and/or
  -- *		modify it under the terms of the GNU General Public License
  -- *		as published by the Free Software Foundation; either version
  -- *		2 of the License, or (at your option) any later version.
  --  

  -- *	IEEE 802.3 Ethernet magic constants.  The frame sizes omit the preamble
  -- *	and FCS/CRC (frame check sequence). 
  --  

  -- *	These are the defined Ethernet Protocol ID's.
  --  

  -- *	Non DIX types. Won't clash for 1500 types.
  --  

  -- *	This is an Ethernet frame header.
  --  

  -- destination eth addr	 
   type ethhdr_h_dest_array is array (0 .. 5) of aliased unsigned_char;
   type ethhdr_h_source_array is array (0 .. 5) of aliased unsigned_char;
   type ethhdr is record
      h_dest : aliased ethhdr_h_dest_array;  -- /usr/include/linux/if_ether.h:114:31
      h_source : aliased ethhdr_h_source_array;  -- /usr/include/linux/if_ether.h:115:33
      h_proto : aliased linux_types_h.uu_be16;  -- /usr/include/linux/if_ether.h:116:10
   end record;
   pragma Convention (C, ethhdr);  -- /usr/include/linux/if_ether.h:113:15

  -- source ether addr	 
  -- packet type ID field	 
end linux_if_ether_h;
