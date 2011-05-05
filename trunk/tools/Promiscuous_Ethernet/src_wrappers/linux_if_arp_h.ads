with Interfaces.C; use Interfaces.C;
with bits_socket_h;
with linux_types_h;

package linux_if_arp_h is


  ARPHRD_NETROM : constant := 0;  --  /usr/include/linux/if_arp.h:29
  ARPHRD_ETHER : constant := 1;  --  /usr/include/linux/if_arp.h:30
  ARPHRD_EETHER : constant := 2;  --  /usr/include/linux/if_arp.h:31
  ARPHRD_AX25 : constant := 3;  --  /usr/include/linux/if_arp.h:32
  ARPHRD_PRONET : constant := 4;  --  /usr/include/linux/if_arp.h:33
  ARPHRD_CHAOS : constant := 5;  --  /usr/include/linux/if_arp.h:34
  ARPHRD_IEEE802 : constant := 6;  --  /usr/include/linux/if_arp.h:35
  ARPHRD_ARCNET : constant := 7;  --  /usr/include/linux/if_arp.h:36
  ARPHRD_APPLETLK : constant := 8;  --  /usr/include/linux/if_arp.h:37
  ARPHRD_DLCI : constant := 15;  --  /usr/include/linux/if_arp.h:38
  ARPHRD_ATM : constant := 19;  --  /usr/include/linux/if_arp.h:39
  ARPHRD_METRICOM : constant := 23;  --  /usr/include/linux/if_arp.h:40
  ARPHRD_IEEE1394 : constant := 24;  --  /usr/include/linux/if_arp.h:41
  ARPHRD_EUI64 : constant := 27;  --  /usr/include/linux/if_arp.h:42
  ARPHRD_INFINIBAND : constant := 32;  --  /usr/include/linux/if_arp.h:43

  ARPHRD_SLIP : constant := 256;  --  /usr/include/linux/if_arp.h:46
  ARPHRD_CSLIP : constant := 257;  --  /usr/include/linux/if_arp.h:47
  ARPHRD_SLIP6 : constant := 258;  --  /usr/include/linux/if_arp.h:48
  ARPHRD_CSLIP6 : constant := 259;  --  /usr/include/linux/if_arp.h:49
  ARPHRD_RSRVD : constant := 260;  --  /usr/include/linux/if_arp.h:50
  ARPHRD_ADAPT : constant := 264;  --  /usr/include/linux/if_arp.h:51
  ARPHRD_ROSE : constant := 270;  --  /usr/include/linux/if_arp.h:52
  ARPHRD_X25 : constant := 271;  --  /usr/include/linux/if_arp.h:53
  ARPHRD_HWX25 : constant := 272;  --  /usr/include/linux/if_arp.h:54
  ARPHRD_CAN : constant := 280;  --  /usr/include/linux/if_arp.h:55
  ARPHRD_PPP : constant := 512;  --  /usr/include/linux/if_arp.h:56
  ARPHRD_CISCO : constant := 513;  --  /usr/include/linux/if_arp.h:57
  --  unsupported macro: ARPHRD_HDLC ARPHRD_CISCO

  ARPHRD_LAPB : constant := 516;  --  /usr/include/linux/if_arp.h:59
  ARPHRD_DDCMP : constant := 517;  --  /usr/include/linux/if_arp.h:60
  ARPHRD_RAWHDLC : constant := 518;  --  /usr/include/linux/if_arp.h:61

  ARPHRD_TUNNEL : constant := 768;  --  /usr/include/linux/if_arp.h:63
  ARPHRD_TUNNEL6 : constant := 769;  --  /usr/include/linux/if_arp.h:64
  ARPHRD_FRAD : constant := 770;  --  /usr/include/linux/if_arp.h:65
  ARPHRD_SKIP : constant := 771;  --  /usr/include/linux/if_arp.h:66
  ARPHRD_LOOPBACK : constant := 772;  --  /usr/include/linux/if_arp.h:67
  ARPHRD_LOCALTLK : constant := 773;  --  /usr/include/linux/if_arp.h:68
  ARPHRD_FDDI : constant := 774;  --  /usr/include/linux/if_arp.h:69
  ARPHRD_BIF : constant := 775;  --  /usr/include/linux/if_arp.h:70
  ARPHRD_SIT : constant := 776;  --  /usr/include/linux/if_arp.h:71
  ARPHRD_IPDDP : constant := 777;  --  /usr/include/linux/if_arp.h:72
  ARPHRD_IPGRE : constant := 778;  --  /usr/include/linux/if_arp.h:73
  ARPHRD_PIMREG : constant := 779;  --  /usr/include/linux/if_arp.h:74
  ARPHRD_HIPPI : constant := 780;  --  /usr/include/linux/if_arp.h:75
  ARPHRD_ASH : constant := 781;  --  /usr/include/linux/if_arp.h:76
  ARPHRD_ECONET : constant := 782;  --  /usr/include/linux/if_arp.h:77
  ARPHRD_IRDA : constant := 783;  --  /usr/include/linux/if_arp.h:78

  ARPHRD_FCPP : constant := 784;  --  /usr/include/linux/if_arp.h:80
  ARPHRD_FCAL : constant := 785;  --  /usr/include/linux/if_arp.h:81
  ARPHRD_FCPL : constant := 786;  --  /usr/include/linux/if_arp.h:82
  ARPHRD_FCFABRIC : constant := 787;  --  /usr/include/linux/if_arp.h:83

  ARPHRD_IEEE802_TR : constant := 800;  --  /usr/include/linux/if_arp.h:85
  ARPHRD_IEEE80211 : constant := 801;  --  /usr/include/linux/if_arp.h:86
  ARPHRD_IEEE80211_PRISM : constant := 802;  --  /usr/include/linux/if_arp.h:87
  ARPHRD_IEEE80211_RADIOTAP : constant := 803;  --  /usr/include/linux/if_arp.h:88

  ARPHRD_VOID : constant := 16#FFFF#;  --  /usr/include/linux/if_arp.h:90
  ARPHRD_NONE : constant := 16#FFFE#;  --  /usr/include/linux/if_arp.h:91

  ARPOP_REQUEST : constant := 1;  --  /usr/include/linux/if_arp.h:94
  ARPOP_REPLY : constant := 2;  --  /usr/include/linux/if_arp.h:95
  ARPOP_RREQUEST : constant := 3;  --  /usr/include/linux/if_arp.h:96
  ARPOP_RREPLY : constant := 4;  --  /usr/include/linux/if_arp.h:97
  ARPOP_InREQUEST : constant := 8;  --  /usr/include/linux/if_arp.h:98
  ARPOP_InREPLY : constant := 9;  --  /usr/include/linux/if_arp.h:99
  ARPOP_NAK : constant := 10;  --  /usr/include/linux/if_arp.h:100

  ATF_COM : constant := 16#02#;  --  /usr/include/linux/if_arp.h:120
  ATF_PERM : constant := 16#04#;  --  /usr/include/linux/if_arp.h:121
  ATF_PUBL : constant := 16#08#;  --  /usr/include/linux/if_arp.h:122
  ATF_USETRAILERS : constant := 16#10#;  --  /usr/include/linux/if_arp.h:123
  ATF_NETMASK : constant := 16#20#;  --  /usr/include/linux/if_arp.h:124

  ATF_DONTPUB : constant := 16#40#;  --  /usr/include/linux/if_arp.h:126

  -- * INET		An implementation of the TCP/IP protocol suite for the LINUX
  -- *		operating system.  INET is implemented using the  BSD Socket
  -- *		interface as the means of communication with the user level.
  -- *
  -- *		Global definitions for the ARP (RFC 826) protocol.
  -- *
  -- * Version:	@(#)if_arp.h	1.0.1	04/16/93
  -- *
  -- * Authors:	Original taken from Berkeley UNIX 4.3, (c) UCB 1986-1988
  -- *		Portions taken from the KA9Q/NOS (v2.00m PA0GRI) source.
  -- *		Ross Biro
  -- *		Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
  -- *		Florian La Roche,
  -- *		Jonathan Layes <layes@loran.com>
  -- *		Arnaldo Carvalho de Melo <acme@conectiva.com.br> ARPHRD_HWX25
  -- *
  -- *		This program is free software; you can redistribute it and/or
  -- *		modify it under the terms of the GNU General Public License
  -- *		as published by the Free Software Foundation; either version
  -- *		2 of the License, or (at your option) any later version.
  --  

  -- ARP protocol HARDWARE identifiers.  
  -- Dummy types for non ARP hardware  
  -- ARP works differently on different FC media .. so   
  -- 787->799 reserved for fibrechannel media types  
  -- ARP protocol opcodes.  
  -- ARP ioctl request.  
  -- protocol address		 
   type arpreq_arp_dev_array is array (0 .. 15) of aliased char;
   type arpreq is record
      arp_pa : aliased bits_socket_h.sockaddr;  -- /usr/include/linux/if_arp.h:105:19
      arp_ha : aliased bits_socket_h.sockaddr;  -- /usr/include/linux/if_arp.h:106:19
      arp_flags : aliased int;  -- /usr/include/linux/if_arp.h:107:9
      arp_netmask : aliased bits_socket_h.sockaddr;  -- /usr/include/linux/if_arp.h:108:25
      arp_dev : aliased arpreq_arp_dev_array;  -- /usr/include/linux/if_arp.h:109:20
   end record;
   pragma Convention (C, arpreq);  -- /usr/include/linux/if_arp.h:104:15

  -- hardware address		 
  -- flags			 
  -- netmask (only for proxy arps)  
  -- protocol address		 
   type arpreq_old is record
      arp_pa : aliased bits_socket_h.sockaddr;  -- /usr/include/linux/if_arp.h:113:19
      arp_ha : aliased bits_socket_h.sockaddr;  -- /usr/include/linux/if_arp.h:114:19
      arp_flags : aliased int;  -- /usr/include/linux/if_arp.h:115:9
      arp_netmask : aliased bits_socket_h.sockaddr;  -- /usr/include/linux/if_arp.h:116:25
   end record;
   pragma Convention (C, arpreq_old);  -- /usr/include/linux/if_arp.h:112:19

  -- hardware address		 
  -- flags			 
  -- netmask (only for proxy arps)  
  -- ARP Flag values.  
  -- *	This structure defines an ethernet arp header.
  --  

  -- format of hardware address	 
   type arphdr is record
      ar_hrd : aliased linux_types_h.uu_be16;  -- /usr/include/linux/if_arp.h:134:10
      ar_pro : aliased linux_types_h.uu_be16;  -- /usr/include/linux/if_arp.h:135:10
      ar_hln : aliased unsigned_char;  -- /usr/include/linux/if_arp.h:136:16
      ar_pln : aliased unsigned_char;  -- /usr/include/linux/if_arp.h:137:16
      ar_op : aliased linux_types_h.uu_be16;  -- /usr/include/linux/if_arp.h:138:10
   end record;
   pragma Convention (C, arphdr);  -- /usr/include/linux/if_arp.h:133:1

  -- format of protocol address	 
  -- length of hardware address	 
  -- length of protocol address	 
  -- ARP opcode (command)		 
  --	  *	 Ethernet looks like this : This bit is variable sized however...
  --	   

  -- sender hardware address	 
  -- sender IP address		 
  -- target hardware address	 
  -- target IP address		 
end linux_if_arp_h;
