with Interfaces.C; use Interfaces.C;
limited with linux_hdlc_ioctl_h;
with bits_socket_h;
with System;
with Interfaces.C.Strings;

package linux_if_h is


  IFNAMSIZ : constant := 16;  --  /usr/include/linux/if.h:26
  IFALIASZ : constant := 256;  --  /usr/include/linux/if.h:27

  IFF_UP : constant := 16#1#;  --  /usr/include/linux/if.h:31
  IFF_BROADCAST : constant := 16#2#;  --  /usr/include/linux/if.h:32
  IFF_DEBUG : constant := 16#4#;  --  /usr/include/linux/if.h:33
  IFF_LOOPBACK : constant := 16#8#;  --  /usr/include/linux/if.h:34
  IFF_POINTOPOINT : constant := 16#10#;  --  /usr/include/linux/if.h:35
  IFF_NOTRAILERS : constant := 16#20#;  --  /usr/include/linux/if.h:36
  IFF_RUNNING : constant := 16#40#;  --  /usr/include/linux/if.h:37
  IFF_NOARP : constant := 16#80#;  --  /usr/include/linux/if.h:38
  IFF_PROMISC : constant := 16#100#;  --  /usr/include/linux/if.h:39
  IFF_ALLMULTI : constant := 16#200#;  --  /usr/include/linux/if.h:40

  IFF_MASTER : constant := 16#400#;  --  /usr/include/linux/if.h:42
  IFF_SLAVE : constant := 16#800#;  --  /usr/include/linux/if.h:43

  IFF_MULTICAST : constant := 16#1000#;  --  /usr/include/linux/if.h:45

  IFF_PORTSEL : constant := 16#2000#;  --  /usr/include/linux/if.h:47
  IFF_AUTOMEDIA : constant := 16#4000#;  --  /usr/include/linux/if.h:48
  IFF_DYNAMIC : constant := 16#8000#;  --  /usr/include/linux/if.h:49

  IFF_LOWER_UP : constant := 16#10000#;  --  /usr/include/linux/if.h:51
  IFF_DORMANT : constant := 16#20000#;  --  /usr/include/linux/if.h:52

  IFF_ECHO : constant := 16#40000#;  --  /usr/include/linux/if.h:54
  --  unsupported macro: IFF_VOLATILE (IFF_LOOPBACK|IFF_POINTOPOINT|IFF_BROADCAST|IFF_ECHO| IFF_MASTER|IFF_SLAVE|IFF_RUNNING|IFF_LOWER_UP|IFF_DORMANT)

  IFF_802_1Q_VLAN : constant := 16#1#;  --  /usr/include/linux/if.h:60
  IFF_EBRIDGE : constant := 16#2#;  --  /usr/include/linux/if.h:61
  IFF_SLAVE_INACTIVE : constant := 16#4#;  --  /usr/include/linux/if.h:62
  IFF_MASTER_8023AD : constant := 16#8#;  --  /usr/include/linux/if.h:63
  IFF_MASTER_ALB : constant := 16#10#;  --  /usr/include/linux/if.h:64
  IFF_BONDING : constant := 16#20#;  --  /usr/include/linux/if.h:65
  IFF_SLAVE_NEEDARP : constant := 16#40#;  --  /usr/include/linux/if.h:66
  IFF_ISATAP : constant := 16#80#;  --  /usr/include/linux/if.h:67

  IF_GET_IFACE : constant := 16#0001#;  --  /usr/include/linux/if.h:69
  IF_GET_PROTO : constant := 16#0002#;  --  /usr/include/linux/if.h:70

  IF_IFACE_V35 : constant := 16#1000#;  --  /usr/include/linux/if.h:73
  IF_IFACE_V24 : constant := 16#1001#;  --  /usr/include/linux/if.h:74
  IF_IFACE_X21 : constant := 16#1002#;  --  /usr/include/linux/if.h:75
  IF_IFACE_T1 : constant := 16#1003#;  --  /usr/include/linux/if.h:76
  IF_IFACE_E1 : constant := 16#1004#;  --  /usr/include/linux/if.h:77
  IF_IFACE_SYNC_SERIAL : constant := 16#1005#;  --  /usr/include/linux/if.h:78
  IF_IFACE_X21D : constant := 16#1006#;  --  /usr/include/linux/if.h:79

  IF_PROTO_HDLC : constant := 16#2000#;  --  /usr/include/linux/if.h:82
  IF_PROTO_PPP : constant := 16#2001#;  --  /usr/include/linux/if.h:83
  IF_PROTO_CISCO : constant := 16#2002#;  --  /usr/include/linux/if.h:84
  IF_PROTO_FR : constant := 16#2003#;  --  /usr/include/linux/if.h:85
  IF_PROTO_FR_ADD_PVC : constant := 16#2004#;  --  /usr/include/linux/if.h:86
  IF_PROTO_FR_DEL_PVC : constant := 16#2005#;  --  /usr/include/linux/if.h:87
  IF_PROTO_X25 : constant := 16#2006#;  --  /usr/include/linux/if.h:88
  IF_PROTO_HDLC_ETH : constant := 16#2007#;  --  /usr/include/linux/if.h:89
  IF_PROTO_FR_ADD_ETH_PVC : constant := 16#2008#;  --  /usr/include/linux/if.h:90
  IF_PROTO_FR_DEL_ETH_PVC : constant := 16#2009#;  --  /usr/include/linux/if.h:91
  IF_PROTO_FR_PVC : constant := 16#200A#;  --  /usr/include/linux/if.h:92
  IF_PROTO_FR_ETH_PVC : constant := 16#200B#;  --  /usr/include/linux/if.h:93
  IF_PROTO_RAW : constant := 16#200C#;  --  /usr/include/linux/if.h:94

  IFHWADDRLEN : constant := 6;  --  /usr/include/linux/if.h:161
  --  unsupported macro: ifr_name ifr_ifrn.ifrn_name
  --  unsupported macro: ifr_hwaddr ifr_ifru.ifru_hwaddr
  --  unsupported macro: ifr_addr ifr_ifru.ifru_addr
  --  unsupported macro: ifr_dstaddr ifr_ifru.ifru_dstaddr
  --  unsupported macro: ifr_broadaddr ifr_ifru.ifru_broadaddr
  --  unsupported macro: ifr_netmask ifr_ifru.ifru_netmask
  --  unsupported macro: ifr_flags ifr_ifru.ifru_flags
  --  unsupported macro: ifr_metric ifr_ifru.ifru_ivalue
  --  unsupported macro: ifr_mtu ifr_ifru.ifru_mtu
  --  unsupported macro: ifr_map ifr_ifru.ifru_map
  --  unsupported macro: ifr_slave ifr_ifru.ifru_slave
  --  unsupported macro: ifr_data ifr_ifru.ifru_data
  --  unsupported macro: ifr_ifindex ifr_ifru.ifru_ivalue
  --  unsupported macro: ifr_bandwidth ifr_ifru.ifru_ivalue
  --  unsupported macro: ifr_qlen ifr_ifru.ifru_ivalue
  --  unsupported macro: ifr_newname ifr_ifru.ifru_newname
  --  unsupported macro: ifr_settings ifr_ifru.ifru_settings
  --  unsupported macro: ifc_buf ifc_ifcu.ifcu_buf
  --  unsupported macro: ifc_req ifc_ifcu.ifcu_req

  -- * INET		An implementation of the TCP/IP protocol suite for the LINUX
  -- *		operating system.  INET is implemented using the  BSD Socket
  -- *		interface as the means of communication with the user level.
  -- *
  -- *		Global definitions for the INET interface module.
  -- *
  -- * Version:	@(#)if.h	1.0.2	04/18/93
  -- *
  -- * Authors:	Original taken from Berkeley UNIX 4.3, (c) UCB 1982-1988
  -- *		Ross Biro
  -- *		Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
  -- *
  -- *		This program is free software; you can redistribute it and/or
  -- *		modify it under the terms of the GNU General Public License
  -- *		as published by the Free Software Foundation; either version
  -- *		2 of the License, or (at your option) any later version.
  --

  -- for "__kernel_caddr_t" et al
  -- for "struct sockaddr" et al
  -- for "__user" et al
  -- Standard interface flags (netdevice->flags).
  -- Private (from user) interface flags (netdevice->priv_flags).
  -- For definitions see hdlc.h
  -- For definitions see hdlc.h
  -- RFC 2863 operational status
  -- link modes
  -- limit upward transition to dormant
  -- *	Device mapping structure. I'd just gone off and designed a
  -- *	beautiful scheme using only loadable modules with arguments
  -- *	for driver options and along come the PCMCIA people 8)
  -- *
  -- *	Ah well. The get() side of this is good for WDSETUP, and it'll
  -- *	be handy for debugging things. The set side is fine for now and
  -- *	being very small might be worth keeping for clean configuration.
  --

   type ifmap is record
      mem_start : aliased unsigned_long;  -- /usr/include/linux/if.h:125:16
      mem_end : aliased unsigned_long;  -- /usr/include/linux/if.h:126:16
      base_addr : aliased unsigned_short;  -- /usr/include/linux/if.h:127:17
      irq : aliased unsigned_char;  -- /usr/include/linux/if.h:128:16
      dma : aliased unsigned_char;  -- /usr/include/linux/if.h:129:16
      port : aliased unsigned_char;  -- /usr/include/linux/if.h:130:16
   end record;
   pragma Convention (C, ifmap);  -- /usr/include/linux/if.h:124:1

  -- 3 bytes spare
  -- Type of physical device or protocol
   type anon_29 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
         raw_hdlc : access linux_hdlc_ioctl_h.raw_hdlc_proto;  -- /usr/include/linux/if.h:140:20
         when 1 =>
         cisco : access linux_hdlc_ioctl_h.cisco_proto;  -- /usr/include/linux/if.h:141:17
         when 2 =>
         fr : access linux_hdlc_ioctl_h.fr_proto;  -- /usr/include/linux/if.h:142:14
         when 3 =>
         fr_pvc : access linux_hdlc_ioctl_h.fr_proto_pvc;  -- /usr/include/linux/if.h:143:18
         when 4 =>
         fr_pvc_info : access linux_hdlc_ioctl_h.fr_proto_pvc_info;  -- /usr/include/linux/if.h:144:22
         when 5 =>
         sync : access linux_hdlc_ioctl_h.sync_serial_settings;  -- /usr/include/linux/if.h:147:25
         when others =>
         te1 : access linux_hdlc_ioctl_h.te1_settings;  -- /usr/include/linux/if.h:148:18
      end case;
   end record;
   pragma Convention (C, anon_29);
   pragma Unchecked_Union (anon_29);
   type if_settings is record
      c_type : aliased unsigned;  -- /usr/include/linux/if.h:136:15
      size : aliased unsigned;  -- /usr/include/linux/if.h:137:15
      ifs_ifsu : aliased anon_29;  -- /usr/include/linux/if.h:149:4
   end record;
   pragma Convention (C, if_settings);  -- /usr/include/linux/if.h:135:1

  -- Size of the data allocated by the caller
  -- {atm/eth/dsl}_settings anyone ?
  -- interface settings
  -- * Interface request structure used for socket
  -- * ioctl's.  All interface ioctl's must have parameter
  -- * definitions which begin with ifr_name.  The
  -- * remainder may be interface specific.
  --

  -- if name, e.g. "en0"
   type ifreq_ifrn_name_array is array (0 .. 15) of aliased char;
   type anon_30 (discr : unsigned := 0) is record
      case discr is
         when others =>
         ifrn_name : aliased ifreq_ifrn_name_array;  -- /usr/include/linux/if.h:164:26
      end case;
   end record;
   pragma Convention (C, anon_30);
   pragma Unchecked_Union (anon_30);
   type ifreq_ifru_slave_array is array (0 .. 15) of aliased char;
   type ifreq_ifru_newname_array is array (0 .. 15) of aliased char;
   type anon_31 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
         ifru_addr : aliased bits_socket_h.sockaddr;  -- /usr/include/linux/if.h:168:19
         when 1 =>
         ifru_dstaddr : aliased bits_socket_h.sockaddr;  -- /usr/include/linux/if.h:169:19
         when 2 =>
         ifru_broadaddr : aliased bits_socket_h.sockaddr;  -- /usr/include/linux/if.h:170:19
         when 3 =>
         ifru_netmask : aliased bits_socket_h.sockaddr;  -- /usr/include/linux/if.h:171:19
         when 4 =>
         ifru_hwaddr : aliased bits_socket_h.sockaddr;  -- /usr/include/linux/if.h:172:20
         when 5 =>
         ifru_flags : aliased short;  -- /usr/include/linux/if.h:173:9
         when 6 =>
         ifru_ivalue : aliased int;  -- /usr/include/linux/if.h:174:7
         when 7 =>
         ifru_mtu : aliased int;  -- /usr/include/linux/if.h:175:7
         when 8 =>
         ifru_map : aliased ifmap;  -- /usr/include/linux/if.h:176:17
         when 9 =>
         ifru_slave : aliased ifreq_ifru_slave_array;  -- /usr/include/linux/if.h:177:27
         when 10 =>
         ifru_newname : aliased ifreq_ifru_newname_array;  -- /usr/include/linux/if.h:178:29
         when 11 =>
         ifru_data : System.Address;  -- /usr/include/linux/if.h:179:10
         when others =>
         ifru_settings : aliased if_settings;  -- /usr/include/linux/if.h:180:22
      end case;
   end record;
   pragma Convention (C, anon_31);
   pragma Unchecked_Union (anon_31);
   type ifreq is record
      ifr_ifrn : aliased anon_30;  -- /usr/include/linux/if.h:165:4
      ifr_ifru : aliased anon_31;  -- /usr/include/linux/if.h:181:4
   end record;
   pragma Convention (C, ifreq);  -- /usr/include/linux/if.h:160:1

   type Ifreq_Ptr is access all Ifreq;
  -- Just fits the size
  -- * Structure used in SIOCGIFCONF request.
  -- * Used to retrieve interface configuration
  -- * for machine (useful for programs which
  -- * must know all networks accessible).
  --

  -- size of buffer
   type anon_32 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
         ifcu_buf : Interfaces.C.Strings.chars_ptr;  -- /usr/include/linux/if.h:214:9
         when others =>
         ifcu_req : access ifreq;  -- /usr/include/linux/if.h:215:17
      end case;
   end record;
   pragma Convention (C, anon_32);
   pragma Unchecked_Union (anon_32);
   type ifconf is record
      ifc_len : aliased int;  -- /usr/include/linux/if.h:211:6
      ifc_ifcu : aliased anon_32;  -- /usr/include/linux/if.h:216:4
   end record;
   pragma Convention (C, ifconf);  -- /usr/include/linux/if.h:210:1

end linux_if_h;
