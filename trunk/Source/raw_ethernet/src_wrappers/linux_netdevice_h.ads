with Interfaces.C; use Interfaces.C;

package linux_netdevice_h is


  MAX_ADDR_LEN : constant := 32;  --  /usr/include/linux/netdevice.h:33

  NETDEV_TX_OK : constant := 0;  --  /usr/include/linux/netdevice.h:36
  NETDEV_TX_BUSY : constant := 1;  --  /usr/include/linux/netdevice.h:37
  NETDEV_TX_LOCKED : constant := -1;  --  /usr/include/linux/netdevice.h:38

  -- * INET		An implementation of the TCP/IP protocol suite for the LINUX
  -- *		operating system.  INET is implemented using the  BSD Socket
  -- *		interface as the means of communication with the user level.
  -- *
  -- *		Definitions for the Interfaces handler.
  -- *
  -- * Version:	@(#)dev.h	1.0.10	08/12/93
  -- *
  -- * Authors:	Ross Biro
  -- *		Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
  -- *		Corey Minyard <wf-rch!minyard@relay.EU.net>
  -- *		Donald J. Becker, <becker@cesdis.gsfc.nasa.gov>
  -- *		Alan Cox, <alan@lxorguk.ukuu.org.uk>
  -- *		Bjorn Ekwall. <bj0rn@blox.se>
  -- *              Pekka Riikonen <priikone@poseidon.pspt.fi>
  -- *
  -- *		This program is free software; you can redistribute it and/or
  -- *		modify it under the terms of the GNU General Public License
  -- *		as published by the Free Software Foundation; either version
  -- *		2 of the License, or (at your option) any later version.
  -- *
  -- *		Moved to /usr/include/linux for NET3
  --  

  -- Driver transmit return codes  
  -- *	Network device statistics. Akin to the 2.0 ether stats but
  -- *	with byte counters.
  --  

  -- total packets received	 
   type net_device_stats is record
      rx_packets : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:48:16
      tx_packets : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:49:16
      rx_bytes : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:50:16
      tx_bytes : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:51:16
      rx_errors : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:52:16
      tx_errors : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:53:16
      rx_dropped : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:54:16
      tx_dropped : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:55:16
      multicast : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:56:16
      collisions : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:57:16
      rx_length_errors : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:60:16
      rx_over_errors : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:61:16
      rx_crc_errors : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:62:16
      rx_frame_errors : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:63:16
      rx_fifo_errors : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:64:16
      rx_missed_errors : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:65:16
      tx_aborted_errors : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:68:16
      tx_carrier_errors : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:69:16
      tx_fifo_errors : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:70:16
      tx_heartbeat_errors : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:71:16
      tx_window_errors : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:72:16
      rx_compressed : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:75:16
      tx_compressed : aliased unsigned_long;  -- /usr/include/linux/netdevice.h:76:16
   end record;
   pragma Convention (C, net_device_stats);  -- /usr/include/linux/netdevice.h:47:1

  -- total packets transmitted	 
  -- total bytes received 	 
  -- total bytes transmitted	 
  -- bad packets received		 
  -- packet transmit problems	 
  -- no space in linux buffers	 
  -- no space available in linux	 
  -- multicast packets received	 
  -- detailed rx_errors:  
  -- receiver ring buff overflow	 
  -- recved pkt with crc error	 
  -- recv'd frame alignment error  
  -- recv'r fifo overrun		 
  -- receiver missed packet	 
  -- detailed tx_errors  
  -- for cslip etc  
  -- Media selection options.  
end linux_netdevice_h;
