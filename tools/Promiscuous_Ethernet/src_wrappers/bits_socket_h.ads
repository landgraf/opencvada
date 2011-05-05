with Interfaces.C; use Interfaces.C;
with bits_types_h;
with bits_sockaddr_h;
with System;
limited with bits_uio_h;
with stddef_h;
with sys_types_h;

package bits_socket_h is

  --  unsupported macro: SOCK_STREAM SOCK_STREAM
  --  unsupported macro: SOCK_DGRAM SOCK_DGRAM
  --  unsupported macro: SOCK_RAW SOCK_RAW
  --  unsupported macro: SOCK_RDM SOCK_RDM
  --  unsupported macro: SOCK_SEQPACKET SOCK_SEQPACKET
  --  unsupported macro: SOCK_DCCP SOCK_DCCP
  --  unsupported macro: SOCK_PACKET SOCK_PACKET
  --  unsupported macro: SOCK_CLOEXEC SOCK_CLOEXEC
  --  unsupported macro: SOCK_NONBLOCK SOCK_NONBLOCK

  PF_UNSPEC : constant := 0;  --  /usr/include/bits/socket.h:74
  PF_LOCAL : constant := 1;  --  /usr/include/bits/socket.h:75
  --  unsupported macro: PF_UNIX PF_LOCAL
  --  unsupported macro: PF_FILE PF_LOCAL

  PF_INET : constant := 2;  --  /usr/include/bits/socket.h:78
  PF_AX25 : constant := 3;  --  /usr/include/bits/socket.h:79
  PF_IPX : constant := 4;  --  /usr/include/bits/socket.h:80
  PF_APPLETALK : constant := 5;  --  /usr/include/bits/socket.h:81
  PF_NETROM : constant := 6;  --  /usr/include/bits/socket.h:82
  PF_BRIDGE : constant := 7;  --  /usr/include/bits/socket.h:83
  PF_ATMPVC : constant := 8;  --  /usr/include/bits/socket.h:84
  PF_X25 : constant := 9;  --  /usr/include/bits/socket.h:85
  PF_INET6 : constant := 10;  --  /usr/include/bits/socket.h:86
  PF_ROSE : constant := 11;  --  /usr/include/bits/socket.h:87
  PF_DECnet : constant := 12;  --  /usr/include/bits/socket.h:88
  PF_NETBEUI : constant := 13;  --  /usr/include/bits/socket.h:89
  PF_SECURITY : constant := 14;  --  /usr/include/bits/socket.h:90
  PF_KEY : constant := 15;  --  /usr/include/bits/socket.h:91
  PF_NETLINK : constant := 16;  --  /usr/include/bits/socket.h:92
  --  unsupported macro: PF_ROUTE PF_NETLINK

  PF_PACKET : constant := 17;  --  /usr/include/bits/socket.h:94
  PF_ASH : constant := 18;  --  /usr/include/bits/socket.h:95
  PF_ECONET : constant := 19;  --  /usr/include/bits/socket.h:96
  PF_ATMSVC : constant := 20;  --  /usr/include/bits/socket.h:97
  PF_SNA : constant := 22;  --  /usr/include/bits/socket.h:98
  PF_IRDA : constant := 23;  --  /usr/include/bits/socket.h:99
  PF_PPPOX : constant := 24;  --  /usr/include/bits/socket.h:100
  PF_WANPIPE : constant := 25;  --  /usr/include/bits/socket.h:101
  PF_BLUETOOTH : constant := 31;  --  /usr/include/bits/socket.h:102
  PF_IUCV : constant := 32;  --  /usr/include/bits/socket.h:103
  PF_RXRPC : constant := 33;  --  /usr/include/bits/socket.h:104
  PF_ISDN : constant := 34;  --  /usr/include/bits/socket.h:105
  PF_MAX : constant := 35;  --  /usr/include/bits/socket.h:106
  --  unsupported macro: AF_UNSPEC PF_UNSPEC
  --  unsupported macro: AF_LOCAL PF_LOCAL
  --  unsupported macro: AF_UNIX PF_UNIX
  --  unsupported macro: AF_FILE PF_FILE
  --  unsupported macro: AF_INET PF_INET
  --  unsupported macro: AF_AX25 PF_AX25
  --  unsupported macro: AF_IPX PF_IPX
  --  unsupported macro: AF_APPLETALK PF_APPLETALK
  --  unsupported macro: AF_NETROM PF_NETROM
  --  unsupported macro: AF_BRIDGE PF_BRIDGE
  --  unsupported macro: AF_ATMPVC PF_ATMPVC
  --  unsupported macro: AF_X25 PF_X25
  --  unsupported macro: AF_INET6 PF_INET6
  --  unsupported macro: AF_ROSE PF_ROSE
  --  unsupported macro: AF_DECnet PF_DECnet
  --  unsupported macro: AF_NETBEUI PF_NETBEUI
  --  unsupported macro: AF_SECURITY PF_SECURITY
  --  unsupported macro: AF_KEY PF_KEY
  --  unsupported macro: AF_NETLINK PF_NETLINK
  --  unsupported macro: AF_ROUTE PF_ROUTE
  --  unsupported macro: AF_PACKET PF_PACKET
  --  unsupported macro: AF_ASH PF_ASH
  --  unsupported macro: AF_ECONET PF_ECONET
  --  unsupported macro: AF_ATMSVC PF_ATMSVC
  --  unsupported macro: AF_SNA PF_SNA
  --  unsupported macro: AF_IRDA PF_IRDA
  --  unsupported macro: AF_PPPOX PF_PPPOX
  --  unsupported macro: AF_WANPIPE PF_WANPIPE
  --  unsupported macro: AF_BLUETOOTH PF_BLUETOOTH
  --  unsupported macro: AF_IUCV PF_IUCV
  --  unsupported macro: AF_RXRPC PF_RXRPC
  --  unsupported macro: AF_ISDN PF_ISDN
  --  unsupported macro: AF_MAX PF_MAX

  SOL_RAW : constant := 255;  --  /usr/include/bits/socket.h:147
  SOL_DECNET : constant := 261;  --  /usr/include/bits/socket.h:148
  SOL_X25 : constant := 262;  --  /usr/include/bits/socket.h:149
  SOL_PACKET : constant := 263;  --  /usr/include/bits/socket.h:150
  SOL_ATM : constant := 264;  --  /usr/include/bits/socket.h:151
  SOL_AAL : constant := 265;  --  /usr/include/bits/socket.h:152
  SOL_IRDA : constant := 266;  --  /usr/include/bits/socket.h:153

  SOMAXCONN : constant := 128;  --  /usr/include/bits/socket.h:156
  --  unsupported macro: MSG_OOB MSG_OOB
  --  unsupported macro: MSG_PEEK MSG_PEEK
  --  unsupported macro: MSG_DONTROUTE MSG_DONTROUTE
  --  unsupported macro: MSG_TRYHARD MSG_DONTROUTE
  --  unsupported macro: MSG_CTRUNC MSG_CTRUNC
  --  unsupported macro: MSG_PROXY MSG_PROXY
  --  unsupported macro: MSG_TRUNC MSG_TRUNC
  --  unsupported macro: MSG_DONTWAIT MSG_DONTWAIT
  --  unsupported macro: MSG_EOR MSG_EOR
				--  unsupported macro: MSG_WAITALL MSG_WAITALL
   MSG_WAITALL : constant := 16#100#;

  --  unsupported macro: MSG_FIN MSG_FIN
  --  unsupported macro: MSG_SYN MSG_SYN
  --  unsupported macro: MSG_CONFIRM MSG_CONFIRM
  --  unsupported macro: MSG_RST MSG_RST
  --  unsupported macro: MSG_ERRQUEUE MSG_ERRQUEUE
  --  unsupported macro: MSG_NOSIGNAL MSG_NOSIGNAL
  --  unsupported macro: MSG_MORE MSG_MORE
  --  unsupported macro: MSG_CMSG_CLOEXEC MSG_CMSG_CLOEXEC
  --  unsupported macro: CMSG_DATA(cmsg) ((cmsg)->__cmsg_data)
  --  unsupported macro: CMSG_NXTHDR(mhdr,cmsg) __cmsg_nxthdr (mhdr, cmsg)
  --  unsupported macro: CMSG_FIRSTHDR(mhdr) ((size_t) (mhdr)->msg_controllen >= sizeof (struct cmsghdr) ? (struct cmsghdr *) (mhdr)->msg_control : (struct cmsghdr *) 0)
  --  arg-macro: function CMSG_ALIGN (len)
  --    return ((len) + sizeof (size_t) - 1) and (size_t) ~(sizeof (size_t) - 1);
  --  arg-macro: function CMSG_SPACE (len)
  --    return CMSG_ALIGN (len) + CMSG_ALIGN (sizeof (struct cmsghdr));
  --  arg-macro: function CMSG_LEN (len)
  --    return CMSG_ALIGN (sizeof (struct cmsghdr)) + (len);
  --  unsupported macro: SCM_RIGHTS SCM_RIGHTS
  --  unsupported macro: SCM_CREDENTIALS SCM_CREDENTIALS

  -- System-specific socket constants and types.  Linux version.
  --   Copyright (C) 1991, 1992, 1994-2001, 2004, 2006, 2007, 2008
  --   Free Software Foundation, Inc.
  --   This file is part of the GNU C Library.
  --   The GNU C Library is free software; you can redistribute it and/or
  --   modify it under the terms of the GNU Lesser General Public
  --   License as published by the Free Software Foundation; either
  --   version 2.1 of the License, or (at your option) any later version.
  --   The GNU C Library is distributed in the hope that it will be useful,
  --   but WITHOUT ANY WARRANTY; without even the implied warranty of
  --   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  --   Lesser General Public License for more details.
  --   You should have received a copy of the GNU Lesser General Public
  --   License along with the GNU C Library; if not, write to the Free
  --   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
  --   02111-1307 USA.

  -- Type for length arguments in socket calls.
   subtype socklen_t is bits_types_h.uu_socklen_t;  -- /usr/include/bits/socket.h:35:21

  -- Types of sockets.
   subtype uu_socket_type is unsigned;
   SOCK_STREAM : constant uu_socket_type := 1;
   SOCK_DGRAM : constant uu_socket_type := 2;
   SOCK_RAW : constant uu_socket_type := 3;
   SOCK_RDM : constant uu_socket_type := 4;
   SOCK_SEQPACKET : constant uu_socket_type := 5;
   SOCK_DCCP : constant uu_socket_type := 6;
   SOCK_PACKET : constant uu_socket_type := 10;
   SOCK_CLOEXEC : constant uu_socket_type := 524288;
   SOCK_NONBLOCK : constant uu_socket_type := 2048;  -- /usr/include/bits/socket.h:40:6

  -- Sequenced, reliable, connection-based
  --				   byte streams.

  -- Connectionless, unreliable datagrams
  --				   of fixed maximum length.

  -- Raw protocol interface.
  -- Reliably-delivered messages.
  -- Sequenced, reliable, connection-based,
  --				   datagrams of fixed maximum length.

  -- Datagram Congestion Control Protocol.
  -- Linux specific way of getting packets
  --				   at the dev level.  For writing rarp and
  --				   other similar things on the user level.

  -- Flags to be ORed into the type parameter of socket and socketpair and
  --     used for the flags parameter of paccept.

  -- Atomically set close-on-exec flag for the
  --				   new descriptor(s).

  -- Atomically mark descriptor(s) as
  --				   non-blocking.

  -- Protocol families.
  -- Address families.
  -- Socket level values.  Others are defined in the appropriate headers.
  --   XXX These definitions also should go into the appropriate headers as
  --   far as they are available.

  -- Maximum queue length specifiable by listen.
  -- Get the definition of the macro to define the common sockaddr members.
  -- Structure describing a generic socket address.
  -- Common data: address family and length.
   type sockaddr_sa_data_array is array (0 .. 13) of aliased char;
   type sockaddr is record
      sa_family : aliased bits_sockaddr_h.sa_family_t;  -- /usr/include/bits/socket.h:164:1
      sa_data : aliased sockaddr_sa_data_array;  -- /usr/include/bits/socket.h:165:20
   end record;
   pragma Convention (C, sockaddr);  -- /usr/include/bits/socket.h:163:3

  -- Address data.
  -- Structure large enough to hold any socket address (with the historical
  --   exception of AF_UNIX).  We reserve 128 bytes.

  -- Address family, etc.
   type sockaddr_storage_uu_ss_padding_array is array (0 .. 119) of aliased char;
   type sockaddr_storage is record
      ss_family : aliased bits_sockaddr_h.sa_family_t;  -- /usr/include/bits/socket.h:177:1
      uu_ss_align : aliased unsigned_long;  -- /usr/include/bits/socket.h:178:20
      uu_ss_padding : aliased sockaddr_storage_uu_ss_padding_array;  -- /usr/include/bits/socket.h:179:34
   end record;
   pragma Convention (C, sockaddr_storage);  -- /usr/include/bits/socket.h:176:3

  -- Force desired alignment.
  -- Bits in the FLAGS argument to `send', `recv', et al.
  -- Process out-of-band data.
  -- Peek at incoming messages.
  -- Don't use local routing.
  -- DECnet uses a different name.
  -- Control data lost before delivery.
  -- Supply or ask second address.
  -- Nonblocking IO.
  -- End of record.
  -- Wait for a full request.
  -- Confirm path validity.
  -- Fetch message from error queue.
  -- Do not generate SIGPIPE.
  -- Sender will send more.
  -- Set close_on_exit for file
  --                                           descriptor received through
  --                                           SCM_RIGHTS.

  -- Structure describing messages sent by
  --   `sendmsg' and received by `recvmsg'.

  -- Address to send to/receive from.
   type msghdr is record
      msg_name : System.Address;  -- /usr/include/bits/socket.h:235:11
      msg_namelen : aliased socklen_t;  -- /usr/include/bits/socket.h:236:15
      msg_iov : access bits_uio_h.iovec;  -- /usr/include/bits/socket.h:238:19
      msg_iovlen : aliased stddef_h.size_t;  -- /usr/include/bits/socket.h:239:12
      msg_control : System.Address;  -- /usr/include/bits/socket.h:241:11
      msg_controllen : aliased stddef_h.size_t;  -- /usr/include/bits/socket.h:242:12
      msg_flags : aliased int;  -- /usr/include/bits/socket.h:247:9
   end record;
   pragma Convention (C, msghdr);  -- /usr/include/bits/socket.h:234:3

  -- Length of address data.
  -- Vector of data to send/receive into.
  -- Number of elements in the vector.
  -- Ancillary data (eg BSD filedesc passing).
  -- Ancillary data buffer length.
  --				   !! The type should be socklen_t but the
  --				   definition of the kernel is incompatible
  --				   with this.

  -- Flags on received message.
  -- Structure used for storage of ancillary data object information.
  -- Length of data in cmsg_data plus length
  --				   of cmsghdr structure.
  --				   !! The type should be socklen_t but the
  --				   definition of the kernel is incompatible
  --				   with this.

   type cmsghdr_uu_cmsg_data_array is array (0 .. -1) of aliased unsigned_char;
   type cmsghdr is record
      cmsg_len : aliased stddef_h.size_t;  -- /usr/include/bits/socket.h:253:12
      cmsg_level : aliased int;  -- /usr/include/bits/socket.h:258:9
      cmsg_type : aliased int;  -- /usr/include/bits/socket.h:259:9
      uu_cmsg_data : aliased cmsghdr_uu_cmsg_data_array;  -- /usr/include/bits/socket.h:261:45
   end record;
   pragma Convention (C, cmsghdr);  -- /usr/include/bits/socket.h:252:3

  -- Originating protocol.
  -- Protocol specific type.
  -- Ancillary data.
  -- Ancillary data object manipulation macros.
  -- The kernel header does this so there may be a reason.
  -- No more entries.
  -- Socket level message types.  This must match the definitions in
  --   <linux/socket.h>.

  -- Transfer file descriptors.
  -- Credentials passing.
  -- User visible structure for SCM_CREDENTIALS message
  -- PID of sending process.
   type ucred is record
      pid : aliased sys_types_h.pid_t;  -- /usr/include/bits/socket.h:322:9
      uid : aliased sys_types_h.uid_t;  -- /usr/include/bits/socket.h:323:9
      gid : aliased sys_types_h.gid_t;  -- /usr/include/bits/socket.h:324:9
   end record;
   pragma Convention (C, ucred);  -- /usr/include/bits/socket.h:321:1

  -- UID of sending process.
  -- GID of sending process.
  -- Ugly workaround for unclean kernel headers.
  -- Get socket manipulation related informations from kernel headers.
  -- Structure used to manipulate the SO_LINGER option.
  -- Nonzero to linger on close.
   type linger is record
      l_onoff : aliased int;  -- /usr/include/bits/socket.h:390:9
      l_linger : aliased int;  -- /usr/include/bits/socket.h:391:9
   end record;
   pragma Convention (C, linger);  -- /usr/include/bits/socket.h:389:3

  -- Time to linger.
end bits_socket_h;
