with Interfaces.C; use Interfaces.C;
with bits_socket_h;
with System;
with stddef_h;
with sys_types_h;
with Linux_If_Packet_H;

generic
   type Buffer_Type_Ptr is private;
package sys_socket_h is

   ETH_FRAME_LEN : constant Integer := 1518;

   --     subtype Byte is Integer range 0 .. 255;
--     type Super_Buffer_Type is array (Positive range <>) of Byte;
--     type Buffer_Type is new Super_Buffer_Type (1 .. ETH_FRAME_LEN);
--     pragma Pack (Buffer_Type);
--     type Buffer_Type_Ptr is access all Buffer_Type;


  --  unsupported macro: SHUT_RD SHUT_RD
  --  unsupported macro: SHUT_WR SHUT_WR
  --  unsupported macro: SHUT_RDWR SHUT_RDWR
  -- Declarations of socket constants, types, and functions.
  --   Copyright (C) 1991,92,1994-2001,2003,2005,2007,2008
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

  -- Get the __sigset_t definition.
  -- This operating system-specific header file defines the SOCK_*, PF_*,
  --   AF_*, MSG_*, SOL_*, and SO_* constants, and the `struct sockaddr',
  --   `struct msghdr', and `struct linger' types.

  -- This is the 4.3 BSD `struct sockaddr' format, which is used as wire
  --   format in the grotty old 4.3 `talk' protocol.

   type osockaddr_sa_data_array is array (0 .. 13) of aliased unsigned_char;
   type osockaddr is record
      sa_family : aliased unsigned_short;  -- /usr/include/sys/socket.h:47:24
      sa_data : aliased osockaddr_sa_data_array;  -- /usr/include/sys/socket.h:48:29
   end record;
   pragma Convention (C, osockaddr);  -- /usr/include/sys/socket.h:46:3

  -- The following constants should be used for the second parameter of
  --   `shutdown'.

  -- No more receptions.
  -- No more transmissions.
  -- No more receptions or transmissions.
  -- This is the type we use for generic socket address arguments.
  --   With GCC 2.7 and later, the funky union causes redeclarations or
  --   uses with any of the listed types to be allowed without complaint.
  --   G++ 2.7 does not support transparent unions so there we want the
  --   old-style declaration, too.

  -- Add more `struct sockaddr_AF' types here as necessary.
  --   These are all the ones I found on NetBSD and Linux.

  -- Create a new socket of type TYPE in domain DOMAIN, using
  --   protocol PROTOCOL.  If PROTOCOL is zero, one is chosen automatically.
  --   Returns a file descriptor for the new socket, or -1 for errors.

   function socket
     (uu_domain : int;
      uu_type : int;
      uu_protocol : int) return int;  -- /usr/include/sys/socket.h:105:62
   pragma Import (C, socket, "socket");

  -- Create two new sockets, of type TYPE in domain DOMAIN and using
  --   protocol PROTOCOL, which are connected to each other, and put file
  --   descriptors for them in FDS[0] and FDS[1].  If PROTOCOL is zero,
  --   one will be chosen automatically.  Returns 0 on success, -1 for errors.

   function socketpair
     (uu_domain : int;
      uu_type : int;
      uu_protocol : int;
      uu_fds : access int) return int;  -- /usr/include/sys/socket.h:112:24
   pragma Import (C, socketpair, "socketpair");

  -- Give the socket FD the local address ADDR (which is LEN bytes long).
   function bind
     (uu_fd : int;
      uu_addr : access constant bits_socket_h.sockaddr;
      uu_len : bits_socket_h.socklen_t) return int;  -- /usr/include/sys/socket.h:116:6
   pragma Import (C, bind, "bind");

  -- Put the local address of FD into *ADDR and its length in *LEN.
   function getsockname
     (uu_fd : int;
      uu_addr : access bits_socket_h.sockaddr;
      uu_len : access bits_socket_h.socklen_t) return int;  -- /usr/include/sys/socket.h:120:33
   pragma Import (C, getsockname, "getsockname");

  -- Open a connection on socket FD to peer at ADDR (which LEN bytes long).
  --   For connectionless socket types, just set the default address to send to
  --   and the only address from which to accept transmissions.
  --   Return 0 on success, -1 for errors.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.

   function connect
     (uu_fd : int;
      uu_addr : access constant bits_socket_h.sockaddr;
      uu_len : bits_socket_h.socklen_t) return int;  -- /usr/include/sys/socket.h:129:75
   pragma Import (C, connect, "connect");

  -- Put the address of the peer connected to socket FD into *ADDR
  --   (which is *LEN bytes long), and its actual length into *LEN.

   function getpeername
     (uu_fd : int;
      uu_addr : access bits_socket_h.sockaddr;
      uu_len : access bits_socket_h.socklen_t) return int;  -- /usr/include/sys/socket.h:134:33
   pragma Import (C, getpeername, "getpeername");

  -- Send N bytes of BUF to socket FD.  Returns the number sent or -1.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.

   function send
     (uu_fd : int;
      uu_buf : System.Address;
      uu_n : stddef_h.size_t;
      uu_flags : int) return sys_types_h.ssize_t;  -- /usr/include/sys/socket.h:141:76
   pragma Import (C, send, "send");

  -- Read N bytes into BUF from socket FD.
  --   Returns the number read or -1 for errors.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.

   function recv
     (uu_fd : int;
      uu_buf : System.Address;
      uu_n : stddef_h.size_t;
      uu_flags : int) return sys_types_h.ssize_t;  -- /usr/include/sys/socket.h:148:68
   pragma Import (C, recv, "recv");

  -- Send N bytes of BUF on socket FD to peer at address ADDR (which is
  --   ADDR_LEN bytes long).  Returns the number sent, or -1 for errors.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.

   function sendto
     (uu_fd : int;
      uu_buf : Buffer_Type_Ptr;
      uu_n : stddef_h.size_t;
      uu_flags : int;
      --uu_addr : access constant bits_socket_h.sockaddr;
      Uu_Addr : access constant Linux_If_Packet_H.Sockaddr_Ll;
      uu_addr_len : bits_socket_h.socklen_t) return sys_types_h.ssize_t;  -- /usr/include/sys/socket.h:157:30
   pragma Import (C, sendto, "sendto");

  -- Read N bytes into BUF through socket FD.
  --   If ADDR is not NULL, fill in *ADDR_LEN bytes of it with tha address of
  --   the sender, and store the actual size of the address in *ADDR_LEN.
  --   Returns the number of bytes read or -1 for errors.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.

   function recvfrom
     (uu_fd : int;
      Uu_Buf : Buffer_Type_Ptr;
      uu_n : stddef_h.size_t;
      uu_flags : int;
      uu_addr : access Linux_If_Packet_H.Sockaddr_Ll;
      Uu_Addr_Len : access Unsigned) return Sys_Types_H.Ssize_T;  -- /usr/include/sys/socket.h:168:37
   pragma Import (C, recvfrom, "recvfrom");

  -- Send a message described MESSAGE on socket FD.
  --   Returns the number of bytes sent, or -1 for errors.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.

   function sendmsg
     (uu_fd : int;
      uu_message : access constant bits_socket_h.msghdr;
      uu_flags : int) return sys_types_h.ssize_t;  -- /usr/include/sys/socket.h:177:15
   pragma Import (C, sendmsg, "sendmsg");

  -- Receive a message as described by MESSAGE from socket FD.
  --   Returns the number of bytes read or -1 for errors.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.

   function recvmsg
     (uu_fd : int;
      uu_message : access bits_socket_h.msghdr;
      uu_flags : int) return sys_types_h.ssize_t;  -- /usr/include/sys/socket.h:184:72
   pragma Import (C, recvmsg, "recvmsg");

  -- Put the current value for socket FD's option OPTNAME at protocol level LEVEL
  --   into OPTVAL (which is *OPTLEN bytes long), and set *OPTLEN to the value's
  --   actual length.  Returns 0 on success, -1 for errors.

   function getsockopt
     (uu_fd : int;
      uu_level : int;
      uu_optname : int;
      uu_optval : System.Address;
      uu_optlen : access bits_socket_h.socklen_t) return int;  -- /usr/include/sys/socket.h:192:42
   pragma Import (C, getsockopt, "getsockopt");

  -- Set socket FD's option OPTNAME at protocol level LEVEL
  --   to *OPTVAL (which is OPTLEN bytes long).
  --   Returns 0 on success, -1 for errors.

   function setsockopt
     (uu_fd : int;
      uu_level : int;
      uu_optname : int;
      uu_optval : System.Address;
      uu_optlen : bits_socket_h.socklen_t) return int;  -- /usr/include/sys/socket.h:198:54
   pragma Import (C, setsockopt, "setsockopt");

  -- Prepare to accept connections on socket FD.
  --   N connection requests will be queued before further requests are refused.
  --   Returns 0 on success, -1 for errors.

   function listen (uu_fd : int; uu_n : int) return int;  -- /usr/include/sys/socket.h:204:39
   pragma Import (C, listen, "listen");

  -- Await a connection on socket FD.
  --   When a connection arrives, open a new socket to communicate with it,
  --   set *ADDR (which is *ADDR_LEN bytes long) to the address of the connecting
  --   peer and *ADDR_LEN to the address's actual length, and return the
  --   new socket's descriptor, or -1 for errors.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.

   function c_accept
     (uu_fd : int;
      uu_addr : access bits_socket_h.sockaddr;
      uu_addr_len : access bits_socket_h.socklen_t) return int;  -- /usr/include/sys/socket.h:215:38
   pragma Import (C, c_accept, "accept");

  -- Shut down all or part of the connection open on socket FD.
  --   HOW determines what to shut down:
  --     SHUT_RD   = No more receptions;
  --     SHUT_WR   = No more transmissions;
  --     SHUT_RDWR = No more receptions or transmissions.
  --   Returns 0 on success, -1 for errors.

   function shutdown (uu_fd : int; uu_how : int) return int;  -- /usr/include/sys/socket.h:223:43
   pragma Import (C, shutdown, "shutdown");

  -- Determine wheter socket is at a out-of-band mark.
   function sockatmark (uu_fd : int) return int;  -- /usr/include/sys/socket.h:228:34
   pragma Import (C, sockatmark, "sockatmark");

  -- FDTYPE is S_IFSOCK or another S_IF* macro defined in <sys/stat.h>;
  --   returns 1 if FD is open on an object of the indicated type, 0 if not,
  --   or -1 for errors (setting errno).

   function isfdtype (uu_fd : int; uu_fdtype : int) return int;  -- /usr/include/sys/socket.h:236:46
   pragma Import (C, isfdtype, "isfdtype");

  -- Define some macros helping to catch buffer overflows.
end sys_socket_h;
