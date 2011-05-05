with Interfaces.C; use Interfaces.C;

package bits_sockaddr_h is

  -- Definition of `struct sockaddr_*' common members.  Generic/4.2 BSD version.
  --   Copyright (C) 1995,1996,1997,1998,2000,2001 Free Software Foundation, Inc.
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

  -- * Never include this file directly; use <sys/socket.h> instead.
  --  

  -- POSIX.1g specifies this type name for the `sa_family' member.   
   subtype sa_family_t is unsigned_short;  -- /usr/include/bits/sockaddr.h:29:28

  -- This macro is used to declare the initial common members
  --   of the data types used for socket addresses, `struct sockaddr',
  --   `struct sockaddr_in', `struct sockaddr_un', etc.   

end bits_sockaddr_h;
