with Interfaces.C; use Interfaces.C;
with Linux_If_H;
package sys_ioctl_h is

  -- Copyright (C) 1991, 92, 93, 94, 96, 98, 99 Free Software Foundation, Inc.
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

  -- Get the list of `ioctl' requests and related constants.
  -- Define some types used by `ioctl' requests.
  -- On a Unix system, the system <sys/ioctl.h> probably defines some of
  --   the symbols we define in <sys/ttydefaults.h> (usually with the same
  --   values).  The code to generate <bits/ioctls.h> has omitted these
  --   symbols to avoid the conflict, but a Unix program expects <sys/ioctl.h>
  --   to define them, so we must include <sys/ttydefaults.h> here.

  -- Perform the I/O control operation specified by REQUEST on FD.
  --   One argument may follow; its presence and type depend on REQUEST.
  --   Return value depends on REQUEST.  Usually -1 indicates error.

   SIOCGIFINDEX : constant := 16#8933#;
   SIOCGIFHWADDR : constant := 16#8927#;
   SIOCSIFFLAGS : constant := 16#8914#;

   function ioctl (uu_fd : int; uu_request : unsigned_long  -- , ...
      ) return int;  -- /usr/include/sys/ioctl.h:42:63
--     pragma Import (C, ioctl, "ioctl");
   function ioctl (uu_fd      : int;
                   uu_request : Unsigned_Long;
                   Uu_Ifr : Linux_If_H.Ifreq_Ptr
                  ) return Int;  -- /usr/include/sys/ioctl.h:42:63
   pragma Import (C, ioctl, "ioctl");

end sys_ioctl_h;