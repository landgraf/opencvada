with Interfaces.C; use Interfaces.C;
with bits_sigset_h;
limited with bits_time_h;
limited with time_h;

package sys_select_h is

  --  unsupported macro: FD_SETSIZE __FD_SETSIZE
  --  unsupported macro: NFDBITS __NFDBITS
  --  unsupported macro: FD_SET(fd,fdsetp) __FD_SET (fd, fdsetp)
  --  unsupported macro: FD_CLR(fd,fdsetp) __FD_CLR (fd, fdsetp)
  --  unsupported macro: FD_ISSET(fd,fdsetp) __FD_ISSET (fd, fdsetp)
  --  unsupported macro: FD_ZERO(fdsetp) __FD_ZERO (fdsetp)
  -- `fd_set' type and related macros, and `select'/`pselect' declarations.
  --   Copyright (C) 1996,97,98,99,2000,01,02,2003 Free Software Foundation, Inc.
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

  --	POSIX 1003.1g: 6.2 Select from File Descriptor Sets <sys/select.h>   
  -- Get definition of needed basic types.   
  -- Get __FD_* definitions.   
  -- Get __sigset_t.   
   subtype sigset_t is bits_sigset_h.uu_sigset_t;

  -- Get definition of timer specification structures.   
  -- The fd_set member is required to be an array of longs.   
   subtype uu_fd_mask is long;  -- /usr/include/sys/select.h:55:18

  -- Some versions of <linux/posix_types.h> define these macros.   
  -- It's easier to assume 8-bit bytes than to get CHAR_BIT.   
  -- fd_set for select and pselect.   
  -- XPG4.2 requires this member name.  Otherwise avoid the name
  --       from the global namespace.   

   type fd_set_fds_bits_array is array (0 .. 31) of aliased uu_fd_mask;
   type fd_set is record
      fds_bits : aliased fd_set_fds_bits_array;  -- /usr/include/sys/select.h:72:48
   end record;
   pragma Convention (C, fd_set);  -- /usr/include/sys/select.h:78:5

   --  skipped anonymous struct anon_2

  -- Maximum number of file descriptors in `fd_set'.   
  -- Sometimes the fd_set member is assumed to have this type.   
   subtype fd_mask is uu_fd_mask;  -- /usr/include/sys/select.h:85:19

  -- Number of bits per word of `fd_set' (some code assumes this is 32).   
  -- Access macros for `fd_set'.   
  -- Check the first NFDS descriptors each in READFDS (if not NULL) for read
  --   readiness, in WRITEFDS (if not NULL) for write readiness, and in EXCEPTFDS
  --   (if not NULL) for exceptional conditions.  If TIMEOUT is not NULL, time out
  --   after waiting the interval specified therein.  Returns the number of ready
  --   descriptors, or -1 for errors.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function c_select
     (uu_nfds : int;
      uu_readfds : access fd_set;
      uu_writefds : access fd_set;
      uu_exceptfds : access fd_set;
      uu_timeout : access bits_time_h.timeval) return int;  -- /usr/include/sys/select.h:112:42
   pragma Import (C, c_select, "select");

  -- Same as above only that the TIMEOUT value is given with higher
  --   resolution and a sigmask which is been set temporarily.  This version
  --   should be used.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function pselect
     (uu_nfds : int;
      uu_readfds : access fd_set;
      uu_writefds : access fd_set;
      uu_exceptfds : access fd_set;
      uu_timeout : access constant time_h.timespec;
      uu_sigmask : access constant bits_sigset_h.uu_sigset_t) return int;  -- /usr/include/sys/select.h:125:45
   pragma Import (C, pselect, "pselect");

end sys_select_h;
