with Interfaces.C; use Interfaces.C;
with bits_types_h;
with Interfaces.C.Extensions;

package sys_types_h is

  -- Copyright (C) 1991,1992,1994,1995,1996,1997,1998,1999,2000,2001,2002,2006
  --   	Free Software Foundation, Inc.
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

  -- *	POSIX Standard: 2.6 Primitive System Data Types	<sys/types.h>
  --  

   subtype u_char is bits_types_h.uu_u_char;  -- /usr/include/sys/types.h:35:18

   subtype u_short is bits_types_h.uu_u_short;  -- /usr/include/sys/types.h:36:19

   subtype u_int is bits_types_h.uu_u_int;  -- /usr/include/sys/types.h:37:17

   subtype u_long is bits_types_h.uu_u_long;  -- /usr/include/sys/types.h:38:18

   subtype quad_t is bits_types_h.uu_quad_t;  -- /usr/include/sys/types.h:39:18

   subtype u_quad_t is bits_types_h.uu_u_quad_t;  -- /usr/include/sys/types.h:40:20

   subtype fsid_t is bits_types_h.uu_fsid_t;

   subtype loff_t is bits_types_h.uu_loff_t;  -- /usr/include/sys/types.h:46:18

   subtype ino_t is bits_types_h.uu_ino_t;  -- /usr/include/sys/types.h:50:17

   subtype ino64_t is bits_types_h.uu_ino64_t;  -- /usr/include/sys/types.h:57:19

   subtype dev_t is bits_types_h.uu_dev_t;  -- /usr/include/sys/types.h:62:17

   subtype gid_t is bits_types_h.uu_gid_t;  -- /usr/include/sys/types.h:67:17

   subtype mode_t is bits_types_h.uu_mode_t;  -- /usr/include/sys/types.h:72:18

   subtype nlink_t is bits_types_h.uu_nlink_t;  -- /usr/include/sys/types.h:77:19

   subtype uid_t is bits_types_h.uu_uid_t;  -- /usr/include/sys/types.h:82:17

   subtype off_t is bits_types_h.uu_off_t;  -- /usr/include/sys/types.h:88:17

   subtype off64_t is bits_types_h.uu_off64_t;  -- /usr/include/sys/types.h:95:19

   subtype pid_t is bits_types_h.uu_pid_t;  -- /usr/include/sys/types.h:100:17

   subtype id_t is bits_types_h.uu_id_t;  -- /usr/include/sys/types.h:105:16

   subtype ssize_t is bits_types_h.uu_ssize_t;  -- /usr/include/sys/types.h:110:19

   subtype daddr_t is bits_types_h.uu_daddr_t;  -- /usr/include/sys/types.h:116:19

   subtype caddr_t is bits_types_h.uu_caddr_t;  -- /usr/include/sys/types.h:117:19

   subtype key_t is bits_types_h.uu_key_t;  -- /usr/include/sys/types.h:123:17

   subtype useconds_t is bits_types_h.uu_useconds_t;  -- /usr/include/sys/types.h:137:22

   subtype suseconds_t is bits_types_h.uu_suseconds_t;  -- /usr/include/sys/types.h:141:23

  -- Old compatibility names for C types.   
   subtype ulong is unsigned_long;  -- /usr/include/sys/types.h:151:27

   subtype ushort is unsigned_short;  -- /usr/include/sys/types.h:152:28

   subtype uint is unsigned;  -- /usr/include/sys/types.h:153:22

  -- These size-specific names are used by some of the inet code.   
  -- These types are defined by the ISO C99 header <inttypes.h>.  
  -- But these were defined by ISO C without the first `_'.   
  -- For GCC 2.7 and later, we can use specific type-size attributes.   
   subtype int8_t is signed_char;  -- /usr/include/sys/types.h:195:1

   subtype int16_t is short;  -- /usr/include/sys/types.h:196:1

   subtype int32_t is int;  -- /usr/include/sys/types.h:197:1

   subtype int64_t is Long_Long_Integer;  -- /usr/include/sys/types.h:198:1

   subtype u_int8_t is unsigned_char;  -- /usr/include/sys/types.h:201:1

   subtype u_int16_t is unsigned_short;  -- /usr/include/sys/types.h:202:1

   subtype u_int32_t is unsigned;  -- /usr/include/sys/types.h:203:1

   subtype u_int64_t is Extensions.unsigned_long_long;  -- /usr/include/sys/types.h:204:1

   subtype register_t is int;  -- /usr/include/sys/types.h:206:60

  -- Some code from BIND tests this macro to see if the types above are
  --   defined.   

  -- In BSD <sys/types.h> is expected to define BYTE_ORDER.   
  -- It also defines `fd_set' and the FD_* macros for `select'.   
  -- BSD defines these symbols, so we follow.   
   subtype blksize_t is bits_types_h.uu_blksize_t;  -- /usr/include/sys/types.h:228:21

  -- Types from the Large File Support interface.   
  -- Type to count number of disk blocks.   
   subtype blkcnt_t is bits_types_h.uu_blkcnt_t;  -- /usr/include/sys/types.h:235:20

  -- Type to count file system blocks.   
   subtype fsblkcnt_t is bits_types_h.uu_fsblkcnt_t;  -- /usr/include/sys/types.h:239:22

  -- Type to count file system inodes.   
   subtype fsfilcnt_t is bits_types_h.uu_fsfilcnt_t;  -- /usr/include/sys/types.h:243:22

  -- Type to count number of disk blocks.   
  -- Type to count file system blocks.   
  -- Type to count file system inodes.   
  -- Type to count number of disk blocks.  
   subtype blkcnt64_t is bits_types_h.uu_blkcnt64_t;  -- /usr/include/sys/types.h:262:22

  -- Type to count file system blocks.   
   subtype fsblkcnt64_t is bits_types_h.uu_fsblkcnt64_t;  -- /usr/include/sys/types.h:263:24

  -- Type to count file system inodes.   
   subtype fsfilcnt64_t is bits_types_h.uu_fsfilcnt64_t;  -- /usr/include/sys/types.h:264:24

  -- Now add the thread types.   
end sys_types_h;
