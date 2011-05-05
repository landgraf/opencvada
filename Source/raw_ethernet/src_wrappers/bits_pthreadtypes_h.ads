with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with System;

package bits_pthreadtypes_h is

  -- Copyright (C) 2002,2003,2004,2005,2006,2007 Free Software Foundation, Inc.
  --   This file is part of the GNU C Library.
  --   Contributed by Ulrich Drepper <drepper@redhat.com>, 2002.
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

  -- Thread identifiers.  The structure of the attribute type is not
  --   exposed on purpose.   

   subtype pthread_t is unsigned_long;  -- /usr/include/bits/pthreadtypes.h:50:27

   type pthread_attr_t_uu_size_array is array (0 .. 35) of aliased char;
   type pthread_attr_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
         uu_size : aliased pthread_attr_t_uu_size_array;  -- /usr/include/bits/pthreadtypes.h:55:38
         when others =>
         uu_align : aliased long;  -- /usr/include/bits/pthreadtypes.h:56:12
      end case;
   end record;
   pragma Convention (C, pthread_attr_t);
   pragma Unchecked_Union (pthread_attr_t);  -- /usr/include/bits/pthreadtypes.h:57:3

   --  skipped anonymous struct anon_3

   type uu_pthread_internal_slist is record
      uu_next : access uu_pthread_internal_slist;  -- /usr/include/bits/pthreadtypes.h:69:36
   end record;
   pragma Convention (C, uu_pthread_internal_slist);  -- /usr/include/bits/pthreadtypes.h:68:1

   subtype uu_pthread_slist_t is uu_pthread_internal_slist;

  -- Data structures for mutex handling.  The structure of the attribute
  --   type is not exposed on purpose.   

  -- KIND must stay at this position in the structure to maintain
  --       binary compatibility.   

   type pthread_mutex_t_uu_size_array is array (0 .. 23) of aliased char;
   type pthread_mutex_t;
   type anon_5 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
         uu_spins : aliased int;  -- /usr/include/bits/pthreadtypes.h:97:11
         when others =>
         uu_list : aliased uu_pthread_slist_t;  -- /usr/include/bits/pthreadtypes.h:98:25
      end case;
   end record;
   pragma Convention (C, anon_5);
   pragma Unchecked_Union (anon_5);
   type uu_pthread_mutex_s is record
      uu_lock : aliased int;  -- /usr/include/bits/pthreadtypes.h:80:9
      uu_count : aliased unsigned;  -- /usr/include/bits/pthreadtypes.h:81:18
      uu_owner : aliased int;  -- /usr/include/bits/pthreadtypes.h:82:9
      uu_kind : aliased int;  -- /usr/include/bits/pthreadtypes.h:88:9
      uu_nusers : aliased unsigned;  -- /usr/include/bits/pthreadtypes.h:94:18
      Parent6 : anon_5;
   end record;
   pragma Convention (C, uu_pthread_mutex_s);
   type pthread_mutex_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
         uu_data : aliased uu_pthread_mutex_s;  -- /usr/include/bits/pthreadtypes.h:101:5
         when 1 =>
         uu_size : aliased pthread_mutex_t_uu_size_array;  -- /usr/include/bits/pthreadtypes.h:102:39
         when others =>
         uu_align : aliased long;  -- /usr/include/bits/pthreadtypes.h:103:12
      end case;
   end record;
   pragma Convention (C, pthread_mutex_t);
   pragma Unchecked_Union (pthread_mutex_t);  -- /usr/include/bits/pthreadtypes.h:104:3

   --  skipped anonymous struct anon_4

   type pthread_mutexattr_t_uu_size_array is array (0 .. 3) of aliased char;
   type pthread_mutexattr_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
         uu_size : aliased pthread_mutexattr_t_uu_size_array;  -- /usr/include/bits/pthreadtypes.h:108:43
         when others =>
         uu_align : aliased int;  -- /usr/include/bits/pthreadtypes.h:109:7
      end case;
   end record;
   pragma Convention (C, pthread_mutexattr_t);
   pragma Unchecked_Union (pthread_mutexattr_t);  -- /usr/include/bits/pthreadtypes.h:110:3

   --  skipped anonymous struct anon_6

  -- Data structure for conditional variable handling.  The structure of
  --   the attribute type is not exposed on purpose.   

   type pthread_cond_t_uu_size_array is array (0 .. 47) of aliased char;
   type pthread_cond_t;
   type anon_8 is record
      uu_lock : aliased int;  -- /usr/include/bits/pthreadtypes.h:119:9
      uu_futex : aliased unsigned;  -- /usr/include/bits/pthreadtypes.h:120:18
      uu_total_seq : aliased Extensions.unsigned_long_long;  -- /usr/include/bits/pthreadtypes.h:121:42
      uu_wakeup_seq : aliased Extensions.unsigned_long_long;  -- /usr/include/bits/pthreadtypes.h:122:42
      uu_woken_seq : aliased Extensions.unsigned_long_long;  -- /usr/include/bits/pthreadtypes.h:123:42
      uu_mutex : System.Address;  -- /usr/include/bits/pthreadtypes.h:124:11
      uu_nwaiters : aliased unsigned;  -- /usr/include/bits/pthreadtypes.h:125:18
      uu_broadcast_seq : aliased unsigned;  -- /usr/include/bits/pthreadtypes.h:126:18
   end record;
   pragma Convention (C, anon_8);
   type pthread_cond_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
         uu_data : aliased anon_8;  -- /usr/include/bits/pthreadtypes.h:127:5
         when 1 =>
         uu_size : aliased pthread_cond_t_uu_size_array;  -- /usr/include/bits/pthreadtypes.h:128:38
         when others =>
         uu_align : aliased Long_Long_Integer;  -- /usr/include/bits/pthreadtypes.h:129:31
      end case;
   end record;
   pragma Convention (C, pthread_cond_t);
   pragma Unchecked_Union (pthread_cond_t);  -- /usr/include/bits/pthreadtypes.h:130:3

   --  skipped anonymous struct anon_7

   type pthread_condattr_t_uu_size_array is array (0 .. 3) of aliased char;
   type pthread_condattr_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
         uu_size : aliased pthread_condattr_t_uu_size_array;  -- /usr/include/bits/pthreadtypes.h:134:42
         when others =>
         uu_align : aliased int;  -- /usr/include/bits/pthreadtypes.h:135:7
      end case;
   end record;
   pragma Convention (C, pthread_condattr_t);
   pragma Unchecked_Union (pthread_condattr_t);  -- /usr/include/bits/pthreadtypes.h:136:3

   --  skipped anonymous struct anon_9

  -- Keys for thread-specific data  
   subtype pthread_key_t is unsigned;  -- /usr/include/bits/pthreadtypes.h:140:22

  -- Once-only execution  
   subtype pthread_once_t is int;  -- /usr/include/bits/pthreadtypes.h:144:13

  -- Data structure for read-write lock variable handling.  The
  --   structure of the attribute type is not exposed on purpose.   

  -- FLAGS must stay at this position in the structure to maintain
  --       binary compatibility.   

  -- FLAGS must stay at this position in the structure to maintain
  --       binary compatibility.   

   type pthread_rwlock_t_uu_size_array is array (0 .. 31) of aliased char;
   type pthread_rwlock_t;
   type anon_11 is record
      uu_lock : aliased int;  -- /usr/include/bits/pthreadtypes.h:172:9
      uu_nr_readers : aliased unsigned;  -- /usr/include/bits/pthreadtypes.h:173:18
      uu_readers_wakeup : aliased unsigned;  -- /usr/include/bits/pthreadtypes.h:174:18
      uu_writer_wakeup : aliased unsigned;  -- /usr/include/bits/pthreadtypes.h:175:18
      uu_nr_readers_queued : aliased unsigned;  -- /usr/include/bits/pthreadtypes.h:176:18
      uu_nr_writers_queued : aliased unsigned;  -- /usr/include/bits/pthreadtypes.h:177:18
      uu_flags : aliased unsigned_char;  -- /usr/include/bits/pthreadtypes.h:180:19
      uu_shared : aliased unsigned_char;  -- /usr/include/bits/pthreadtypes.h:181:19
      uu_pad1 : aliased unsigned_char;  -- /usr/include/bits/pthreadtypes.h:182:19
      uu_pad2 : aliased unsigned_char;  -- /usr/include/bits/pthreadtypes.h:183:19
      uu_writer : aliased int;  -- /usr/include/bits/pthreadtypes.h:184:9
   end record;
   pragma Convention (C, anon_11);
   type pthread_rwlock_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
         uu_data : aliased anon_11;  -- /usr/include/bits/pthreadtypes.h:185:5
         when 1 =>
         uu_size : aliased pthread_rwlock_t_uu_size_array;  -- /usr/include/bits/pthreadtypes.h:187:40
         when others =>
         uu_align : aliased long;  -- /usr/include/bits/pthreadtypes.h:188:12
      end case;
   end record;
   pragma Convention (C, pthread_rwlock_t);
   pragma Unchecked_Union (pthread_rwlock_t);  -- /usr/include/bits/pthreadtypes.h:189:3

   --  skipped anonymous struct anon_10

   type pthread_rwlockattr_t_uu_size_array is array (0 .. 7) of aliased char;
   type pthread_rwlockattr_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
         uu_size : aliased pthread_rwlockattr_t_uu_size_array;  -- /usr/include/bits/pthreadtypes.h:193:44
         when others =>
         uu_align : aliased long;  -- /usr/include/bits/pthreadtypes.h:194:12
      end case;
   end record;
   pragma Convention (C, pthread_rwlockattr_t);
   pragma Unchecked_Union (pthread_rwlockattr_t);  -- /usr/include/bits/pthreadtypes.h:195:3

   --  skipped anonymous struct anon_12

  -- POSIX spinlock data type.   
   subtype pthread_spinlock_t is int;  -- /usr/include/bits/pthreadtypes.h:201:22

  -- POSIX barriers data type.  The structure of the type is
  --   deliberately not exposed.   

   type pthread_barrier_t_uu_size_array is array (0 .. 19) of aliased char;
   type pthread_barrier_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
         uu_size : aliased pthread_barrier_t_uu_size_array;  -- /usr/include/bits/pthreadtypes.h:208:41
         when others =>
         uu_align : aliased long;  -- /usr/include/bits/pthreadtypes.h:209:12
      end case;
   end record;
   pragma Convention (C, pthread_barrier_t);
   pragma Unchecked_Union (pthread_barrier_t);  -- /usr/include/bits/pthreadtypes.h:210:3

   --  skipped anonymous struct anon_13

   type pthread_barrierattr_t_uu_size_array is array (0 .. 3) of aliased char;
   type pthread_barrierattr_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
         uu_size : aliased pthread_barrierattr_t_uu_size_array;  -- /usr/include/bits/pthreadtypes.h:214:45
         when others =>
         uu_align : aliased int;  -- /usr/include/bits/pthreadtypes.h:215:7
      end case;
   end record;
   pragma Convention (C, pthread_barrierattr_t);
   pragma Unchecked_Union (pthread_barrierattr_t);  -- /usr/include/bits/pthreadtypes.h:216:3

   --  skipped anonymous struct anon_14

  -- Extra attributes for the cleanup functions.   
end bits_pthreadtypes_h;
