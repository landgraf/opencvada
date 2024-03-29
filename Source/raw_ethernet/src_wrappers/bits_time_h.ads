with Interfaces.C; use Interfaces.C;
with bits_types_h;

package bits_time_h is

  -- System-dependent timing definitions.  Generic version.
  --   Copyright (C) 1996,1997,1999-2002,2003 Free Software Foundation, Inc.
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

  -- * Never include this file directly; use <time.h> instead.
  --  

  -- ISO/IEC 9899:1990 7.12.1: <time.h>
  --   The macro `CLOCKS_PER_SEC' is the number per second of the value
  --   returned by the `clock' function.  

  -- CAE XSH, Issue 4, Version 2: <time.h>
  --   The value of CLOCKS_PER_SEC is required to be 1 million on all
  --   XSI-conformant systems.  

  -- Even though CLOCKS_PER_SEC has such a strange value CLK_TCK
  --   presents the real value for clock ticks per second for the system.   

  -- Identifier for system-wide realtime clock.   
  -- Monotonic system-wide clock.   
  -- High-resolution timer from the CPU.   
  -- Thread-specific CPU-time clock.   
  -- Flag to indicate time is absolute.   
  -- A time value that is accurate to the nearest
  --   microsecond but also has a range of years.   

  -- Seconds.   
   type timeval is record
      tv_sec : aliased bits_types_h.uu_time_t;  -- /usr/include/bits/time.h:71:14
      tv_usec : aliased bits_types_h.uu_suseconds_t;  -- /usr/include/bits/time.h:72:19
   end record;
   pragma Convention (C, timeval);  -- /usr/include/bits/time.h:70:3

  -- Microseconds.   
end bits_time_h;
