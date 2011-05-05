with Interfaces.C; use Interfaces.C;

package bits_ioctl_types_h is


  NCC : constant := 8;  --  /usr/include/bits/ioctl-types.h:36

  TIOCM_LE : constant := 16#001#;  --  /usr/include/bits/ioctl-types.h:48
  TIOCM_DTR : constant := 16#002#;  --  /usr/include/bits/ioctl-types.h:49
  TIOCM_RTS : constant := 16#004#;  --  /usr/include/bits/ioctl-types.h:50
  TIOCM_ST : constant := 16#008#;  --  /usr/include/bits/ioctl-types.h:51
  TIOCM_SR : constant := 16#010#;  --  /usr/include/bits/ioctl-types.h:52
  TIOCM_CTS : constant := 16#020#;  --  /usr/include/bits/ioctl-types.h:53
  TIOCM_CAR : constant := 16#040#;  --  /usr/include/bits/ioctl-types.h:54
  TIOCM_RNG : constant := 16#080#;  --  /usr/include/bits/ioctl-types.h:55
  TIOCM_DSR : constant := 16#100#;  --  /usr/include/bits/ioctl-types.h:56
  --  unsupported macro: TIOCM_CD TIOCM_CAR
  --  unsupported macro: TIOCM_RI TIOCM_RNG

  N_TTY : constant := 0;  --  /usr/include/bits/ioctl-types.h:63
  N_SLIP : constant := 1;  --  /usr/include/bits/ioctl-types.h:64
  N_MOUSE : constant := 2;  --  /usr/include/bits/ioctl-types.h:65
  N_PPP : constant := 3;  --  /usr/include/bits/ioctl-types.h:66
  N_STRIP : constant := 4;  --  /usr/include/bits/ioctl-types.h:67
  N_AX25 : constant := 5;  --  /usr/include/bits/ioctl-types.h:68
  N_X25 : constant := 6;  --  /usr/include/bits/ioctl-types.h:69
  N_6PACK : constant := 7;  --  /usr/include/bits/ioctl-types.h:70
  N_MASC : constant := 8;  --  /usr/include/bits/ioctl-types.h:71
  N_R3964 : constant := 9;  --  /usr/include/bits/ioctl-types.h:72
  N_PROFIBUS_FDL : constant := 10;  --  /usr/include/bits/ioctl-types.h:73
  N_IRDA : constant := 11;  --  /usr/include/bits/ioctl-types.h:74
  N_SMSBLOCK : constant := 12;  --  /usr/include/bits/ioctl-types.h:75
  N_HDLC : constant := 13;  --  /usr/include/bits/ioctl-types.h:76
  N_SYNC_PPP : constant := 14;  --  /usr/include/bits/ioctl-types.h:77
  N_HCI : constant := 15;  --  /usr/include/bits/ioctl-types.h:78

  -- Structure types for pre-termios terminal ioctls.  Linux version.
  --   Copyright (C) 1996, 1997, 1999, 2001 Free Software Foundation, Inc.
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

  -- Get definition of constants for use with `ioctl'.   
   type winsize is record
      ws_row : aliased unsigned_short;  -- /usr/include/bits/ioctl-types.h:30:24
      ws_col : aliased unsigned_short;  -- /usr/include/bits/ioctl-types.h:31:24
      ws_xpixel : aliased unsigned_short;  -- /usr/include/bits/ioctl-types.h:32:24
      ws_ypixel : aliased unsigned_short;  -- /usr/include/bits/ioctl-types.h:33:24
   end record;
   pragma Convention (C, winsize);  -- /usr/include/bits/ioctl-types.h:29:3

  -- input mode flags  
   type termio_c_cc_array is array (0 .. 7) of aliased unsigned_char;
   type termio is record
      c_iflag : aliased unsigned_short;  -- /usr/include/bits/ioctl-types.h:39:24
      c_oflag : aliased unsigned_short;  -- /usr/include/bits/ioctl-types.h:40:24
      c_cflag : aliased unsigned_short;  -- /usr/include/bits/ioctl-types.h:41:24
      c_lflag : aliased unsigned_short;  -- /usr/include/bits/ioctl-types.h:42:24
      c_line : aliased unsigned_char;  -- /usr/include/bits/ioctl-types.h:43:19
      c_cc : aliased termio_c_cc_array;  -- /usr/include/bits/ioctl-types.h:44:27
   end record;
   pragma Convention (C, termio);  -- /usr/include/bits/ioctl-types.h:38:3

  -- output mode flags  
  -- control mode flags  
  -- local mode flags  
  -- line discipline  
  -- control characters  
  -- modem lines  
  -- ioctl (fd, TIOCSERGETLSR, &result) where result may be as below  
  -- line disciplines  
end bits_ioctl_types_h;
