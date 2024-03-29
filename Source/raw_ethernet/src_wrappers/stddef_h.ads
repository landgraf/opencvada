with Interfaces.C; use Interfaces.C;

package stddef_h is

  -- Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
  --   Free Software Foundation, Inc.
  --This file is part of GCC.
  --GCC is free software; you can redistribute it and/or modify
  --it under the terms of the GNU General Public License as published by
  --the Free Software Foundation; either version 2, or (at your option)
  --any later version.
  --GCC is distributed in the hope that it will be useful,
  --but WITHOUT ANY WARRANTY; without even the implied warranty of
  --MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  --GNU General Public License for more details.
  --You should have received a copy of the GNU General Public License
  --along with GCC; see the file COPYING.  If not, write to
  --the Free Software Foundation, 51 Franklin Street, Fifth Floor,
  --Boston, MA 02110-1301, USA.   

  -- As a special exception, if you include this header file into source
  --   files compiled by GCC, this header file does not by itself cause
  --   the resulting executable to be covered by the GNU General Public
  --   License.  This exception does not however invalidate any other
  --   reasons why the executable file might be covered by the GNU General
  --   Public License.   

  -- * ISO C Standard:  7.17  Common definitions  <stddef.h>
  --  

  -- Any one of these symbols __need_* means that GNU libc
  --   wants us just to define one data type.  So don't define
  --   the symbols that indicate this file's entire job has been done.   

  -- snaroff@next.com says the NeXT needs this.   
  -- Irix 5.1 needs this.   
  -- This avoids lossage on SunOS but only if stdtypes.h comes first.
  --   There's no way to win with the other order!  Sun lossage.   

  -- On 4.3bsd-net2, make sure ansi.h is included, so we have
  --   one less case to deal with in the following.   

  -- On FreeBSD 5, machine/ansi.h does not exist anymore...  
  -- In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
  --   defined if the corresponding type is *not* defined.
  --   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_  

  -- On BSD/386 1.1, at least, machine/ansi.h defines _BSD_WCHAR_T_
  --   instead of _WCHAR_T_.  

  -- Undef _FOO_T_ if we are supposed to define foo_t.   
  -- Sequent's header files use _PTRDIFF_T_ in some conflicting way.
  --   Just ignore it.   

  -- On VxWorks, <type/vxTypesBase.h> may have defined macros like
  --   _TYPE_size_t which will typedef size_t.  fixincludes patched the
  --   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
  --   not defined, and so that defining this macro defines _GCC_SIZE_T.
  --   If we find that the macros are still defined at this point, we must
  --   invoke them so that the type is defined as expected.   

  -- In case nobody has defined these types, but we aren't running under
  --   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
  --   __WCHAR_TYPE__ have reasonable values.  This can happen if the
  --   parts of GCC is compiled by an older compiler, that actually
  --   include gstddef.h, such as collect2.   

  -- Signed type of difference of two pointers.   
  -- Define this type if we are doing the whole job,
  --   or if we want this type in particular.   

  -- If this symbol has done its job, get rid of it.   
  -- Unsigned type of `sizeof' something.   
  -- Define this type if we are doing the whole job,
  --   or if we want this type in particular.   

  -- __size_t is a typedef on FreeBSD 5!, must not trash it.  
   subtype size_t is unsigned;  -- /usr/gnat09/bin/../lib/gcc/i686-pc-linux-gnu/4.3.4/include/stddef.h:214:23

  -- Wide character type.
  --   Locale-writers should change this as necessary to
  --   be big enough to hold unique values not between 0 and 127,
  --   and not (wchar_t) -1, for each defined multibyte character.   

  -- Define this type if we are doing the whole job,
  --   or if we want this type in particular.   

  -- On BSD/386 1.1, at least, machine/ansi.h defines _BSD_WCHAR_T_
  --   instead of _WCHAR_T_, and _BSD_RUNE_T_ (which, unlike the other
  --   symbols in the _FOO_T_ family, stays defined even after its
  --   corresponding type is defined).  If we define wchar_t, then we
  --   must undef _WCHAR_T_; for BSD/386 1.1 (and perhaps others), if
  --   we undef _WCHAR_T_, then we must also define rune_t, since 
  --   headers like runetype.h assume that if machine/ansi.h is included,
  --   and _BSD_WCHAR_T_ is not defined, then rune_t is available.
  --   machine/ansi.h says, "Note that _WCHAR_T_ and _RUNE_T_ must be of
  --   the same type."  

  -- Why is this file so hard to maintain properly?  In contrast to
  --   the comment above regarding BSD/386 1.1, on FreeBSD for as long
  --   as the symbol has existed, _BSD_RUNE_T_ must not stay defined or
  --   redundant typedefs will occur when stdlib.h is included after this file.  

  -- FreeBSD 5 can't be handled well using "traditional" logic above
  --   since it no longer defines _BSD_RUNE_T_ yet still desires to export
  --   rune_t in some cases...  

  --  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
  --    are already defined.   

  --  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.   
  --  The references to _GCC_PTRDIFF_T_, _GCC_SIZE_T_, and _GCC_WCHAR_T_
  --    are probably typos and should be removed before 2.8 is released.   

  --  The following ones are the real ones.   
  -- A null pointer constant.   
  -- Offset of member MEMBER in a struct of type TYPE.  
end stddef_h;
