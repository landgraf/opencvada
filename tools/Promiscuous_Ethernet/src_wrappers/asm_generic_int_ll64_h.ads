with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

package asm_generic_int_ll64_h is

  -- * asm-generic/int-ll64.h
  -- *
  -- * Integer declarations for architectures which use "long long"
  -- * for 64-bit types.
  --  

  -- * __xx is ok: it doesn't pollute the POSIX namespace. Use these in the
  -- * header files exported to user space
  --  

   subtype uu_s8 is signed_char;  -- /usr/include/asm-generic/int-ll64.h:17:25

   subtype uu_u8 is unsigned_char;  -- /usr/include/asm-generic/int-ll64.h:18:23

   subtype uu_s16 is short;  -- /usr/include/asm-generic/int-ll64.h:20:26

   subtype uu_u16 is unsigned_short;  -- /usr/include/asm-generic/int-ll64.h:21:24

   subtype uu_s32 is int;  -- /usr/include/asm-generic/int-ll64.h:23:24

   subtype uu_u32 is unsigned;  -- /usr/include/asm-generic/int-ll64.h:24:22

   subtype uu_s64 is Long_Long_Integer;  -- /usr/include/asm-generic/int-ll64.h:27:44

   subtype uu_u64 is Extensions.unsigned_long_long;  -- /usr/include/asm-generic/int-ll64.h:28:42

end asm_generic_int_ll64_h;
