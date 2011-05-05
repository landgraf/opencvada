with Interfaces.C; use Interfaces.C;
with asm_generic_int_ll64_h;

package linux_types_h is

  -- * The following typedefs are also protected by individual ifdefs for
  -- * historical reasons:
  --  

  -- bsd  
  -- sysv  
  -- this is a special 64bit data type that is 8-byte aligned  
  --*
  -- * The type used for indexing onto a disc or disc partition.
  -- *
  -- * Linux always considers sectors to be 512 bytes long independently
  -- * of the devices real block size.
  --  

  -- * The type of the inode's block count.
  --  

  -- * The type of an index into the pagecache.  Use a #define so asm/types.h
  -- * can override it.
  --  

  -- * Below are truly Linux-specific types that should never collide with
  -- * any application/library that wants linux/types.h.
  --  

   subtype uu_le16 is asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/types.h:153:25

   subtype uu_be16 is asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/types.h:154:25

   subtype uu_le32 is asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/types.h:155:25

   subtype uu_be32 is asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/types.h:156:25

   subtype uu_le64 is asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/types.h:158:25

   subtype uu_be64 is asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/types.h:159:25

   subtype uu_sum16 is asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/types.h:161:25

   subtype uu_wsum is asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/types.h:162:25

end linux_types_h;
