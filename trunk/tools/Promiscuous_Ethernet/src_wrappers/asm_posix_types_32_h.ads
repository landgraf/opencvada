with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package asm_posix_types_32_h is

  -- * This file is generally used by user-level software, so you need to
  -- * be a little careful about namespace pollution etc.  Also, we cannot
  -- * assume GCC is being used.
  --  

   subtype uu_kernel_ino_t is unsigned_long;  -- /usr/include/asm/posix_types_32.h:10:23

   subtype uu_kernel_mode_t is unsigned_short;  -- /usr/include/asm/posix_types_32.h:11:24

   subtype uu_kernel_nlink_t is unsigned_short;  -- /usr/include/asm/posix_types_32.h:12:24

   subtype uu_kernel_off_t is long;  -- /usr/include/asm/posix_types_32.h:13:15

   subtype uu_kernel_pid_t is int;  -- /usr/include/asm/posix_types_32.h:14:14

   subtype uu_kernel_ipc_pid_t is unsigned_short;  -- /usr/include/asm/posix_types_32.h:15:24

   subtype uu_kernel_uid_t is unsigned_short;  -- /usr/include/asm/posix_types_32.h:16:24

   subtype uu_kernel_gid_t is unsigned_short;  -- /usr/include/asm/posix_types_32.h:17:24

   subtype uu_kernel_size_t is unsigned;  -- /usr/include/asm/posix_types_32.h:18:22

   subtype uu_kernel_ssize_t is int;  -- /usr/include/asm/posix_types_32.h:19:14

   subtype uu_kernel_ptrdiff_t is int;  -- /usr/include/asm/posix_types_32.h:20:14

   subtype uu_kernel_time_t is long;  -- /usr/include/asm/posix_types_32.h:21:15

   subtype uu_kernel_suseconds_t is long;  -- /usr/include/asm/posix_types_32.h:22:15

   subtype uu_kernel_clock_t is long;  -- /usr/include/asm/posix_types_32.h:23:15

   subtype uu_kernel_timer_t is int;  -- /usr/include/asm/posix_types_32.h:24:14

   subtype uu_kernel_clockid_t is int;  -- /usr/include/asm/posix_types_32.h:25:14

   subtype uu_kernel_daddr_t is int;  -- /usr/include/asm/posix_types_32.h:26:14

   type uu_kernel_caddr_t is new Interfaces.C.Strings.chars_ptr;  -- /usr/include/asm/posix_types_32.h:27:17

   subtype uu_kernel_uid16_t is unsigned_short;  -- /usr/include/asm/posix_types_32.h:28:24

   subtype uu_kernel_gid16_t is unsigned_short;  -- /usr/include/asm/posix_types_32.h:29:24

   subtype uu_kernel_uid32_t is unsigned;  -- /usr/include/asm/posix_types_32.h:30:22

   subtype uu_kernel_gid32_t is unsigned;  -- /usr/include/asm/posix_types_32.h:31:22

   subtype uu_kernel_old_uid_t is unsigned_short;  -- /usr/include/asm/posix_types_32.h:33:24

   subtype uu_kernel_old_gid_t is unsigned_short;  -- /usr/include/asm/posix_types_32.h:34:24

   subtype uu_kernel_old_dev_t is unsigned_short;  -- /usr/include/asm/posix_types_32.h:35:24

   subtype uu_kernel_loff_t is Long_Long_Integer;  -- /usr/include/asm/posix_types_32.h:38:19

   type uu_kernel_fsid_t_val_array is array (0 .. 1) of aliased int;
   type uu_kernel_fsid_t is record
      val : aliased uu_kernel_fsid_t_val_array;  -- /usr/include/asm/posix_types_32.h:42:11
   end record;
   pragma Convention (C, uu_kernel_fsid_t);  -- /usr/include/asm/posix_types_32.h:43:3

   --  skipped anonymous struct anon_19

end asm_posix_types_32_h;
