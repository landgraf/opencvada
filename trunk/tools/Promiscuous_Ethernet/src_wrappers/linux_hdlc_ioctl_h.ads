with Interfaces.C; use Interfaces.C;

package linux_hdlc_ioctl_h is


  GENERIC_HDLC_VERSION : constant := 4;  --  /usr/include/linux/hdlc/ioctl.h:5

  CLOCK_DEFAULT : constant := 0;  --  /usr/include/linux/hdlc/ioctl.h:7
  CLOCK_EXT : constant := 1;  --  /usr/include/linux/hdlc/ioctl.h:8
  CLOCK_INT : constant := 2;  --  /usr/include/linux/hdlc/ioctl.h:9
  CLOCK_TXINT : constant := 3;  --  /usr/include/linux/hdlc/ioctl.h:10
  CLOCK_TXFROMRX : constant := 4;  --  /usr/include/linux/hdlc/ioctl.h:11

  ENCODING_DEFAULT : constant := 0;  --  /usr/include/linux/hdlc/ioctl.h:14
  ENCODING_NRZ : constant := 1;  --  /usr/include/linux/hdlc/ioctl.h:15
  ENCODING_NRZI : constant := 2;  --  /usr/include/linux/hdlc/ioctl.h:16
  ENCODING_FM_MARK : constant := 3;  --  /usr/include/linux/hdlc/ioctl.h:17
  ENCODING_FM_SPACE : constant := 4;  --  /usr/include/linux/hdlc/ioctl.h:18
  ENCODING_MANCHESTER : constant := 5;  --  /usr/include/linux/hdlc/ioctl.h:19

  PARITY_DEFAULT : constant := 0;  --  /usr/include/linux/hdlc/ioctl.h:22
  PARITY_NONE : constant := 1;  --  /usr/include/linux/hdlc/ioctl.h:23
  PARITY_CRC16_PR0 : constant := 2;  --  /usr/include/linux/hdlc/ioctl.h:24
  PARITY_CRC16_PR1 : constant := 3;  --  /usr/include/linux/hdlc/ioctl.h:25
  PARITY_CRC16_PR0_CCITT : constant := 4;  --  /usr/include/linux/hdlc/ioctl.h:26
  PARITY_CRC16_PR1_CCITT : constant := 5;  --  /usr/include/linux/hdlc/ioctl.h:27
  PARITY_CRC32_PR0_CCITT : constant := 6;  --  /usr/include/linux/hdlc/ioctl.h:28
  PARITY_CRC32_PR1_CCITT : constant := 7;  --  /usr/include/linux/hdlc/ioctl.h:29

  LMI_DEFAULT : constant := 0;  --  /usr/include/linux/hdlc/ioctl.h:31
  LMI_NONE : constant := 1;  --  /usr/include/linux/hdlc/ioctl.h:32
  LMI_ANSI : constant := 2;  --  /usr/include/linux/hdlc/ioctl.h:33
  LMI_CCITT : constant := 3;  --  /usr/include/linux/hdlc/ioctl.h:34
  LMI_CISCO : constant := 4;  --  /usr/include/linux/hdlc/ioctl.h:35

  -- bits per second  
   type sync_serial_settings is record
      clock_rate : aliased unsigned;  -- /usr/include/linux/hdlc/ioctl.h:38:15
      clock_type : aliased unsigned;  -- /usr/include/linux/hdlc/ioctl.h:39:15
      loopback : aliased unsigned_short;  -- /usr/include/linux/hdlc/ioctl.h:40:17
   end record;
   pragma Convention (C, sync_serial_settings);  -- /usr/include/linux/hdlc/ioctl.h:41:3

   --  skipped anonymous struct anon_20

  -- internal, external, TX-internal etc.  
  -- V.35, V.24, X.21  
  -- bits per second  
   type te1_settings is record
      clock_rate : aliased unsigned;  -- /usr/include/linux/hdlc/ioctl.h:44:15
      clock_type : aliased unsigned;  -- /usr/include/linux/hdlc/ioctl.h:45:15
      loopback : aliased unsigned_short;  -- /usr/include/linux/hdlc/ioctl.h:46:17
      slot_map : aliased unsigned;  -- /usr/include/linux/hdlc/ioctl.h:47:15
   end record;
   pragma Convention (C, te1_settings);  -- /usr/include/linux/hdlc/ioctl.h:48:3

   --  skipped anonymous struct anon_21

  -- internal, external, TX-internal etc.  
  -- T1, E1  
   type raw_hdlc_proto is record
      encoding : aliased unsigned_short;  -- /usr/include/linux/hdlc/ioctl.h:51:17
      parity : aliased unsigned_short;  -- /usr/include/linux/hdlc/ioctl.h:52:17
   end record;
   pragma Convention (C, raw_hdlc_proto);  -- /usr/include/linux/hdlc/ioctl.h:53:3

   --  skipped anonymous struct anon_22

   type fr_proto is record
      t391 : aliased unsigned;  -- /usr/include/linux/hdlc/ioctl.h:56:15
      t392 : aliased unsigned;  -- /usr/include/linux/hdlc/ioctl.h:57:15
      n391 : aliased unsigned;  -- /usr/include/linux/hdlc/ioctl.h:58:15
      n392 : aliased unsigned;  -- /usr/include/linux/hdlc/ioctl.h:59:15
      n393 : aliased unsigned;  -- /usr/include/linux/hdlc/ioctl.h:60:15
      lmi : aliased unsigned_short;  -- /usr/include/linux/hdlc/ioctl.h:61:17
      dce : aliased unsigned_short;  -- /usr/include/linux/hdlc/ioctl.h:62:17
   end record;
   pragma Convention (C, fr_proto);  -- /usr/include/linux/hdlc/ioctl.h:63:3

   --  skipped anonymous struct anon_23

  -- 1 for DCE (network side) operation  
   type fr_proto_pvc is record
      dlci : aliased unsigned;  -- /usr/include/linux/hdlc/ioctl.h:66:15
   end record;
   pragma Convention (C, fr_proto_pvc);  -- /usr/include/linux/hdlc/ioctl.h:67:3

   --  skipped anonymous struct anon_24

  -- for creating/deleting FR PVCs  
   type fr_proto_pvc_info_master_array is array (0 .. 15) of aliased char;
   type fr_proto_pvc_info is record
      dlci : aliased unsigned;  -- /usr/include/linux/hdlc/ioctl.h:70:15
      master : aliased fr_proto_pvc_info_master_array;  -- /usr/include/linux/hdlc/ioctl.h:71:22
   end record;
   pragma Convention (C, fr_proto_pvc_info);  -- /usr/include/linux/hdlc/ioctl.h:72:2

   --  skipped anonymous struct anon_25

  -- Name of master FRAD device  
  -- for returning PVC information only  
   type cisco_proto is record
      interval : aliased unsigned;  -- /usr/include/linux/hdlc/ioctl.h:75:18
      timeout : aliased unsigned;  -- /usr/include/linux/hdlc/ioctl.h:76:18
   end record;
   pragma Convention (C, cisco_proto);  -- /usr/include/linux/hdlc/ioctl.h:77:3

   --  skipped anonymous struct anon_26

  -- PPP doesn't need any info now - supply length = 0 to ioctl  
end linux_hdlc_ioctl_h;
