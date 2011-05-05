with Interfaces.C; use Interfaces.C;
with linux_types_h;
with asm_generic_int_ll64_h;

package linux_if_packet_h is


  PACKET_HOST : constant := 0;  --  /usr/include/linux/if_packet.h:26
  PACKET_BROADCAST : constant := 1;  --  /usr/include/linux/if_packet.h:27
  PACKET_MULTICAST : constant := 2;  --  /usr/include/linux/if_packet.h:28
  PACKET_OTHERHOST : constant := 3;  --  /usr/include/linux/if_packet.h:29
  PACKET_OUTGOING : constant := 4;  --  /usr/include/linux/if_packet.h:30

  PACKET_LOOPBACK : constant := 5;  --  /usr/include/linux/if_packet.h:32
  PACKET_FASTROUTE : constant := 6;  --  /usr/include/linux/if_packet.h:33

  PACKET_ADD_MEMBERSHIP : constant := 1;  --  /usr/include/linux/if_packet.h:37
  PACKET_DROP_MEMBERSHIP : constant := 2;  --  /usr/include/linux/if_packet.h:38
  PACKET_RECV_OUTPUT : constant := 3;  --  /usr/include/linux/if_packet.h:39

  PACKET_RX_RING : constant := 5;  --  /usr/include/linux/if_packet.h:41
  PACKET_STATISTICS : constant := 6;  --  /usr/include/linux/if_packet.h:42
  PACKET_COPY_THRESH : constant := 7;  --  /usr/include/linux/if_packet.h:43
  PACKET_AUXDATA : constant := 8;  --  /usr/include/linux/if_packet.h:44
  PACKET_ORIGDEV : constant := 9;  --  /usr/include/linux/if_packet.h:45
  PACKET_VERSION : constant := 10;  --  /usr/include/linux/if_packet.h:46
  PACKET_HDRLEN : constant := 11;  --  /usr/include/linux/if_packet.h:47
  PACKET_RESERVE : constant := 12;  --  /usr/include/linux/if_packet.h:48

  TP_STATUS_KERNEL : constant := 0;  --  /usr/include/linux/if_packet.h:69
  TP_STATUS_USER : constant := 1;  --  /usr/include/linux/if_packet.h:70
  TP_STATUS_COPY : constant := 2;  --  /usr/include/linux/if_packet.h:71
  TP_STATUS_LOSING : constant := 4;  --  /usr/include/linux/if_packet.h:72
  TP_STATUS_CSUMNOTREADY : constant := 8;  --  /usr/include/linux/if_packet.h:73

  TPACKET_ALIGNMENT : constant := 16;  --  /usr/include/linux/if_packet.h:82
  --  arg-macro: function TPACKET_ALIGN (x)
  --    return ((x)+TPACKET_ALIGNMENT-1)and~(TPACKET_ALIGNMENT-1);
  --  unsupported macro: TPACKET_HDRLEN (TPACKET_ALIGN(sizeof(struct tpacket_hdr)) + sizeof(struct sockaddr_ll))
  --  unsupported macro: TPACKET2_HDRLEN (TPACKET_ALIGN(sizeof(struct tpacket2_hdr)) + sizeof(struct sockaddr_ll))

  PACKET_MR_MULTICAST : constant := 0;  --  /usr/include/linux/if_packet.h:135
  PACKET_MR_PROMISC : constant := 1;  --  /usr/include/linux/if_packet.h:136
  PACKET_MR_ALLMULTI : constant := 2;  --  /usr/include/linux/if_packet.h:137

   type sockaddr_pkt_spkt_device_array is array (0 .. 13) of aliased unsigned_char;
   type sockaddr_pkt is record
      spkt_family : aliased unsigned_short;  -- /usr/include/linux/if_packet.h:8:17
      spkt_device : aliased sockaddr_pkt_spkt_device_array;  -- /usr/include/linux/if_packet.h:9:30
      spkt_protocol : aliased linux_types_h.uu_be16;  -- /usr/include/linux/if_packet.h:10:9
   end record;
   pragma Convention (C, sockaddr_pkt);  -- /usr/include/linux/if_packet.h:7:1

   type sockaddr_ll_sll_addr_array is array (0 .. 7) of aliased unsigned_char;
   type sockaddr_ll is record
      sll_family : aliased unsigned_short;  -- /usr/include/linux/if_packet.h:15:17
      sll_protocol : aliased linux_types_h.uu_be16;  -- /usr/include/linux/if_packet.h:16:10
      sll_ifindex : aliased int;  -- /usr/include/linux/if_packet.h:17:7
      sll_hatype : aliased unsigned_short;  -- /usr/include/linux/if_packet.h:18:17
      sll_pkttype : aliased unsigned_char;  -- /usr/include/linux/if_packet.h:19:16
      sll_halen : aliased Unsigned_Short;  -- /usr/include/linux/if_packet.h:20:16
      sll_addr : aliased sockaddr_ll_sll_addr_array;  -- /usr/include/linux/if_packet.h:21:26
   end record;
   pragma Convention (C, sockaddr_ll);  -- /usr/include/linux/if_packet.h:14:1

  -- Packet types
  -- These ones are invisible by user level
  -- Packet socket options
  -- Value 4 is still used by obsolete turbo-packet.
   type tpacket_stats is record
      tp_packets : aliased unsigned;  -- /usr/include/linux/if_packet.h:52:15
      tp_drops : aliased unsigned;  -- /usr/include/linux/if_packet.h:53:15
   end record;
   pragma Convention (C, tpacket_stats);  -- /usr/include/linux/if_packet.h:51:1

   type tpacket_auxdata is record
      tp_status : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/if_packet.h:58:9
      tp_len : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/if_packet.h:59:9
      tp_snaplen : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/if_packet.h:60:9
      tp_mac : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/if_packet.h:61:9
      tp_net : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/if_packet.h:62:9
      tp_vlan_tci : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/if_packet.h:63:9
   end record;
   pragma Convention (C, tpacket_auxdata);  -- /usr/include/linux/if_packet.h:57:1

   type tpacket_hdr is record
      tp_status : aliased unsigned_long;  -- /usr/include/linux/if_packet.h:68:16
      tp_len : aliased unsigned;  -- /usr/include/linux/if_packet.h:74:15
      tp_snaplen : aliased unsigned;  -- /usr/include/linux/if_packet.h:75:15
      tp_mac : aliased unsigned_short;  -- /usr/include/linux/if_packet.h:76:17
      tp_net : aliased unsigned_short;  -- /usr/include/linux/if_packet.h:77:17
      tp_sec : aliased unsigned;  -- /usr/include/linux/if_packet.h:78:15
      tp_usec : aliased unsigned;  -- /usr/include/linux/if_packet.h:79:15
   end record;
   pragma Convention (C, tpacket_hdr);  -- /usr/include/linux/if_packet.h:67:1

   type tpacket2_hdr is record
      tp_status : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/if_packet.h:88:9
      tp_len : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/if_packet.h:89:9
      tp_snaplen : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/if_packet.h:90:9
      tp_mac : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/if_packet.h:91:9
      tp_net : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/if_packet.h:92:9
      tp_sec : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/if_packet.h:93:9
      tp_nsec : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/if_packet.h:94:9
      tp_vlan_tci : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/if_packet.h:95:9
   end record;
   pragma Convention (C, tpacket2_hdr);  -- /usr/include/linux/if_packet.h:87:1

   subtype tpacket_versions is unsigned;
   TPACKET_V1 : constant tpacket_versions := 0;
   TPACKET_V2 : constant tpacket_versions := 1;  -- /usr/include/linux/if_packet.h:100:6

  --   Frame structure:
  --   - Start. Frame must be aligned to TPACKET_ALIGNMENT=16
  --   - struct tpacket_hdr
  --   - pad to TPACKET_ALIGNMENT=16
  --   - struct sockaddr_ll
  --   - Gap, chosen so that packet data (Start+tp_net) alignes to TPACKET_ALIGNMENT=16
  --   - Start+tp_mac: [ Optional MAC header ]
  --   - Start+tp_net: Packet data, aligned to TPACKET_ALIGNMENT=16.
  --   - Pad to align to TPACKET_ALIGNMENT=16
  --

  -- Minimal size of contiguous block
   type tpacket_req is record
      tp_block_size : aliased unsigned;  -- /usr/include/linux/if_packet.h:121:15
      tp_block_nr : aliased unsigned;  -- /usr/include/linux/if_packet.h:122:15
      tp_frame_size : aliased unsigned;  -- /usr/include/linux/if_packet.h:123:15
      tp_frame_nr : aliased unsigned;  -- /usr/include/linux/if_packet.h:124:15
   end record;
   pragma Convention (C, tpacket_req);  -- /usr/include/linux/if_packet.h:120:1

  -- Number of blocks
  -- Size of frame
  -- Total number of frames
   type packet_mreq_mr_address_array is array (0 .. 7) of aliased unsigned_char;
   type packet_mreq is record
      mr_ifindex : aliased int;  -- /usr/include/linux/if_packet.h:129:7
      mr_type : aliased unsigned_short;  -- /usr/include/linux/if_packet.h:130:17
      mr_alen : aliased unsigned_short;  -- /usr/include/linux/if_packet.h:131:17
      mr_address : aliased packet_mreq_mr_address_array;  -- /usr/include/linux/if_packet.h:132:28
   end record;
   pragma Convention (C, packet_mreq);  -- /usr/include/linux/if_packet.h:128:1

end linux_if_packet_h;
