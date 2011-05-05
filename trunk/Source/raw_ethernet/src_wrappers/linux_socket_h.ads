with Interfaces.C; use Interfaces.C;

package linux_socket_h is

  -- * Desired design of maximum size and alignment (see RFC2553)
  --  

  -- Implementation specific desired alignment  
  -- address family  
   type uu_kernel_sockaddr_storage_uu_data_array is array (0 .. 125) of aliased char;
   type uu_kernel_sockaddr_storage is record
      ss_family : aliased unsigned_short;  -- /usr/include/linux/socket.h:12:17
      uu_data : aliased uu_kernel_sockaddr_storage_uu_data_array;  -- /usr/include/linux/socket.h:14:53
   end record;
   pragma Convention (C, uu_kernel_sockaddr_storage);  -- /usr/include/linux/socket.h:11:34

  -- Following field(s) are implementation specific  
  -- space to achieve desired size,  
  -- _SS_MAXSIZE value minus size of ss_family  
  -- force desired alignment  
  -- 		 
  -- *	1003.1g requires sa_family_t and that sa_data is char.
  --  

  -- address family, AF_xxx	 
  -- 14 bytes of protocol address	 
  -- Linger active		 
  -- How long to linger for	 
  -- *	As we do 4.4BSD message passing we use a 4.4BSD message passing
  -- *	system, not 4.3. Thus msg_accrights(len) are now missing. They
  -- *	belong in an obscure libc emulation or the bin.
  --  

  -- Socket name			 
  -- Length of name		 
  -- Data blocks			 
  -- Number of blocks		 
  -- Per protocol magic (eg BSD file descriptor passing)  
  -- Length of cmsg list  
  -- *	POSIX 1003.1g - ancillary data object information
  -- *	Ancillary data consits of a sequence of pairs of
  -- *	(cmsghdr, cmsg_data[])
  --  

  -- data byte count, including hdr  
  -- originating protocol  
  -- protocol-specific type  
  -- *	Ancilliary data object information MACROS
  -- *	Table 5-14 of POSIX 1003.1g
  --  

  -- *	This mess will go away with glibc
  --  

  -- *	Get the next cmsg header
  -- *
  -- *	PLEASE, do not touch this function. If you think, that it is
  -- *	incorrect, grep kernel sources and think about consequences
  -- *	before trying to improve it.
  -- *
  -- *	Now it always returns valid, not truncated ancillary object
  -- *	HEADER. But caller still MUST check, that cmsg->cmsg_len is
  -- *	inside range, given by msg->msg_controllen before using
  -- *	ancillary object DATA.				--ANK (980731)
  --  

  -- "Socket"-level control message types:  
  -- Supported address families.  
  -- Protocol families, same as address families.  
  -- Maximum queue length specifiable by listen.   
  -- Flags we can use with send/ and recv. 
  --   Added those for 1003.1g not all are supported yet
  --  

  -- Setsockoptions(2) level. Thanks to BSD these must match IPPROTO_xxx  
  -- #define SOL_ICMP	1	No-no-no! Due to Linux :-) we cannot use SOL_ICMP=1  
  -- IPX options  
end linux_socket_h;
