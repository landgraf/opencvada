#include <pcap.h>
void packet_handler(u_char * param,const struct pcap_pkthdr * header, const u_char *pkt_data);

pcap_t *handle;

int main(void){
  pcap_if_t * alldevs, *d;
  char * name = "rpcap://\\Device\\NPF_{BFBE5489-7479-4C9D-8522-7B05E5A90C47}";
  char errbuf[PCAP_ERRBUF_SIZE];
  pcap_findalldevs_ex("rpcap://",NULL,&alldevs,errbuf);

  for (d = alldevs; d != NULL; d = d->next){
    printf("%s", d->name);
    printf("\t(%s)\n", d->description);
  }


  handle = (pcap_t*)pcap_open(name,65536,1,1000,NULL,errbuf);

  pcap_loop(handle,0,packet_handler,NULL);


  pcap_freealldevs(alldevs);
}

u_char * create_raw_enet_header(u_char src[6], u_char dest[6], int length){
  int i;
  u_char * head = (u_char*)malloc(sizeof(char)*14);//[14];
  for (i = 0; i < 6; i++)
    head[i] = dest[i];

  for (i = 0; i < 6; i++)
    head[i+6] = src[i];

  head[12] = (u_char)((length >> 8) & 0xFF);
  head[13] = (u_char)((length) & 0xFF);

  return head;
}

void packet_handler(u_char * param,const struct pcap_pkthdr * header, const u_char *pkt_data){
  int i;
  printf("%d\t%d\n", header->len,header->caplen);
  printf("%x:%x:%x:%x:%x:%x\n", pkt_data[0],pkt_data[1],pkt_data[2],pkt_data[3],pkt_data[4],pkt_data[5]);
  printf("%x:%x:%x:%x:%x:%x\n", pkt_data[6],pkt_data[7],pkt_data[8],pkt_data[9],pkt_data[10],pkt_data[11]);
  /*   packat type 12,13 */
  printf("%x %x\n\n", pkt_data[12],pkt_data[13]);
  u_char * src_addr = (u_char*)malloc(sizeof(char)*6);
  u_char * dest_addr = (u_char*)malloc(sizeof(char)*6);
  for (i = 0; i < 6; i++)
    src_addr[i] = pkt_data[i];
  for (i = 0; i < 6; i++)
    dest_addr[i] = pkt_data[i+6];

  u_char * buf = create_raw_enet_header(dest_addr,src_addr,0);

  if (pcap_sendpacket(handle,buf,14) == -1)
    printf("fel\n");
}
