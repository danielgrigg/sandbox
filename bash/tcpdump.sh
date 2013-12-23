sudo tcpdump -w /tmp/pcap.ssl -ni en0 -s0 host 202.4.162.99 and port 443
sudo tcpdump -q -nn -A -i lo0 port 8080 and dst port 8080
