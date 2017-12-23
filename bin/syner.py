#!/usr/bin/env python2
import paramiko
from sys import argv
import os

sport = 24800
hosts = argv[1:]

for host in hosts:
    if host == '.' or host == 'localhost':
        pass
    else:
        ssh = paramiko.SSHClient()
        ssh.load_host_keys(os.path.expanduser('~/.ssh/known_hosts'))
        ssh.connect(host)
        t = ssh.get_transport()
        t.request_port_forward('', sport)
        t.open_forwarded_tcpip_channel(sport, ('localhost', sport))
