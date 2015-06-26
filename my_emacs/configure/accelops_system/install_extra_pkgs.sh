#!/bin/bash
id=$(id -u)
if [ $id -ne 0 ]; then
  echo "Only root can run"
  exit 1
fi

yum upgrade -y cyrus-sasl*
yum install -y openldap-devel.x86_64
yum install -y openldap-devel.i386
yum install -y nmap
yum install -y net-snmp
yum install -y expect
yum install -y curl
yum install -y rpm-libs
yum install -y rsyslog
/sbin/chkconfig syslog off
/sbin/service syslog stop
cd /opt
tar xzvf /pub/release/jdk.tgz
rm -f /usr/bin/java
ln -sf /opt/Java/bin/java /usr/bin/java
ln -sf /opt/Java/bin/javac /usr/bin/javac


