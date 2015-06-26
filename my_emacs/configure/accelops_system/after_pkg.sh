#!/bin/bash
id=$(id -u)
if [ $id -ne 0 ]; then
  echo "Only root can run"
  exit 1
fi

#sysctlFile="/etc/etc_sysctl.conf.el5x64"
#cp -f $sysctlFile /etc/sysctl.conf
#/sbin/sysctl -p &>/dev/null

# cd /etc/httpd/conf/
# chcon -t httpd_config_t httpd.conf
# /sbin/service httpd restart

# chcon -t texrel_shlib_t /opt/phoenix/lib32/libcpbcrypt.so

chown -R admin.admin /data
