#!/bin/bash
id=$(id -u)
if [ $id -ne 0 ]; then
  echo "Only root can run"
  exit 1
fi

cd /opt
tar xzvf /pub/release/glassfish.tgz
chown -R admin.admin /opt/glassfish/*
yum install -y subversion.x86_64
yum install -y postgresql-server
yum install -y mod_auth_pgsql
yum install -y mod_dav_svn
yum install -y mod_ssl
cd /etc/httpd
tar xzvf /pub/release/svn.tgz
/sbin/service postgresql initdb
/sbin/service postgresql start
cd /var/lib/pgsql/data
cp /pub/release/postgres/* .

#create svn
mkdir /data
chown -R admin.admin /data
mkdir /data/svn
svnadmin create /data/svn/repos
svn mkdir -m "initial version" file:///data/svn/repos/cmdb
svn ls file:///data/svn/repos/cmdb
chown -R apache.apache /data/svn

## svnadmin create /data/svn/repos
## svn mkdir -m "" file:///data/svn/repos/cmdb

