#!/bin/bash
id=$(id -u)
if [ $id -ne 0 ]; then
  echo "Only root can run"
  exit 1
fi

cd /
mkdir /pub

cd /opt
mkdir /opt/phoenix
mkdir /opt/phoenix/bin
mkdir /opt/phoenix/deployment
chown -R admin.admin /opt/phoenix

cd /home
mkdir /home/phoenix_dev
chown -R admin.admin /home/phoenix_dev
su -l admin -c "mkdir /home/phoenix_dev/projects"
su -l admin -c "mkdir /home/phoenix_dev/projects/trunk"
su -l admin -c "mkdir /home/phoenix_dev/projects/trunk/phoenix"
su -l admin -c "ln -s /home/phoenix_dev/projects/trunk/phoenix /home/phoenix_dev/projects/phoenix"
