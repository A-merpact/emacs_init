#!/bin/bash

upgrade_schema() {

    local BACKUP_DIR=/tmp/backup
    local DB_BACKUP_DIR=/tmp/db 
    local DB_SCHEMA_LOG=/tmp/dbschema.log
    
    if [ -d $DB_BACKUP_DIR ] ; then
        /bin/rm -rf $DB_BACKUP_DIR 
    fi

    /bin/mkdir -p $DB_BACKUP_DIR 
    chown -R postgres:postgres $DB_BACKUP_DIR 

    if [ -f $DB_SCHEMA_LOG ] ; then
        /bin/rm -f $DB_SCHEMA_LOG 
    fi

    su -l postgres -c "/opt/phoenix/deployment/db_upgrade_nonva.sh $DB_BACKUP_DIR $DBSVR_VIP ";
    if [ $? != 0 ] ; then
	echo "Error: failed to upgrade db schema"
        exit 1
    fi
}


prep_user_files() {

    cat > /tmp/sys_conf_cust.cmd << EOF
{
s/<value>SVN_URL_VALUE<\/value>/<value>$SVN_HOST_ESC<\/value>/g
s/<value>SVN_USERNAME_VALUE<\/value>/<value>$SVN_USERNAME<\/value>/g
s/<value>SVN_PASSWORD_VALUE<\/value>/<value>$SVN_PASSWORD<\/value>/g
s/<value>SYSTEM_NAME_VALUE<\/value>/<value>$SYSTEM_NAME<\/value>/g
}
EOF

    sed -f /tmp/sys_conf_cust.cmd /opt/phoenix/data-definition/systemConfigs.xml > /tmp/systemConfigs.xml
    mv -f /tmp/systemConfigs.xml /opt/phoenix/data-definition
    rm -f /tmp/sys_conf_cust.cmd

    sed -e "s/Prospect@Hi123/$AGENT_PASSWD/g" /opt/phoenix/data-definition/Cust-Super.xml > /tmp/Cust-Super.xml
    rm /opt/phoenix/data-definition/Cust-Super.xml
    mv /tmp/Cust-Super.xml /opt/phoenix/data-definition/

    sed -e "s/Prospect@Hi123/$AGENT_PASSWD/g" /opt/phoenix/data-definition/Cust-Test.xml > /tmp/Cust-Test.xml
    rm /opt/phoenix/data-definition/Cust-Test.xml
    mv /tmp/Cust-Test.xml /opt/phoenix/data-definition/

    if [ "$POPULATE_MAIL_ACCOUNT" = "true" ]; then
      cat > /tmp/mail_systemConfigs.xml <<EOF
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<config>
    <systemConfigs>
        <systemConfig>
            <category>Message</category>
            <name>Mail_Server</name>
            <value>smtpx16.msoutlookonline.net</value>
        </systemConfig>
        <systemConfig>
            <category>Message</category>
            <name>Mail_Account_User</name>
            <value>notify@accelops.net</value>
        </systemConfig>
        <systemConfig>
            <category>Message</category>
            <name>Mail_Account_Password</name>
            <value>(jWohE68N</value>
            <sensitive>true</sensitive>
        </systemConfig>
    </systemConfigs>
</config>
EOF
    fi
}

clean_user_files() {
    rm /opt/phoenix/data-definition/Cust-Super.xml
    rm /opt/phoenix/data-definition/Cust-Test.xml
    rm /opt/phoenix/data-definition/systemConfigs.xml
    rm /tmp/mail_systemConfigs.xml &>/dev/null
}

provision_cust() {
    id=$1
    ci=$2
    oda=$3
    eps=$4
    cid=$5
    # convert into GB
    oda=$(( oda * 1024 * 1024 * 1024 ))
    sql="insert into ph_sys_cust_res (id,creation_time,cust_org_id,owner_id,config_item,disk_quote,eps,target_cust_id) values ($id,0,0,0,$ci,$oda,$eps,$cid)"
    su -l postgres -c "psql -h $DBSVR_VIP -U phoenix -c \"$sql\" $DBNAME"
}

insert_into_db() {
  sql="insert into ph_sys_server (id, creation_time, last_modified_time, cust_org_id, owner_id, eps, ip_addr, mode, active) values ($1,0,0,0,0,0,'$2',$3,$4)"
  su -l postgres -c "psql -h $DBSVR_VIP -U phoenix -c \"$sql\" $DBNAME"
}

insert_phoenix_server() {
  svrId=1;

  ## insert supervisor
  insert_into_db $svrId $SUPERVISOR_IP 2 true
  svrId=$(( svrId + 1 ))

  if [ -n "$BACKUP_SUPERVISOR_IP" ]; then
    insert_into_db $svrId $BACKUP_SUPERVISOR_IP 2 false
    svrId=$(( svrId + 1 ))
  fi

  ## insert workers
  if [ -n "$WORKER_IP" ]; then
    for wkrIp in ${WORKER_IP_ARR[@]}; do
      insert_into_db $svrId $wkrIp 1 true
      svrId=$(( svrId + 1 ))
    done
  fi

  ## generic servers
  if [ "$APPSVR_IP" != "$SUPERVISOR_IP" ]; then
    for appIp in ${APPSVR_IP_ARR[@]}; do
      insert_into_db $svrId $appIp 0 true
      svrId=$(( svrId + 1 ))
    done
  fi

  if [ "$DBSVR_IP" != "$APPSVR_IP" -a "$DBSVR_IP" != "$SUPERVISOR_IP" ]; then
    for dbIp in ${DBSVR_IP_ARR[@]}; do
      insert_into_db $svrId $dbIp 0 true
      svrId=$(( svrId + 1 ))
    done
  fi
}

GLASSFISH_HOME=/opt/glassfish
GF_ADMIN_TOOL=$GLASSFISH_HOME/bin/asadmin

## check if running user is root
uid=`id -u`
if [ ${uid} -ne 0 ]; then
  echo "This script must be run as root"
  exit 1;
fi

## get this machine's IP address
myIP=`hostname -i`
if [ $? -ne 0 ]; then
  echo "Hostname not configured correctly, please check /etc/hosts and /etc/sysconfig/network"
  exit 1
fi

if [ ${myIP} = "127.0.0.1" ]; then
  echo "Hostname and DNS lookup not configured correctly, please check /etc/hosts and /etc/sysconfig/network"
  exit 1
fi
echo "Local IP address is $myIP"

## check arguments
if [ $# -lt 2 ]; then 
  echo "Usage: $0 [phx_package] [config_file] [skip_apache_config] {populate_db | upgrade_db] [skip_syslog_conf] [noautorun]"
  exit 1
fi

phxPkg=$1
phxPkgDir=$(dirname $1)

## load configuration file
source $2
if [ $? -ne 0 ]; then
  echo "Bad config file"
  exit 1
fi

populateDb=0
skipApacheConfig=0
upgradeDb=0
noautorun=0
skipSyslogConf=0

shift 2
while [ $# -gt 0 ]; do
  if [ "$1" = "skip_apache_config" ]; then
    skipApacheConfig=1
  elif [ "$1" = "skip_syslog_conf" ]; then
    skipSyslogConf=1
  elif [ "$1" = "populate_db" ]; then
    populateDb=1
    upgradeDb=0
  elif [ "$1" = "upgrade_db" ]; then
    upgradeDb=1
    populateDb=0
  elif [ "$1" = "noautorun" ]; then
    upgradeDb=0
    populdateDb=0
    noautorun=1
  fi
  shift
done

## check configruations
if [ -z ${APPSVR_IP} ]; then
  echo "APPSVR_IP is not configured"
  exit 1
else
  echo "APPSVR_IP is $APPSVR_IP"
  OLD_IFS="$IFS"
  IFS=","
  APPSVR_IP_ARR=( $APPSVR_IP )
  IFS="$OLD_IFS"
fi

if [ -z ${APPSVR_HOSTNAME} ]; then
  APPSVR_HOSTNAME=$APPSVR_IP
fi

if [ -z ${SUPERVISOR_IP} ]; then
  echo "warning: SUPERVISOR_IP is not configured"
else
  echo "SUPERVISOR_IP is $SUPERVISOR_IP"
fi

if [ -z ${WORKER_IP} ]; then
  echo "warning: WORKER_IP is not configured"
else
  echo "WORKER_IP is $WORKER_IP"
  OLD_IFS="$IFS"
  IFS=","
  WORKER_IP_ARR=( $WORKER_IP )
  IFS="$OLD_IFS"
fi

if [ -z ${AGENT_IP} ]; then
  echo "warning: AGENT_IP is not configured"
else
  echo "AGENT_IP is $AGENT_IP"
  OLD_IFS="$IFS"
  IFS=","
  AGENT_IP_ARR=( $AGENT_IP )
  IFS="$OLD_IFS"
fi

if [ -z ${DBSVR_IP} ]; then
  echo "DBSVR_IP not configured, use APPSVR_IP"
  DBSVR_IP=${APPSVR_IP}
  DBSVR_IP_ARR=${APPSVR_IP_ARR}
else
  echo "DBSVR_IP is $DBSVR_IP"
  OLD_IFS="$IFS"
  IFS=","
  DBSVR_IP_ARR=( $DBSVR_IP )
  IFS="$OLD_IFS"
fi

if [ -z ${DBSVR_VIP} ]; then
  DBSVR_VIP=${DBSVR_IP_ARR[0]}
fi

if [ -z ${DBNAME} ]; then
  DBNAME=phoenixdb
fi

if [ -z ${AGENT_PASSWD} ]; then
  echo "AGENT_PASSWD not configured"
  exit 1
fi

## determine BOX_TYPE based on IP
isAppSvr=0
isDBSvr=0
runDBSvr=0

if [ `expr match ${DBSVR_IP} ".*${myIP}"` -gt 0 ]; then
    isDBSvr=1
    if [ "$myIP" = "$DBSVR_VIP" ]; then
        runDBSvr=1
    fi
fi

if [ `expr match ${APPSVR_IP} ".*${myIP}"` -gt 0 ]; then
    isAppSvr=1
fi

echo "SUPERVISOR_IP is $SUPERVISOR_IP Flag"
echo "myIP is $myIP Flag"

if [ "$SUPERVISOR_IP" = "$myIP" ]; then

  echo "Setting up SUPERVISOR machine"
  BOX_TYPE="supervisor"
  MON_ROLE="phMonitorSupervisor"

elif [ "$BACKUP_SUPERVISOR_IP" = "$myIP" ]; then

  echo "Setting up SUPERVISOR (backup) machine"
  BOX_TYPE="supervisor"
  MON_ROLE="phMonitorSupervisor"

elif [ -n "$WORKER_IP" ] && [ `expr match ${WORKER_IP} ".*${myIP}"` -gt 0 ]; then

  echo "Setting up WORKER machine"
  BOX_TYPE="worker"
  MON_ROLE="phMonitorWorker"

elif [ -n "$AGENT_IP" ] && [ `expr match ${AGENT_IP} ".*${myIP}"` -gt 0 ]; then

  echo "Setting up COLLECTOR machine"
  BOX_TYPE="collector"
  MON_ROLE="phMonitorAgent"

else
  if [ $isDBSvr -eq 0 -a $isAppSvr -eq 0 ]; then
    echo "Unable to determine box type: local IP does not appear in SUPERVISOR_IP/WORKER_IP/AGENT_IP/DBSVR_IP/APPSVR_IP"
    exit 1
  fi
  echo "Setting up App/DB machine"
  BOX_TYPE="other"
  MON_ROLE="phMonitorGenericServer"
fi

myAgentID=1
myCustID=1
  
if [ ${BOX_TYPE} = "collector" ]; then
  if [ -z ${EVENT_UPLOAD_IP} ]; then
    echo "EVENT_UPLOAD_IP is not configured"
    exit 1
  fi
  if [ -z ${AGENT_CUST_ID} ]; then
    echo "AGENT_CUST_ID not configured"
    exit 1
  fi
  if [ -z ${AGENT_ID} ]; then
    echo "AGENT_ID not configured"
    exit 1
  fi

  OLD_IFS="$IFS"
  IFS=","
  agentIPArr=( $AGENT_IP )
  agentIDArr=( $AGENT_ID )
  agentCustIDArr=( $AGENT_CUST_ID )
  IFS="$OLD_IFS"

  aipidx=0
  for aip in ${agentIPArr[@]}; do
    if [ "$myIP" = "$aip" ]; then
      agentIdx=${aipidx}
    fi
    (( aipidx = aipidx + 1 ))
  done

  myCustID=${agentCustIDArr[agentIdx]}
  myAgentID=${agentIDArr[agentIdx]}

  if [ -z $myCustID -o -z $myAgentID ]; then
    echo "could not locate agent's custID or agentID"
    exit 1
  fi

  echo "Collector's cust_id=$myCustID, agent_id=$myAgentID"
fi

if [ "${BOX_TYPE}" = "supervisor" -o "${BOX_TYPE}" = "worker" -o $isAppSvr -eq 1 ]; then
  if [ -z ${NFS_DATA_STORE} ]; then
    echo "NFS_DATA_STORE is not configured"
    exit 1
  fi 
  nncolon=`expr index ${NFS_DATA_STORE} ":"`
  if [ "$NFS_DATA_STORE" != "local" -a $nncolon -eq 0 ]; then 
    echo "${NFS_DATA_STORE} is not NFS mount"
    exit 1
  fi
  if [ -z "${SVN_HOST}" ]; then 
    echo "SVN_HOST not configured"
    exit 1
  fi
  if [ -z "${SVN_USERNAME}" ]; then
    echo "SVN_USERNAME not configured"
    exit 1
  fi
  if [ -z "${SVN_PASSWORD}" ]; then
    echo "SVN_PASSWORD not configured"
    exit 1
  fi
  runApache=1
  ## escape svn_host
  SVN_HOST_ESC=`echo "${SVN_HOST}" | sed -e 's/\\//\\\\\\//g'`
else
  SVN_HOST=
  SVN_HOST_ESC=
  SVN_USERNAME=
  SVN_PASSWORD=
  runApache=0
fi

## check environment (suppose to be per BOX_TYPE thing)
echo "Checking environment ..."
if [ ! -x /etc/init.d/rsyslog ]; then
  echo "rsyslog is not installed!"
  exit 1
fi

if [ ! -L /opt/Java ]; then
  echo "JDK not installed"
  exit 1
fi

if [ ! -x /usr/bin/expect ]; then
  echo "/usr/bin/expect not installed"
  exit 1
fi

if [ ! -x /usr/bin/nmap ]; then
  echo "/usr/bin/nmap not installed"
  exit 1
fi

snmps=`/bin/rpm -qa |grep net-snmp`
#if [ -z "$snmps" -o ! -x /usr/bin/snmpwalk -o ! -x /usr/sbin/snmptrapd ]; then
if [ -z "$snmps" ]; then
  echo "Please ensure net-snmp net-snmp-utils are installed properly"
  exit 1
fi

if [ $isAppSvr -eq 1 ]; then 
  if [ ! -d /opt/glassfish ]; then
    echo "glassfish not installed on this machine!"
    exit 1
  fi
fi

## stop cron first
echo "Stopping cron job ..."
rm -f /var/spool/cron/root
sleep 10
killall -9 phwatchdog &>/dev/null
killall -9 phxctl &>/dev/null
killall -9 wmic &>/dev/null
sleep 5

if [ $runApache -eq 1 ]; then 
  echo "Stopping apache ..."
  /sbin/service httpd stop &>/dev/null
  if [ -n "$(/usr/bin/pgrep httpd)" ]; then
    echo "Error: could not shutdown apache"
  fi
fi

## kill phoenix processes
echo "Stopping phoenix processes ..."
phProcesses="phMonitor phParser phQueryWorker phQueryMaster phRuleWorker phRuleMaster \
phReportWorker phReportMaster phIpIdentityWorker phIpIdentityMaster phDiscover \
phEventPackager phAgentManager phDataManager phCheckpoint phPerfMonitor"

for p in ${phProcesses}; do
  killall -9 ${p} &>/dev/null
done

## remove shared memory
echo "Removing parser shared memory ..."
shmid=`ipcs -a | grep 3f2  | awk '{print $2}'`
if [ -n "$shmid" ]; then 
  ipcrm -m ${shmid}
else 
  echo "Parser shared memory not found"
fi

# config system services
/sbin/chkconfig --add rsyslog
/sbin/chkconfig rsyslog on
/sbin/chkconfig --del syslog &>/dev/null
/sbin/chkconfig --del httpd &>/dev/null
/sbin/chkconfig --del postgresql &>/dev/null

## fixing ntp
echo "Setting up ntp service ..."
/sbin/service ntpd stop &>/dev/null
for i in $(seq 5); do
  /usr/sbin/ntpdate -u 0.rhel.pool.ntp.org &>/dev/null
done
/sbin/chkconfig ntpd on &>/dev/null
/sbin/service ntpd start &>/dev/null
/usr/bin/pgrep ntpd &>/dev/null
if [ $? -ne 0 ]; then 
  echo "WARNING: ntp service is not running"
fi

## check phoenix user
if [ -z "$PH_USER" ]; then 
  newPhUser=0
  PH_USER=admin
  id -u phoenix_dev &>/dev/null
  if [ $? -eq 0 ]; then
    PH_USER=phoenix_dev
  else
    id -u $PH_USER &>/dev/null
    if [ $? -ne 0 ]; then
      echo "Creating ${PH_USER}"
      useradd -d /opt/phoenix/bin $PH_USER &>/dev/null
      newPhUser=1
    fi
  fi

  if [ $newPhUser -eq 0 ]; then
      if [ -z "$NO_CHANGE_HOME" ]; then
          echo "Changing $PH_USER home to /opt/phoenix/bin"
          usermod -d /opt/phoenix/bin $PH_USER &>/dev/null
      fi
  fi
else 
  id -u $PH_USER &>/dev/null
  if [ $? -ne 0 ]; then
    echo "$PH_USER does not exist"
    exit 1
  fi
fi

## stop application server
if [ $isAppSvr -eq 1 ]; then
  echo "AS_ADMIN_PASSWORD=adminadmin" > /tmp/passwords.txt

  if [ -z "$(/usr/bin/pgrep java)" ]; then
    echo "Starting application server to undeploy phoenix application ..."
    su -l $PH_USER -c "$GF_ADMIN_TOOL start-domain --user admin --passwordfile /tmp/passwords.txt domain1"
    sleep 5
  fi
  if [ -n "$(/usr/bin/pgrep java)" ]; then
    echo "Undeploy current phoenix application ..."
    su -l $PH_USER -c "$GF_ADMIN_TOOL undeploy --user admin --passwordfile /tmp/passwords.txt phoenix"
    rm -f $GLASSFISH_HOME/domains/domain1/autodeploy/phoenix.ear*
    sleep 3
    echo "Stop application server ..."
    $GF_ADMIN_TOOL stop-domain domain1
    sleep 1
    echo "Clean application server data ..."
    rm -rf $GLASSFISH_HOME/domains/domain1/session-store/ejb/j2ee-apps/phoenix_*
    rm -rf $GLASSFISH_HOME/domains/domain1/applications/j2ee-apps/phoenix
  else
    echo "Could not start application server ..."
    exit 1
  fi
  rm -f /tmp/passwords.txt
fi

## stop the postgres server
if [ $runDBSvr -eq 1 ]; then
  echo "Stopping postgres ..."
  su -l postgres -c "pg_ctl stop -D /var/lib/pgsql/data -s -m fast"
  if [ -n "$(/usr/bin/pgrep postmaster)" ]; then
    echo "Could not stop postgres ..."
    exit 1
  fi
fi

## create dirs
if [ "${BOX_TYPE}" = "supervisor" -o "${BOX_TYPE}" = "worker" -o $isAppSvr -eq 1 ]; then
  mkdir -p /query/active
  mkdir -p /query/stopped
  mkdir -p /query/completed
  mkdir -p /query/result
  mkdir -p /query/report
  mkdir -p /querywkr/active
  mkdir -p /querywkr/stopped
  mkdir -p /querywkr/completed
  mkdir -p /querywkr/result
  mkdir -p /querywkr/uploader
  mkdir -p /querywkr/report
  chown $PH_USER.$PH_USER -R /query
  chown $PH_USER.$PH_USER -R /querywkr
  
  if [ ! -d /data ]; then 
    mkdir /data
  fi

  if [ "$NFS_DATA_STORE" != "local" ]; then
    datamnton=`mount |grep ' /data ' | awk '{print $1}'`
    if [ ! -z "${datamnton}" ]; then 
      echo "/data is currently mounted on ${datamnton}"
      if [ `expr ${datamnton} : /dev` -ne 0 ]; then 
        echo "/data is mounted on hard disk!"
        exit 1
      fi

      echo "Unmounting /data ..."
      #fuser -k /data
      #sleep 1
      umount -l /data 
      if [ $? -ne 0 ]; then 
        echo "Could not umount /data"
        exit 1
      fi
    fi

    sleep 1
    echo "Mounting ${NFS_DATA_STORE} ... "
    mount ${NFS_DATA_STORE} /data
    if [ $? -ne 0 ]; then 
      echo "Could not mount ${NFS_DATA_STORE}"
      exit 1
    fi

    ## add nfs mount to /etc/fstab
    echo "Adding nfs data store to /etc/fstab ..."
    sed '/.*[ \t]\/data[\/ \t].*/D' /etc/fstab  > /tmp/fstab.1
    echo "${NFS_DATA_STORE}	/data	nfs	defaults	0 0" >> /tmp/fstab.1
    mv /etc/fstab /etc/fstab.orig
    mv /tmp/fstab.1 /etc/fstab 
  fi

  if [ ! -d /data/cache ]; then
      mkdir -p /data/cache
      chown -R $PH_USER.$PH_USER /data/cache
  fi
fi
  
## mount nfs if necessary
pkgOnNFS=0
colonPos=`expr index ${phxPkg} ":"`
if [ ${colonPos} -gt 0 ]; then
  #phxPkg = 10.1.1.1:/pub/phx.tgz
  pkgOnNFS=1
  nfsMount=${phxPkg%/*} # strip phx.tgz from back
  pkgName=${phxPkg##*/} # strip 10.1.1.1:/pub/ from front
 
  echo "Mounting ${nfsMount} ..."
  mkdir -p /mnt/phxsetup
  umount /mnt/phxsetup
  sleep 1
  mount ${nfsMount} /mnt/phxsetup
  if [ $? -ne 0 ]; then
      echo "Could not mount ${nfsMount}"
      exit 1
  fi
  echo "Mounted"
  phxPkg="/mnt/phxsetup/${pkgName}"
fi

## check if phxPkg exists
if [ ! -f ${phxPkg} ]; then 
  echo "Package ${phxPkg} does not exist"
  exit 1
fi

## unpack the package
echo "Unpacking phoenix package ..."
rm -rf /tmp/phxsetup
mkdir -p /tmp/phxsetup
cd /tmp/phxsetup
tar xzvf ${phxPkg} &>/dev/null

if [ ! -d /tmp/phxsetup/phximg ]; then
  echo "Bad package file, could not find 'phximg'"
  exit 1
fi

## saving old packages
if [ -d /opt/phoenix ]; then
  echo "Saving /opt/phoenix to /opt/phoenix.old"
  rm -rf /opt/phoenix.old
  mv -f /opt/phoenix /opt/phoenix.old
fi

## copy /opt/phoenix
cp -r /tmp/phxsetup/phximg/opt/phoenix /opt
chown -R $PH_USER.$PH_USER /opt/phoenix

set_uid_progs="phParser phshell phFastPing"
for pg in ${set_uid_progs}; do
  if [ -f /opt/phoenix/bin/${pg} ]; then
    chown root.root /opt/phoenix/bin/${pg}
    chmod +s /opt/phoenix/bin/${pg}
  fi
done

chmod 777 -R /opt/phoenix/cache/parser/upload

if [ ! -d /etc/opsd ] ; then
	mkdir /etc/opsd ;
	chown $PH_USER.$PH_USER /etc/opsd ;
fi

if [ $isAppSvr -eq 0 ]; then 
  ## dont keep java ear
  rm -f /opt/phoenix/deployment/*.ear
fi

## copy system configuration
cp /etc/rsyslog.conf /etc/rsyslog.conf.bak
cp -rf /tmp/phxsetup/phximg/etc /
cp -rf /tmp/phxsetup/phximg/var /
cp -rf /tmp/phxsetup/phximg/usr /
cp -rf /tmp/phxsetup/phximg/root /

if [ $skipSyslogConf -eq 1 ]; then
  cp /etc/rsyslog.conf.bak /etc/rsyslog.conf
fi

## configure security related
if [ $skipApacheConfig -eq 0 ] && [ $runApache -eq 1 ]; then
  mkdir -p /etc/httpd/conf
  mkdir -p /etc/httpd/conf.d
  cp -f /opt/phoenix/deployment/jumpbox/httpd.conf /etc/httpd/conf
  #cp -f /opt/phoenix/config/http/httpd.conf /etc/httpd/conf
  cp -f /opt/phoenix/config/http/ssl.conf /etc/httpd/conf.d
  cp -f /opt/phoenix/config/http/saas* /etc/httpd/conf.d
  cp -f /opt/phoenix/config/http/ca.crt /etc/httpd/conf.d

  mkdir -p /var/www/html
  cp -f /opt/phoenix/config/http/index.html /var/www/html
  cp -f /opt/phoenix/config/http/favicon.ico /var/www/html

  /usr/sbin/setsebool httpd_can_network_connect 1

  #customize httpd.conf
  cat > /tmp/httpd_conf_cust.cmd << EOF
{
s/^[ \t]*Auth_PG_host[ \t]\+.*$/Auth_PG_host ${DBSVR_VIP}/g
s/^[ \t]*Auth_PG_database[ \t]\+.*$/Auth_PG_database ${DBNAME}/g
}
EOF

  sed -f /tmp/httpd_conf_cust.cmd /etc/httpd/conf/httpd.conf > /tmp/httpd.conf.1
  mv /tmp/httpd.conf.1 /etc/httpd/conf/httpd.conf
fi
rm -rf /opt/phoenix/config/http

if [ "$BOX_TYPE" = "collector" ]; then
  rm -rf /opt/phoenix/data-definition
  rm -rf /opt/phoenix/deployment
fi

sed -e "s/create 644 admin admin/create 644 $PH_USER $PH_USER/g" /etc/logrotate_phoenix.conf > /etc/logrotate_phoenix.conf1
mv /etc/logrotate_phoenix.conf1 /etc/logrotate_phoenix.conf

## change sysctl
if [ "$BOX_TYPE" = "supervisor" -o "$BOX_TYPE" = "worker" ]; then
  cp -f /opt/phoenix/deployment/jumpbox/subversion_client_config /etc/subversion/config

  echo "Modifying sysctl settings ..."
  el5=$(/bin/uname -r |grep el5)
  if [ -n "$el5" ]; then
    b64=$(/bin/uname -a |grep x86_64)
    if [ -n "$b64" ]; then
      echo "This is 64-bit el5"
      sysctlFile="/etc/etc_sysctl.conf.el5x64"
    else
      echo "This is 32-bit el5"
      sysctlFile="/etc/etc_sysctl.conf.el5x32"
    fi
  else
    fc8=$(/bin/uname -r |grep fc8)
    if [ -n "$fc8" ]; then
      echo "This is Fedora8 machine"
      sysctlFile="/etc/etc_sysctl.conf.fc8"
    fi
  fi

  if [ -n "$sysctlFile" ]; then
    cp -f $sysctlFile /etc/sysctl.conf
    /sbin/sysctl -p &>/dev/null
  else
    echo "Unable to recognize this machine, please setup sysctl.conf manually"
  fi
fi

if [ $runApache -eq 1 ]; then 
  chown $PH_USER.$PH_USER /var/www/cgi-bin/phEventHandler &>/dev/null
else
  rm -f /var/www/cgi-bin/phEventHandler &>/dev/null
fi
 
if [ "$BOX_TYPE" = "other" -a $skipSyslogConf -eq 0 ]; then
  syslogFwdLine="\*.info;cron.none\t\t\t@${SUPERVISOR_IP}"
  #FORWARDIP="@${SUPERVISOR_IP}"
  if [ -n "${BACKUP_SUPERVISOR_IP}" ]; then
    #FORWARDIP+=" @${BACKUP_SUPERVISOR_IP}"
    syslogFwdLine+="\n\*.info;cron.none\t\t\t@${BACKUP_SUPERVISOR_IP}"
  fi

  # configure rsyslog
  #sed -e "s/\*.info.*@127.0.0.1/\*.info;cron.none\t\t\t${FORWARDIP}/g" /etc/rsyslog.conf > /etc/rsyslog.conf.1
  sed -e "s/\*.info.*@127.0.0.1/$syslogFwdLine/g" /etc/rsyslog.conf > /etc/rsyslog.conf.1
  mv /etc/rsyslog.conf.1 /etc/rsyslog.conf

  sed -e "s/-r5000/-r514/g" /etc/sysconfig/rsyslog > /etc/sysconfig/rsyslog.1
  mv /etc/sysconfig/rsyslog.1 /etc/sysconfig/rsyslog
fi

cp /opt/phoenix/bin/phxctl /etc/init.d/phxctl
chown root.root /etc/init.d/phxctl
/sbin/chkconfig --del phxctl
/sbin/chkconfig --add phxctl
/sbin/chkconfig --del monctl &>/dev/null

## restart rsyslog
echo "Restarting rsyslog ..."
/sbin/service rsyslog restart &>/dev/null
if [ -z "$(/usr/bin/pgrep rsyslogd)" ]; then
  echo "Error: could not start rsyslog"
fi

## ldconfig
/sbin/ldconfig &>/dev/null
#/opt/phoenix/bin/phLicenseTool --update 111:ProspectHills

## customize configuration file
## some IPs should not be exposed to collector
if [ "$BOX_TYPE" = "collector" ]; then
  WORKER_IP=""
  AGENT_IP=""
  SUPERVISOR_IP=""
fi

## fill in BLADE_GENERIC
genericSvrs=""
if [ "$BOX_TYPE" = "supervisor" ]; then
  if [ $isAppSvr -eq 0 ]; then
    genericSvrs+="$APPSVR_IP"
  fi
  if [ $isDBSvr -eq 0 ]; then
    if [ $isAppSvr -eq 1 -o "$APPSVR_IP" != "$DBSVR_IP" ]; then
      genericSvrs+=",$DBSVR_IP"
    fi
  fi
fi



## handle backup supervisor ip
SUP_IP=$SUPERVISOR_IP
if [ "$myIP" = "$BACKUP_SUPERVISOR_IP" ]; then
  SUP_IP=$BACKUP_SUPERVISOR_IP
fi

cat > /tmp/phx_cfg_cust.cmd << EOF
{
s/APP_SERVER_HOST=.*$/APP_SERVER_HOST=${APPSVR_HOSTNAME}/g
s/BLADE_WORKER=.*$/BLADE_WORKER=${WORKER_IP}/g
s/BLADE_AGENT=.*$/BLADE_AGENT=${AGENT_IP}/g
s/BLADE_SUPERVISOR=.*$/BLADE_SUPERVISOR=${SUP_IP}/g
s/BLADE_GENERIC=.*$/BLADE_GENERIC=$genericSvrs/g
s/parser_server_upload_host=.*$/parser_server_upload_host=${EVENT_UPLOAD_IP}/g
s/svn_server_upload_host=.*$/svn_server_upload_host=${EVENT_UPLOAD_IP}/g
s/MON_ROLE=.*$/MON_ROLE=${MON_ROLE}/g
s/agent_id=.*$/agent_id=$myAgentID/g
s/cust_id=.*$/cust_id=$myCustID/g
}
EOF

if [ "$BOX_TYPE" = "other" ]; then
  cat >> /tmp/phx_cfg_cust.cmd <<EOF
{
s/MON_SERVER_HOST=localhost/MON_SERVER_HOST=${SUPERVISOR_IP}/g
}
EOF
fi

## create system_services
sysSvcs="<system type=\"$MON_ROLE\">"
if [ $runApache -eq 1 ]; then
  sysSvcs+="<service><name>httpd<\\/name><method>pgrep httpd<\\/method><\\/service>"
fi
if [ $isAppSvr -eq 1 ]; then
  sysSvcs+="<service><name>glassfish<\\/name><method>ps -ef |grep java |grep glassfish | grep -v pid | gawk '{print \$2}'<\\/method><\\/service>"
fi
if [ $runDBSvr -eq 1 ]; then
  sysSvcs+="<service><name>pgsql DB<\\/name><method>ps -ef | grep pgsql | grep postgres |grep -v pid |  gawk '{print \$2}'<\\/method><\\service>"
fi
sysSvcs+="<\\/system>"

#system_services=<system type="phMonitorSupervisor"><service><name>httpd</name><method>pgrep httpd</method></service><service><name>glassfish</name><method>ps -ef | grep java | grep glassfish | gawk '{print $2}'</method></service><service><name>pgsql DB</name><method>ps -ef | grep pgsql | grep postgres | gawk '{print $2 }'</method></service></system>

cat >> /tmp/phx_cfg_cust.cmd <<EOF
{
s/system_services=<system type="$MON_ROLE">.*$/system_services=$sysSvcs/g
}
EOF

cd /opt/phoenix/config
sed -f /tmp/phx_cfg_cust.cmd phoenix_config.txt > phoenix_config.txt.1
mv phoenix_config.txt.1 phoenix_config.txt
chown $PH_USER.$PH_USER phoenix_config.txt

if [ $isAppSvr -eq 1 -o "$BOX_TYPE" = "supervisor" ]; then
  cat > /tmp/phx_cfg_template_cust.cmd << EOF
{
s/APP_SERVER_HOST=.*$/APP_SERVER_HOST=${APPSVR_HOSTNAME}/g
s/BLADE_WORKER=.*$/BLADE_WORKER=/g
s/BLADE_AGENT=.*$/BLADE_AGENT=/g
s/BLADE_SUPERVISOR=.*$/BLADE_SUPERVISOR=/g
s/MON_SERVER_HOST=.*$/MON_SERVER_HOST=localhost/g
s/identity_master=.*$/identity_master=/g
s/MON_ROLE=.*$/MON_ROLE=phMonitorAgent/g
s/agent_id=.*$/agent_id=/g
s/cust_id=.*$/cust_id=/g
s/agent_password=.*$/agent_password=/g
s/svn_host\s*=.*$/svn_host=/g
s/svn_username\s*=.*$/svn_username=/g
s/svn_password\s*=.*$/svn_password=/g
s/parser_server_upload_host=.*$/parser_server_upload_host=${EVENT_UPLOAD_IP}/g
s/svn_server_upload_host=.*$/svn_server_upload_host=${EVENT_UPLOAD_IP}/g
}
EOF
  cd /opt/phoenix/config
  sed -f /tmp/phx_cfg_template_cust.cmd collector_config_template.txt > collector_config_template.txt.1
  mv -f collector_config_template.txt.1 collector_config_template.txt
  chown $PH_USER.$PH_USER collector_config_template.txt
fi

if [ -d /opt/phoenix.old/config ]; then
  cp -f /opt/phoenix.old/config/*.seq /opt/phoenix/config 2>/dev/null
  chown $PH_USER.$PH_USER /opt/phoenix/config/*.seq 2>/dev/null
fi

## restart postgres
if [ $runDBSvr -eq 1 ]; then
  echo "Starting postgres ..."
  su -l postgres -c "/usr/bin/postmaster -p 5432 -D /var/lib/pgsql/data &" >/tmp/postgres.log 2>&1
  sleep 5
  if [ -z "$(/usr/bin/pgrep postmaster)" ]; then
    echo "ERROR: postgres could not be started!"
  fi
fi

if [ $isAppSvr -eq 1 ]; then
  if [ $noautorun -eq 0 ]; then 
    echo "Start application server ..."
    echo "AS_ADMIN_PASSWORD=adminadmin" > /tmp/passwords.txt

    su -l $PH_USER -c "$GF_ADMIN_TOOL start-domain --user admin --passwordfile /tmp/passwords.txt domain1"
    sleep 60
    if [ -z "$(/usr/bin/pgrep java)" ]; then
      echo "ERROR: application server could not be started!"
    else
      if [ $populateDb -eq 1 ]; then
        echo "Creating database schema ..."
	su -l postgres -c "createlang -d \"$DBNAME\" plpgsql"
        su -l postgres -c "cd /opt/phoenix/deployment; psql -h $DBSVR_VIP -U phoenix -f phoenix_db_init.sql $DBNAME"

        ## insert phoenix servers
        insert_phoenix_server

        ## provision customer
        #provision_cust 1 10000 500 5000 111
        #provision_cust 2 10000 500 5000 112
        #provision_cust 3 10000 500 5000 113
      elif [ $upgradeDb -eq 1 ]; then 
        echo "Upgrading database schema ..."
        upgrade_schema
      fi

      $GF_ADMIN_TOOL add-resources --user admin --passwordfile /tmp/passwords.txt /opt/phoenix/deployment/glassfish-ds.xml  &>/dev/null
      su $PH_USER -c "cp -f /opt/phoenix/deployment/*.ear /opt/glassfish/domains/domain1/autodeploy"

      SYSPASS="Kd8@paL5Dcy"
      if [ $populateDb -eq 1 ]; then
        echo "Populating database ..."
        rm -rf /data/cache/* &>/dev/null
        sleep 60
        prep_user_files
        su -l $PH_USER -c "cd /opt/phoenix/deployment; ./sysinit.sh 127.0.0.1 8181 '$SYSPASS'"
        su -l $PH_USER -c "cd /opt/phoenix/deployment; ./run_prepopulator_new.sh 127.0.0.1 8181 admin '$SYSPASS' $DBSVR_VIP $DBNAME phoenix phoenix /opt/phoenix/data-definition"
#        su -l $PH_USER -c "cd /opt/phoenix/deployment; ./importET.sh $DBSVR_VIP 5432 $DBNAME phoenix phoenix /opt/phoenix/data-definition/eventType ";
        
        if [ -z "$NO_TEST_USER" ]; then
          su -l $PH_USER -c "cd /opt/phoenix/deployment; ./populate_test_user.sh 127.0.0.1 8181 admin '$SYSPASS' /opt/phoenix/data-definition"
          psql -c "UPDATE ph_sys_cust_res SET start_time = last_modified_time,  duration = 365,  end_time = last_modified_time + 31536000000, registered = true" -h $DBSVR_VIP -U phoenix $DBNAME
        fi

        if [ "$POPULATE_MAIL_ACCOUNT" = "true" ]; then
          su -l $PH_USER -c "/opt/phoenix/deployment/phoenixCLI.sh import -host 127.0.0.1 -port 8181 -cust system -user admin -pass '$SYSPASS' -type SysConf -file /tmp/mail_systemConfigs.xml"
        fi

        clean_user_files

      elif [ $upgradeDb -eq 1 ]; then
        echo "Upgrading data ..."
        sleep 60
        su -l $PH_USER -c "/opt/phoenix/deployment/data_upgrade.sh 127.0.0.1 8181 admin '$SYSPASS' $DBSVR_VIP $DBNAME phoenix phoenix /opt/phoenix/data-definition " ;
#        su -l $PH_USER -c "/opt/phoenix/deployment/importET.sh $DBSVR_VIP 5432 $DBNAME phoenix phoenix /opt/phoenix/data-definition/eventType ";
        
      else
        echo "Waiting 60 seconds for appserver to deploy phoenix application ..."
        sleep 60
      fi
    fi
    rm -f /tmp/passwords.txt

  else # if [ $noautorun -eq 0 ]; then

    su $PH_USER -c "cp -f /opt/phoenix/deployment/*.ear /opt/glassfish/domains/domain1/autodeploy"

  fi
fi

if [ $runApache -eq 1 ]; then
  echo "Starting apache ..."
  /sbin/service httpd start
  sleep 10
fi

## set up svn for worker/supervisor
if [ "$BOX_TYPE" = "supervisor" -o "$BOX_TYPE" = "worker" ]; then
  echo "Setting up svn ..."
  mkdir -p /opt/phoenix/Delta
  chown $PH_USER.$PH_USER /opt/phoenix/Delta
  ## code will handle the initial check out
  ## su $PH_USER -c "svn checkout $SVN_HOST --username $SVN_USERNAME --password $SVN_PASSWORD /opt/phoenix/Delta"
fi

if [ "$BOX_TYPE" = "supervisor" ]; then
    rm -rf /query/cache/*
fi

if [ -x "$phxPkgDir/phLicenseMigTool" ]; then
  su -l $PH_USER -c "$phxPkgDir/phLicenseMigTool --mvapps"
fi

if [ -f /opt/phoenix/deployment/jumpbox/phProvision.sh ]; then 
  cp -rf /opt/phoenix/deployment/jumpbox/phProvision.sh /etc/init.d/phProvision.sh ;
fi

## setup cron at last
if [ $noautorun -eq 0 ]; then
  cp /opt/phoenix/bin/cron_root /var/spool/cron/root
  touch /var/spool/cron
  touch /var/lock/subsys/phxctl
else # if [ $noautorun -eq 0 ]; then
  cp /opt/phoenix/bin/cron_root /var/spool/cron/root.new
fi

rm -rf /tmp/phxsetup
if [ ${pkgOnNFS} -eq 1 ]; then
  echo "Unmounting /mnt/phxsetup ..."
  umount /mnt/phxsetup
  sleep 3
fi
echo "Setup done!"
exit 0

