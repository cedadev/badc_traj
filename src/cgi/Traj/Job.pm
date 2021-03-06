package Traj::Job;
#
# History:
#
#  29-Aug-2002 ASH Changed web address 'www.badctraj.rl.ac.uk' to 'cirrus.badc.rl.ac.uk'. This is 
#                  because the login cookies require a '.badc.rl.ac.uk' domain to be compatible with
#                  the cookies issued on tornado. 
use Exporter();
use Traj::Pages;
use Traj::RequestDir qw(make_requests);

@ISA=qw(Exporter);
@EXPORT_OK=qw(submit_job);

use vars qw($ADVECT $INITRAJ $IDL $TRAJ $NC2NASA);

$SRC="/home/users/ecmwftrj/src";
$ADVECT="$SRC/advect.pro";
$INITRAJ="/usr/local/badctraj/ini_traj";  # traject initialisation
$INITRAJ="$SRC/ini_traj";
#$IDL="/usr/local/rsi/idl_5.2/bin/idl";    # idl executable
$IDL="/usr/local/bin/idl";    # idl executable
$TRAJ="/cache/trajectory";               # base working directory
$NC2NASA='/home/users/ecmwftrj/natty/natty';       # conversion from netcdf to NASA-Ames

# $MACHINE is the machine to submit job on 
# 
# It does "ssh badc@$MACHINE qsub ..."
# 
# NB use an empty string to queue the job locally without ssh.  This is
# different from using e.g. "localhost" (which still uses ssh) because of 
# the potential change of username

$MACHINE = "southerly";   
$QUEUE = "all.q";               # q to use
$QUEUE_LONG = "all.q";          # q to use
$QTYPE = "sge";   # allowed values : "nqs", "sge"


# $MACHINE = "mercury";   
# $QUEUE = "short";               # q to use
# $QUEUE_LONG = "medium";          # q to use
# $QTYPE = "nqs";   # allowed values : "nqs", "sge"


sub time_string {
#
# Purpose: create a time string to use to label runs
#
    my ($sec,$min,$hour,$mday,$mon)=(gmtime())[0..4];
    $mon++;
    return sprintf "%.2d%.2d%.2d%.2d%.2d", $mon, $mday, $hour, $min, $sec;
}

sub seperate_date {
#
# Purpose: seperates the date string
#
    my $date=shift;
    $date=~m/(\d\d\d\d)(\d\d)(\d\d)/;
    return ($1,$2,$3);
    
}

sub queresource {
    my $rtraj=shift;
    use Traj::Logrun qw(logsize);
    my $no_trajs=logsize($rtraj);
    my ($nice,$queue)=(0, $QUEUE); # defaults
    my $limits=100;
    if ($no_trajs > $limits) {
      ($nice,$queue)=(10, $QUEUE_LONG);
    }
    return ($nice,$queue);
}

sub submit_job {
#
# Purpose: write the job file for the trajectory
#          submit the job
#
    my ($user,$email,$rtraj)=@_;
    %options=%$rtraj;
#
#  1. initialisation of paths etc.
    $ENV{PATH}="/bin:/usr/bin:/usr/local/bin";  # for taint check
    my $time=time_string();                     # time label
    my $work="/requests/$user/traj_service";    # working directory
#    my $log="$TRAJ/.logs/$user$time.log";       # log file destination
    my $logdir="/home/badc/tmp/trajectory"; # tmp dir - visible on all hosts
    my $log="$logdir/$user$time.log";
    my $pre="tt";                               # prefix for output files
    my $exp="exp";                               # prefix for expt directory
    my $tjfile="$options{date}$options{time}.nc"; # trajectory filename
#
#  2. convert options from form into options for model
    my $tspd=int(86400./$options{"time_step"});
    my ($nice,$queue)=queresource($rtraj);
    $options{"profile"} and $options="-p ";
    $options{"cluster"} and $options.="-r $options{radius}".
                                          "$options{radius_units} ";
    $options{"vert_adv"}==1 and $options.="-theta ";
    $options{"lon"}=~s/,/ /g;
    $options{"lat"}=~s/,/ /g;
    $options{"lev"}=~s/,/ /g;
    $options.="-lon $options{lon} -lat $options{lat} -lev $options{lev}";

    my ($yy,$mm,$dd)=seperate_date($options{"date"});
    my $days=$options{"length"}*($options{"direction"} =~ /forward/i ? 1 : -1);
    my $hour=$options{"time"};
    my $extras;
    $options{"source"} and $extras=qq(, source='$options{"source"}');
    $options{"release_no"} and 
             $extras.=qq(, release_no=$options{"release_no"});
    if ($options{"release_freq"}) {
	my $rel=int($options{"release_freq"}*3600./$options{"time_step"});
        $extras.=qq(, release_freq=$rel);
    }
    $extras.=qq(, kcvert=$options{"vert_adv"});
    if ($options{"out_freq"}) {
        my $kout=int($options{"out_freq"}*3600./$options{"time_step"});
        $extras.=qq(, kout=$kout);
    }
    $work=make_requests($user);
    $work.="/traj_service";
#
#  3. submit the job
    my $id=$<;
#    $< = (getpwnam("ecmwftrj"))[2];  # do this so qsub runs as ecmwftrj
    $< = (getpwnam("badc"))[2];  # do this so qsub runs as badc

    # make log dir /before/ running qsub (maybe got away without this under
    # NQS, but SGE wants to create the path of output file at the start, so
    # not enough just to create it inside the job)
    system("mkdir -p $logdir");

    my ($command, $header);
    if ($QTYPE eq 'sge') { 
       $command = ". /opt/sge/default/common/settings.sh ; qsub";
       $header = <<"EOF";
#\$ -j y
#\$ -o $log
#\$ -q $queue
#\$ -N $user
#\$ -S /bin/sh

renice $nice \$\$
EOF
    }
    elsif ($QTYPE eq 'nqs') {
       $command = "qsub";
       $header = <<"EOF";
#QSUB -eo 
#QSUB -r $user
#QSUB -q $queue
#QSUB -o $log 
#QSUB -ln $nice
#QSUB

EOF
    }

    if ($MACHINE ne '') {
       $command = "ssh badc\@$MACHINE '$command'";
    } else {
       $command = "sh -c '$command'";
    }
   
    $command .= ' > /dev/null 2>&1';

    open (QSUB, "| $command");
##    open (QSUB, "> /tmp/trajectory/job"); print QSUB "To submit with: $command\n";  ##DEBUG ONLY

    print QSUB $header;

    print QSUB <<"EOF";

set -vx

mail_err()    # mail on error
{
mail $email << EOMAIL
Sorry a trajectory job for $options{date} has failed.
The BADC staff have been informed of the error and will try and solve
the problem.
EOMAIL

mail badc-support\@rl.ac.uk << EOMAIL
A trajectory integration failed for user $user.

mail $email.

The log file can be found in $log
EOMAIL

#mail badc-support\@rl.ac.uk <  $log

rm \$runfile
cd ..
rmdir \$runid
at now + 1 minute << ERM
#rm $log       # only really need log in failed case
ERM

exit 1
}

mail_suc()    # mail on success
{

wwwpage="http://badc.nerc.ac.uk/cgi-bin/trajectory/plot_traj/requests/$user/traj_service/\$runid/"

cat > mail.txt << EOMAIL
The trajectory job starting on $options{date} has run.  It has been given an
identifier \$runid.
To view the results point your WWW browser at:

EOMAIL

ls -1 ${pre}*nc | sed "s,^,\$wwwpage," >> mail.txt

cat >> mail.txt << EOMAIL

Other trajectories you have created using the BADC trajectory service can be found through your requests area on the badc

http://badc.nerc.ac.uk/cgi-bin/data_browser/data_browser/requests/$user/traj_service

If you experience any problems please contact the BADC user support (badc\@rl.ac.uk)

EOMAIL

mail $email < mail.txt
rm mail.txt

}

setperms() {
   # set output group-writable by byacl
   chmod 770 $work
   chgrp byacl $work
   chmod -R 770 $work/\$runid
   chgrp -R byacl $work/\$runid
}


# add IDL path 

set IDL_PATH=$SRC/idl_5.2_compat
export IDL_PATH

# 1. Make working directory
mkdir -p $logdir
mkdir -p $work
cd $work
ls | grep $exp
if [ \$? = 0 ]
then
    num=`ls -1d $exp* | tail -1 | sed "s/$exp\\([0-9]*\\).*/\\1/"`
    num=`expr \$num + 1`
    if [ \$num -lt 10 ]
    then
       num="00"\$num
    elif [ \$num -lt 100 ]
    then
       num="0"\$num
    elif [ \$num -gt 999 ]
    then
       num="001"
    fi
else
    num="001"
fi
runid=$exp\$num

mkdir \$runid
cd \$runid

runfile=`pwd`/".${pre}.nc"

# 1.1 Initialisation
$INITRAJ $options \$runfile || mail_err   # make initial conditions

# 2. run the trajectories
$IDL << EOIDL

!PATH = !PATH + ':$SRC/idl_5.2_compat'
.comp $ADVECT

print,!PATH
advect, $yy, $mm, $dd, $hour, $days, input_file='\$runfile', tspd=$tspd, catch_error=1 $extras
EOIDL



if [ \$? != 0 ]
  then 
       setperms
       mail_err
  else 
       rm \$runfile
       for res in .${pre}*.nc
       do
         mv \$res `echo \$res | sed "s/^\\.//"`
       done
       for i in *.nc
       do
         $NC2NASA \$i > `echo \$i | sed "s/nc/na/"`
       done
       setperms
       mail_suc
       
fi

at now + 1 minute << ERM
rm $log       # only really need log in failed case
ERM

exit 0
EOF
   #error trapping??
    close QSUB;
    $<=$id;

}

