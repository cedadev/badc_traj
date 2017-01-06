#!/usr/bin/perl -w

=pod

=head1 SYNOPSIS

traj.pl [--help] 
        --datetime yyyymmddhh 
        --lon lon1,lon2,lon3...
        --lat lat1,lat2,lat3...
        --lev lev1,lev2,lev3...
        [--time_step seconds]
        [--length days]
        [--vert_adv]
        [--release_freq units]
        [--release_no number]
        [--profile] [--cluster]

=cut

use Getopt::Long;
use Pod::Usage;

my $SRC="/home/tornado/ecmwftrj/src";
my $ADVECT="$SRC/advect.pro";
my $INITRAJ="$SRC/ini_traj";
my $IDL="/usr/local/bin/idl";    # idl executable

# 0. Command line options
my %options;
$options{'vert_adv'}=0;
$options{'lon'}='0';
$options{'lat'}='0';
$options{'lev'}='100';
$options{'direction'}='forward';
$options{'time_step'}=3600/2; # check default time steps
$options{'length'}=10;

GetOptions(\%options, 'profile', 'cluster', 'radius=f', 'radius_units=s',
           'vert_adv', 'lon=s', 'lat=s', 'lev=s',
           'time_step=f', 'datetime=i', 'length=i',
           'direction','source=s','out_freq=i','release_freq=i',
           'release_no=i','testonly','help');

if (exists $options{"help"}) {pod2usage(2)};
if (!exists $options{'datetime'}) {pod2usage(2)};

# 1. Trajectory initialisation options
my $options;
$options{"profile"} and $options="-p ";
$options{"cluster"} and $options.="-r $options{radius}"."$options{radius_units} ";
$options{"vert_adv"}==1 and $options.="-theta ";
$options{"lon"}=~s/,/ /g;
$options{"lat"}=~s/,/ /g;
$options{"lev"}=~s/,/ /g;
$options.="-lon $options{lon} -lat $options{lat} -lev $options{lev}";

# 2. Trajectory options
my $tspd=int(86400./$options{"time_step"});
my ($yy,$mm,$dd,$hour)=seperate_date($options{"datetime"});
my $days=$options{"length"}*($options{"direction"} =~ /forward/i ? 1 : -1);
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


my $runfile="ttini.nc";
if ($options{'testonly'}) {
print "$INITRAJ $options $runfile\n";

# 2. run the trajectories
print <<"EOIDL";

!PATH = !PATH + ':$SRC/idl_5.2_compat'
.comp $ADVECT

print,!PATH
advect, $yy, $mm, $dd, $hour, $days, input_file='$runfile', tspd=$tspd, catch_error=1 $extras
EOIDL
    exit (0);
}

`$INITRAJ $options $runfile`;
open(IDL,"|$IDL");
print IDL<<"EOIDL";

!PATH = !PATH + ':$SRC/idl_5.2_compat'
.comp $ADVECT

advect, $yy, $mm, $dd, $hour, $days, input_file='$runfile', tspd=$tspd, catch_error=1 $extras
EOIDL

close(IDL);

unlink $runfile;

sub seperate_date {
#
# Purpose: seperates the date string
#
    my $date=shift;
    $date=~m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
    return ($1,$2,$3,$4);
    
}
