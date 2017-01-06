package Traj::Check;

=pod

=head1 NAME

Traj::Check

=head1 SYNOPSIS

Provides checking routines for entries on the trajectory form

use Traj::Check wq(check_vars);

check_vars($ropts);  # check the form variables

=head1 FUNCTIONS

=cut

use Exporter();
@ISA=qw(Exporter);
@EXPORT_OK=qw(check_vars);

use Traj::Message qw(error warning);
use Traj::ECMWF25P;
use Traj::ECMWF25MF;
use Traj::ECMWF25PE4T;
use Traj::ECMWF1125M;
use Traj::ECMWF1125MF;
use Traj::UKMO_GS;
use Time::Local;

use English;
use strict;

#-----------------------------------------------------------------------
#
# Some values for control of checking:
our $def_length=5.;          # default run length in days
our $max_length=10.;         # maximum run lenght in days
our $def_radius=1.;          # default radius for clusters
our $def_radius_units="deg"; # defaut radius units
our $def_release_freq=24;    # default release  frequency
our $min_release_freq=1;     # minumu release frequency
our $max_release_no=10000;   # maximum number of releases
our $max_points=300;         # maximum number of points

#-----------------------------------------------------------------------
=pod

=head2 check_vars($ropts)

takes a hash reference $ropts representing the trajectory options chosen
and checks the options are within any interface limits, creates filenames
and checks the files are present.  Any warnings or errors produced by the 
checks are written to Traj::Message::warning and Traj::Message::error
respectively.

check_vars($ropts) is the top level checker, it calls the following in order

 chk_date
 chk_length
 chk_coord
 chk_cluster
 chk_multi
 chk_files

with the exception of chk_files all of these routines return corrected versions
of there input arguements.

=cut

sub check_vars {
#
# purpose: check that the variables are all within range and consistent
#
    my $ropts=shift;
    $ropts->{"date"}=chk_date($ropts->{"date"});
    $ropts->{"length"}=chk_length($ropts->{"length"});
    my($lats,$levs)=chk_coord($ropts->{"lon"},$ropts->{"lat"},$ropts->{"lev"},
                              $ropts->{"profile"});
    ($ropts->{"cluster"},$ropts->{"radius"},$ropts->{"radius_units"})=
        chk_cluster($ropts->{"cluster"},$ropts->{"radius"},
                    $ropts->{"radius_units"});
    $ropts->{"no_points"}=chk_no_points($ropts->{"profile"},
                                        $ropts->{"cluster"},
                                        $lats,$levs);
    ($ropts->{"release_no"},$ropts->{"release_freq"})=
              chk_multi($ropts->{"release_no"},$ropts->{"release_freq"},
                        $ropts->{"time_step"});
    my @files=chk_files($ropts->{"date"},$ropts->{"time"},$ropts->{"length"},
                        $ropts->{"direction"},$ropts->{"release_no"},
                        $ropts->{"release_freq"},$ropts->{"source"}) 
                if $ropts->{"date"};
}

#-----------------------------------------------------------------------

=pod

=head2 chk_cluster($cluster,$raduis, $raduis_units);

if the $cluster switch is set checks that $raduis and $raduis_units are set
If they are undefined then default values of 1 and degrees are used.

The default radius is set by the package variable $def_raduis
The default radius units are set by package variable $def_radius_units

=cut

sub chk_cluster{
#
# sub chk_cluster
# Purpose: checks that the cluster radius is set if cluster asked for
#
    my ($cluster, $radius, $radius_units)=@_;
    if ($cluster) {
	unless ($radius) {
	    warning("cluster radius not set, default of ".
		"1 degree used.");
	    $radius=$def_radius;
	    $radius_units=$def_radius_units;
	}
    }
    return ($cluster, $radius, $radius_units);
}

#-----------------------------------------------------------------------

sub exist_var {
    my ($var,$name)=@_;
    unless ($var) {
      error ($name." not set");
    }
    return $var
}

#-----------------------------------------------------------------------

=pod

=head2 chk_date($date)

If the $date is defined checks it is of the form yyyymmdd or yymmdd.
Expands yymmddd date formats to yyyymmdd.  Checks that the date is a valid
date (mm < 12, not too many days in the month).

=cut

sub chk_date{
#
# Purpose: Check the input date
#
    my ($date)=shift;
    if (exist_var($date,'date')) {
	if ($date =~ m/^(\d{8})$/ or $date =~ m /^(\d{6})$/) {
	    $date=long_date($1);
            $date=date_in_range($date);
	} else {
            error("date must be format [yy]yymmdd.");
            $date="";
	}
    }
    return $date;
}

#-----------------------------------------------------------------------

sub long_date {
    my $date = shift;
    if ($date < 1.e7) {
        $date+=($date > 7.e5 ? 19000000 : 20000000);
    }
    return $date
}

#-----------------------------------------------------------------------
# Changed to make leap years work
#
sub date_in_range {
    my $date=shift;
    my ($yy,$mm,$dd)= $date =~ /(\d{4})(\d{2})(\d{2})/; 
    my @months= (31,28,31,30,31,30,31,31,30,31,30,31);
    $months[1] += (not $yy%400 or (not $yy%4 and $yy%100)) ? 1 : 0;
    if ($mm > 12 || $mm < 1) {
      error("month out of range.  ".
	  "Expect date format yymmdd or yyyymmdd.");
	$date="";
    }
    if ( $dd > $months[$mm-1] ) {
      error("too many days in the month.");
	$date="";
    }
    return $date;
}

#-----------------------------------------------------------------------

=pod

=head2 chk_length($length)

if the length in days ($length) is not set then set to default length of
5 days.  Otherwise checks $length is a digit and checks it is not longer
than the longest allowed trajectory lenght (10 days).

The default length is set by package variable $def_length
The default maximum length is set by package variable $max_length

=cut

sub chk_length{
#
# Check length of time
#
    my $length=shift;
    unless ( $length ) {
      warning("length not set, default of 5 days used.");
      $length=$def_length;
    } else {
	if ( $length =~ m/^(\d+)$/) {
	    $length=$1;
	} else {
	  error("expect digits only for length of run");
          $length=0;
	}
    }
    if ( $length > $max_length ) {
      warning("length of run greater than 10 days.".
	    "  10 days will be used.");
      $length=$max_length;
    }
    return $length;
}

#-----------------------------------------------------------------------

=pod

=head2 chk_multi($release_no,$release_freq,$time_step);

If $release_no (the number of multiple releases) is set checks the
frequency of release ($release_freq).
If $release_freq is not set it is set as a default of 24 hours.
If $release_freq is less than one hour then it is reset at an hour.
If $release_no is greater than 100 then it is reset to be 100.
Finally $release_freq is not a whole number of timesteps it is reset
to be a whole number of time steps.

The default values and ranges are set by package variables
$def_release_freq, $min_release_freq, $max_release_freq.

=cut

sub chk_multi {
#
# Purpose: check correct options for multiple release
#
    my $release_no=shift;
    my $release_freq=shift;
    my $time_step=shift;
    if ($release_no) {
	if ($release_no > 1 and $release_freq==0) {
	    warning("release frequency not used.  24 hours set as default.");
	    $release_freq=$def_release_freq;
	}
	if ($release_no > $max_release_no) {
	    warning("more than 10000 releases.  10000 releases used.");
	    $release_no=$max_release_no;
	}
        if ($release_freq < $min_release_freq) {
	    $release_freq=1;
	    warning("release frequency less than one hour. ".
                    " Release frequency set as one hour.");
        }
	my $srel_freq=$release_freq*3600.;    # convert to seconds
	if ($srel_freq%$time_step > 1.e-6) {
	    $release_freq=$time_step*int($srel_freq/$time_step)/3600.;
	    warning("release frequency not a whole number of timesteps. ".
		    " Release Frequency set as $release_freq hours.");
	}
    }
    return ($release_no,$release_freq);
}

#-----------------------------------------------------------------------

=pod

=head2 chk_coord($lon,$lat,$lev,$profile);

Checks the input defining the initial conditions and expands to produce
the full initial condition coordinate arrays.  (arrays of longitude, latitude,
level).  If the $profile flag is set then a profile defined by the levels $lev
is produced at each longitude-latitude point.
The values in $lon, $lat, $lev are comma or space seperated lists of floats.

=cut

sub chk_coord{
#
# Check and make the coordinate arrays
#
    my ($lon,$lat,$lev,$profile) = @_;
    $lon=chk_digit_list($lon,"longitude");
    $lat=chk_digit_list($lat,"latitude");
    $lev=chk_digit_list($lev,"level");
    my ($lons,$lats,$levs,@dum);
    if ($lon ne "" and $lat ne "" and $lev ne "") {
        $lons=(@dum=split /[,\s]+/, $lon);
        $lats=(@dum=split /[,\s]+/, $lat);
        $levs=(@dum=split /[,\s]+/, $lev);
        if ($lons != $lats){
	    error("there are not the same number".
		" of longitudes and latitudes");
            $lats="";
	}
	unless ( $profile || $lons==$levs ) {
	    error("there are not the same number of".
		" levels and longitudes.  Did you want a profile?");
            $levs="";
	}
    }
    return wantarray ? ($lats,$levs) : ($lats && $levs);
}

#-----------------------------------------------------------------------

sub chk_digit_list {
    my ($list,$name)=@_;
    if ($list ne "") {
	if ($list =~ m/^([-\d.,\s]+)$/) {
            $list=$1
        } else {
	    error("$name must be a comma or space ".
		"seperated numeric list");
            $list="";
	}
    } else {
	error("$name not set.");
    }
    return $list
}

#-----------------------------------------------------------------------

=pod

=head2 chk_no_points($profile, $cluster, $lons, $levs);

checks that the number of trajectories to be integrated is less than the
maximum number of points.  The maximum number of points is set by the package
variable $max_ponts.

=cut

sub chk_no_points{
#
# Check the number of points being integrated
    my ($profile, $cluster,$lons,$levs)=@_;
    my ($nlev,$nclus) = (1,1);
    my $mclus=5;
    $nlev=$levs if $profile;
    $nclus=$mclus if $cluster;
    my $no_points=$lons*$nlev*$nclus;
    if ( $no_points > $max_points ) {
	error("there are too many points in this run.".
	    " Please resubmit as more than one run.");
	$no_points="";
    }
    return $no_points;
}

#-----------------------------------------------------------------------

sub source_class {
    my $source=shift;
    my $class;
  CASE: {
      $source eq "ecmwf_2.5p" and do {$class="Traj::ECMWF25P"; last CASE};
      $source eq "ecmwf_2.5mf" and do {$class="Traj::ECMWF25MF"; last CASE};
      $source eq "ecmwf_2.5p_e4t" and do {$class="Traj::ECMWF25PE4T"; last CASE};
      $source eq "ecmwf_1.125m" and do {$class="Traj::ECMWF1125M"; last CASE};
      $source eq "ecmwf_1.125mf" and do {$class="Traj::ECMWF1125MF"; last CASE};
      $source eq "ukmo_gs" and do {$class="Traj::UKMO_GS"; last CASE};
      $class="Traj::File_type";
  }
    return $class;
}

#-----------------------------------------------------------------------

=pod

=head2 chk_files($date,$time, $days, $dir, $release_no, $release_freq,$source);

Creates a list of the filenames that will be used from dataset corresponding 
to $source, for the set of options specified by this run.  File names are
built using the $date, and $time (hh), for a duration specified by
$time, $release_no and $release_freq depending on the trajectory direction
specified by $dir.

=cut

sub chk_files{
#
# Purpose: Check for the exsistance of the files

    my ($date,$time,$days,$dir,$release_no,$release_freq,$source)=@_;
    my $class=source_class($source);
    my $file_t=new $class;
    my ($yy,$mm,$dd)= $date =~ /(\d{4})(\d{2})(\d{2})/;
    $date=timegm(0,0,$time,$dd,$mm-1,$yy);
    my @files=&make_files($date,$days,$dir,$release_no,$release_freq,$file_t);
    my $owner;
    my $gid=$GID;
    foreach my $file (@files) {
        $owner=$file_t->owner($file);
	$GID=getgrnam($owner);
	unless (-e $file) {
	    error("File $file used in deriving trajectories does not exist.");
	    $file="";
	 }
    }
    $GID=$gid;
    @files=$file_t->spec_check(@files);
    return @files;
}

sub make_files {
    my ($date,$days,$direction,$release_no,$release_freq,$file)=@_;
    my (@date,$time);

    my $tuv=$file->dtuv();
    my $srel_freq=$release_freq ? $release_freq*3600. : 0;  # in seconds
    my $rel_no=$release_no ? $release_no : 1;
    my $sdays=$days*86400.;                    # seconds
    my $tot=$sdays+($rel_no-1)*$srel_freq; # seconds
    $tuv*=($direction =~/forward/i ? 1 : -1);
    my @files;
    for ($time=0 ; abs $time <= $tot; $time+=$tuv) {
	if (traj_present(abs $time,$sdays,$srel_freq)) {
           @date=date_str($date+$time);
	   push @files, $file->name(@date);
       }
    }
    return @files;
}

sub traj_present {
   my ($time, $len, $freq)=@_;
   return ($freq == 0 or ($time%$freq) <= $len);
}

#
# Convert the date in unix seconds into an array (year, month, day, hour)
sub date_str{
    my ($date)=@_;
    my @date=(gmtime $date)[2...5];
    $date[2]++;
    $date[3]+=1900;
    @date=reverse @date;
    return @date;
}
