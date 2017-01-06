#!/usr/bin/perl
#
# Purpose: acts as an interface to output NASA-Ames Trajectory files
#
use CGI::Carp qw(fatalsToBrowser);
use lib (".");
use BADCLOCAL::Paths;
use lib "$BADCLOCAL::Paths::PERLLIB";
use lib "$BADCLOCAL::Paths::LOCAL_PERLLIB";

use BADC::LocalDefs;
use BADC::Setup;


use CGI;
use BADC::Weblogon qw(validate);
use Traj::FilePath qw(find_in_path);

$ENV{'PATH'}="/usr/bin:/bin";
  
CGI::ReadParse();
$group="traj";

$< = (getpwnam("root"))[2]; # Added by ASH 4/11/03

if ($user=validate($group,\%in)) {
  my $trajpath="/cache/trajectory/$user:/badc/ecmwf-trj/data:/";
  print "Content-Type: text/plain\n\n";
  $file=$ENV{'PATH_INFO'};
  if ($fullfile=find_in_path($file,$trajpath)) {
    $fullfile=~m!^(/[\w/.-]+)$!;
    $fullfile=$1;
    exec "/home/users/ecmwftrj/natty/natty", "$fullfile";
  } else {
    print "\n\n File $file not found \n";
  }
}
exit 0;
