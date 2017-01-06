#! /usr/bin/perl -w
#
# Produce an ASCII Dump of a netcdf file
#

use lib (".");
use BADCLOCAL::Paths;
use lib "$BADCLOCAL::Paths::PERLLIB";
use lib "$BADCLOCAL::Paths::LOCAL_PERLLIB";

use BADC::LocalDefs;
use BADC::Setup;


use CGI;
use BADC::Weblogon qw(validate);
use Traj::FilePath qw(find_in_path);

use strict;
use vars qw(%in);
my ($user,$path,$file);


CGI::ReadParse();
my $group="traj";
$ENV{"PATH"}="/bin:/usr/bin:/usr/local/bin";

if ($user=validate($group,\%in)) {
    $path="/cache/trajectory/$user:/badc/ecmwf-trj/data:/";
    $file=$ENV{"PATH_INFO"};
    $file=~m!^(/[\w/.-]+)$!;
    $file=$1;
    if ($file=find_in_path($file,$path)) {
        $file=~m!^(/[\w/.-]+)$!;
        $file=$1;
        print "Content-Type:application/netcdf\n\n";
	exec "cat $file";
    } else {
	print "Content-Type:text/plain\n\n";
        print "\n\nSorry file $file not found\n";
    }
}
