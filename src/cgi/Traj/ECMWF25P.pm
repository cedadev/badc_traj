package Traj::ECMWF25P;

use Traj::Message qw(error);

use Traj::File_type;
@ISA=qw(Traj::File_type);

sub new {
  my $class=shift;
  my $self={};
  bless $self, $class;
  $self->initialise();
  return $self;
}
sub initialise {
    my $self=shift;
    $self->{"dtuv"}=21600.;  # attribute inheritance?
    $self->{"owner"}="ecmwfop";
    return;
}

sub name {
    my ($self,$year,$month,$day,$hour)=@_;
    my $filebase="/badc/ecmwf-".op_or_era($year,$month)."/data/gridded_2.5/";
    my $ybase=$year;
    $year%=100;
    my $mbase="lipr".sprintf "%.2d%.2d",$year,$month;
    my $file=$filebase.$ybase."/".$mbase.sprintf "/lipr%.2d%.2d%.2d%.2d",
                                           $year,$month,$day,$hour;
    return $file;
}

sub op_or_era {
    my ($year,$month)=@_;
    my $return="op";
    if ($year < 1994 or ($year < 1995 and $month < 03)) {
        $return="era"
    }
    return $return;
}

sub owner {
    my ($self,$file)=@_;
    my ($year,$month)=($file=~/lipr(\d\d)(\d\d)/);
    $self->{"owner"}="ecmwf".op_or_era(1900+$year,$month);
    return $self->{"owner"};
}

sub spec_check {
    my $self=shift;
    my @files=@_;
    my $imatch=0;
    my $daystr="";
    foreach my $file (@_) {
	if ($file=~/99030900/ || $file=~/99030818/) {
	    $imatch++;
	    $daystr="9th March 1999";
	}
        if ($file=~/94030100/ || $file=~/94022818/) {
	    $imatch++;
            $daystr="1st March 1994";
        }
    }
    if ($imatch == 2) {
      error("Sorry vertical levels in wind files change on ${daystr}.\n".
            " Model cannot, at present, cope with this change.\n".
            " Please try resubmitting with trajectory runs that do not".
            " span ${daystr}.");
      @files="";
    }
    return @files;
}
1;
