package Traj::ECMWF25MF;

use Traj::File_type;
use Time::Local;

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
    $self->{"owner"}="ecmwffor";
    $self->{"basetime"}=$self->calc_basetime();
    $self->{"basedate"}=$self->calc_basedate();
    return;
}

sub name {
    my ($self,$year,$month,$day,$hour)=@_;
    my $file;
    my $filebase="/badc/ecmwf-op/data/forecast/gridded/";
    my $time=timegm(0,0,$hour,$day,$month-1,$year);
    if ($time <= $self->basetime()) {
       $file=$filebase.sprintf "liua%d%.2d%.2d%.2d",
                                           $year,$month,$day,$hour;
    } else {
       my $step=($time-$self->basetime())/3600.;
       $file=$filebase.sprintf "liua%d%.2d%.2d%.2d%.3d",
                                           $self->basedate(),$step;
    }
    return $file;
}

sub calc_basetime {
    my ($basetime);
    $basetime=$^T-$^T%86400.-43200.;
    return $basetime;
}

sub calc_basedate {
    my $self=shift;
    my @date=(gmtime($self->{"basetime"}))[2..5];
    $date[2]++;
    $date[3]+=1900;
    @date=reverse @date;
    return \@date;
}

sub basetime {
    my $self=shift;
    return $self->{"basetime"};
}

sub basedate {
    my $self=shift;
    return @{$self->{"basedate"}};  #  derefernce correct?
}

1;
