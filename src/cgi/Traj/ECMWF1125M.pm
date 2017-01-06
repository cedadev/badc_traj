package Traj::ECMWF1125M;

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
    my $filebase="/badc/ecmwf-op/data/gridded_1.125";
    my $mbase="lipr".sprintf "%.2d%.2d",$year,$month;
    my $file=$filebase.sprintf "/%.4d/%.2d/%.2d/model/ML.u.%.4d%.2d%.2d%.2d.grib",
                     $year,$month,$day,$year,$month,$day,$hour;
    return $file;
}

1;
