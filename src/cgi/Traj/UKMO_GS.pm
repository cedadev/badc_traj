package Traj::UKMO_GS;

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
    $self->{"dtuv"}=43200.;  # attribute inheritance?
    $self->{"owner"}="um";
    return;
}

sub name {
    my ($self,$year,$month,$day,$hour)=@_;
    my $filebase="/badc/ukmo-um/data/global/sg";
    my $file=$filebase.sprintf "/%.4d/%.2d/%.2d/%.2d/sg%.4d%.2d%.2d%.2dp56s00.pp",
                     $year,$month,$day,$hour,$year,$month,$day,$hour;
    return $file;
}

1;
