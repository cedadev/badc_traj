package Traj::ECMWF25PE4T;

use Traj::File_type;
use Time::Local;

@ISA=qw(Traj::File_type);

sub initialise {
    my $self=shift;
    $self->{"dtuv"}=21600.;  # attribute inheritance?
    $self->{"owner"}="era4t";
    return;
}

sub name {
    my ($self,$year,$month,$day,$hour)=@_;
    my $file;
    my $filebase="/badc/ecmwf-e40/data/li/ap/";
    $file=$filebase.sprintf("%.4d/%.2d/liap%.4d%.2d%.2d%.2d.grb",
                            $year,$month,$year,$month,$day,$hour);

    return $file;
}

1;
