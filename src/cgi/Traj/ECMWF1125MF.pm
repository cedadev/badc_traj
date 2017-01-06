package Traj::ECMWF1125MF;

use Traj::Message qw(error);

use Traj::ECMWF25MF;
use Time::Local;

@ISA=qw(Traj::ECMWF25MF); 

sub name {
    my ($self,$year,$month,$day,$hour)=@_;
    my $filebase="/badc/ecmwf-for/data/";
    my $time=timegm(0,0,$hour,$day,$month-1,$year);
    if ($time <= $self->basetime()) {
       $file=$filebase.sprintf "%.4d/%.2d/%.2d/model/ML.u.%.4d%.2d%.2d%.2d.grib",
                                           $year,$month,$day,$year,$month,$day,$hour;
    } else {
       my $step=($time-$self->basetime())/3600.;
       my ($byear, $bmonth, $bday, $bhour)=$self->basedate();
       $file=$filebase.sprintf "%.4d/%.2d/%.2d/model/FC.u.%.4d%.2d%.2d%.2d.%.3d.grib",
                                           $byear,$bmonth,$bday,$byear,$bmonth,$bday,$bhour,$step;
    }
    return $file;
}

# the following  routine is not inherited from ECMWF25MF because the base times
# for the 2.5 and 1.125 model forecasts are different.  TEMPORARY.
sub calc_basetime {
    my ($basetime);
    $basetime=$^T-$^T%86400.-43200.-43200.;  # an added -43200 remove later
    return $basetime;
}


1;
