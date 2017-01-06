package Traj::Logrun;
use Exporter();
@ISA=qw(Exporter);
@EXPORT_OK=qw(logrun logsize);

use BADC::AccessLog;

sub logrun {
    my ($user, $ropts)=@_;
    my $log=new BADC::AccessLog(user=>$user, 
                            logfile=>"/usr/local/stats/raw_webtraj/traj.log",
                            resource=>"webtraj");
    $log->file(logentry($ropts));
    $log->size(logsize($ropts));    # no of trajectories
    $log->record();
}

sub logentry {
    my $ropts=shift;
    my $return="[pid=$$]";
    foreach my $r (sort keys %$ropts) {
	if ($$ropts{$r} or $$ropts{$r} == 0) {
	    $return.=":$r=$$ropts{$r}";
        }
    }
    chop $return;
    return $return; 
}

sub logsize {
    my $options=shift;
    my @tmp=split(/[, ]+/,$$options{"lon"});
    my $size=@tmp;
    if ($$options{"profile"}) {
        @tmp=split(/[, ]+/,$$options{"lev"});
	$size*=@tmp;
    }
    if ($$options{"cluster"}) {
        $size*=5;
    }
    if ($$options{"release_no"}) {
        $size*=$$options{"release_no"};
    }
    return $size;
}
