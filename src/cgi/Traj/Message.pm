package Traj::Message;
use Exporter;
@ISA=qw(Exporter);
@EXPORT_OK=qw(error warning @Error @Warning);

use vars qw(@Error @Warning);

sub error {
    push @Error, @_;
    return;
}

sub warning {
    push @Warning, @_;
    return;
}
