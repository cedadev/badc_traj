package Traj::FilePath;
use Exporter;
@ISA=qw(Exporter);
@EXPORT_OK=qw(find_in_path);

sub find_in_path {
    my $file=shift;
    my @dirs=split ':', shift;
    my $return;
    while (my $dir=shift @dirs) {
        if (-d $dir) {
            if (-e $dir."/".$file) { # only checks existence, nothing else
                $return="$dir/$file";
                last;
            }
        }
    }
    return $return;
}
