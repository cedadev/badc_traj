package Traj::Pages;

use Exporter();

@ISA=qw(Exporter);
@EXPORT_OK=qw($help $style $plot $find_file $traj_top $badc_support $help_plot);

our $help='/community/trajectory/help.html';
our $help_plot='/community/trajectory/help_plot.html';
our $style='/community/trajectory/style.css';
our $traj_top='/community/trajectory/';
our $badc_support='http://badc.nerc.ac.uk/help/contact.html';
#our $plot='/cgi-bin/trajectory/plot_traj';   # comment out cos not currently used
#our $find_file='/cgi-bin/trajectory/find_file'; # comment out cos not currently used

1;
