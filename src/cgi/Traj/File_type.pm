package Traj::File_type;

=pod

=head1 NAME

Traj::File_type

=head1 SYNOPSIS

Provides a base class (or defines interface) for datasets used by traj model.
Each new input dataset should provide a class that has the same interface.

  $dset=new Traj::File_type
  $dtuv=$dset->dtuv()                 # time between wind forcing files
  $owner=$dset->owner()               # the group owner of the files
  $file=$dset->name($year,$month,$day,$hour) # make name of file for time
  @files=$dset->spec_check(@files)    # special checks for this data set

=head1 METHODS

=head2 new()

create a new instance of the File_type class

=cut

sub new {
  my $class=shift;
  my $self={};
  bless $self, $class;
  $self->initialise();
  return $self;
}

sub initialise {
    my $self=shift;
    $self->{"owner"}="badc";
    $self->{"dtuv"}=21600.;
    return;
}

=pod

=head2 dtuv()

returns the time between wind and temperature forcing files for this dataset

=cut

sub dtuv {
    my $self=shift;
    return $self->{"dtuv"};
}

=pod

=head2 owner()

returns the group owner of the files in this dataset

=cut

sub owner {
    my $self=shift;
    return $self->{"owner"};
}

=pod

=head2 spec_check(@files)

performs any special checks of the @files in this dataset, such as weening out known problems.  Sets @errors from module Message.pm if any errors are found.

=cut

sub spec_check {
    $self=shift;
    return @_;
}

=pod

=head2 name($year,$month,$day,$hour)

returns the full path name for the wind and temperature file(s) for
year, month, day, hour.  Note method name is part of the interface,
but is not provided by base class Traj::File_type, any derived classes must
provide their own name() method.

=cut

1;
