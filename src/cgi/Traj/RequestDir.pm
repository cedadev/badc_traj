package Traj::RequestDir;

use Exporter();
use English;

@ISA=qw(Exporter);
@EXPORT_OK=qw(make_requests);

our $owner='badc';

my $test=0;
if ($test) {
    delete_requests('jkettleb');
    make_requests('jkettleb');
}

sub set_owner {
    my $owner=shift;
    my $ouid=getpwnam($owner);
    if ($EFFECTIVE_USER_ID == $ouid) {
	return $ouid;
    } elsif ($REAL_USER_ID == getpwnam('root')) {
        my $euid=$EFFECTIVE_USER_ID;
        $EFFECTIVE_USER_ID=$ouid;
        return $euid;
    }
    return ''
}

sub reset_user {
    my $euid=shift;

    if ($EFFECTIVE_USER_ID != $euid) {
	if ($REAL_USER_ID == getpwnam('root')) {
            my $EFFECTIVE_USER_ID=$euid;
	}
        return $euid;
    }
    return '';
}

sub requests_name {
    my $user=shift;
    return "/requests/$user";
}

sub make_requests {
    my $user=shift;

    $user=~m/(\w*)/;
    $user=$1;
    my $requests=requests_name($user);

    my $euid=set_owner($owner);
    if ($euid eq '') {
       die("can not create $requests as $owner");
    }
    if (! -d $requests) {
	mkdir($requests,0770);
    }
    $ftpaccess=$requests."/.ftpaccess";
    if (! -f $ftpaccess) {
      make_ftpaccess($ftpaccess,$user)
    }
    my $mode=0555;
    chmod ($mode,$ftpaccess);
    reset_user($euid);
    return $requests;
}

sub delete_requests {  # only really called for test purposes.
    my $user=shift;
    my $requests=requests_name($user);
    if ( -d $requests) {
	qx(rm -r $requests);
    }
}

sub make_ftpaccess {
    my $ftpaccess=shift;
    my $user=shift;
    open(OUT,">$ftpaccess") or die("Can not open ftpaccess\n");
    print OUT <<"EOF";
<Limit READ>
         AllowUser $user
         AllowUser badc
	 DenyAll
</Limit>
<Limit WRITE>
         AllowUser $user
	 DenyAll
	 </Limit>
<Limit DIRS>
         AllowUser $user
         AllowUser badc
	 DenyAll
</Limit>
EOF
       close(OUT);
}
1;
