#!/usr/bin/perl
#
# Purpose:  Control the submission of trajectories
# Method:   There are three basic forms
#           The first is the basic submission form
#           which determines the start time, initial conditions etc.
#           The second is confirm form which checks that the user
#           has entered the values they think they have.
#           The third is the trajectory submitted form
#           A fourth html page appears at times if there are errors
# Comments: Deciding which form to print could be tidier
#           Error processing could be tidier
#           No taint checking is done at confirm, only at submit
# Author:   JAK Feb 99 BADC
#
# History:
#
#   24-Oct-2002 ASH Modified to read email address from passwd file copied from tornado
#

use lib (".");
use BADCLOCAL::Paths;
use lib "$BADCLOCAL::Paths::PERLLIB";
use lib "$BADCLOCAL::Paths::LOCAL_PERLLIB";

use BADC::LocalDefs;
use BADC::Setup;

use NDG::Security::Client;
use NDG::Security::Weblogon;

use Traj::Check qw(check_vars);
use Traj::Job qw(submit_job);
use Traj::Message qw(error warning @Error @Warning);
use Traj::Logrun qw(logrun);
use Traj::Pages qw($help $style $traj_top $badc_support);
use CGI;
use English;

use strict;
use vars qw ($User %in);


my $PASSWD_FILE = "/home/badc/software/infrastructure/accounts/files/BADCpasswd";    #Password file
$ENV{PATH}="/usr/bin:/usr/sbin:/usr/local/bin";          #Required by taint checking

my %source_lables = ("ecmwf_2.5p" => "ECMWF archive (2.5 degree/pressure levels)",
#                  "ecmwf_2.5mf" => "ECMWF forecast/current analysis",
		  "ecmwf_1.125mf" => "ECMWF 1.125 degree forecast/current analysis",
		     "ecmwf_2.5p_e4t"=> "ERA 40 archive (2.5 degree/pressure levels) - post 1st Jan 1970 only",
                  "ukmo_gs" => "Unified model (Global/Standard levels)",
		  "ecmwf_1.125m" => "ECMWF 1.125 op archive");
     
##
## Moved this so that validate would work. ASH 28/10/02
##
##$< = (getpwnam("ecmwftrj"))[2];    # do this first to get out of su root
##
$< = (getpwnam("root"))[2]; 

CGI::ReadParse();

my $group='traj';

NDG::Security::Weblogon::weblogon(mustLogin=>"y");
$User = NDG::Security::Weblogon::username();

my @roles = NDG::Security::Weblogon::roles();

if (grep /^${group}$/, @roles) {
    $< = (getpwnam("ecmwftrj"))[2];    # do this first to get out of su root
                                       # note effective uid still root!
    my @sources = ();

    push @sources, "ecmwf_2.5p";
    push @sources, "ecmwf_1.125m";

    push @sources, "ecmwf_1.125mf" if (grep /^ecmwffor$/, @roles);
    push @sources, "ecmwf_2.5p_e4t" if (grep /^era$/, @roles);
    push @sources, "ukmo_gs" if (grep /^um$/, @roles);
  
    process_traj_form(@sources);
}

 print_no_resource ($User);
 
#
#-----------------------------------------------------------------------
#
sub process_traj_form {
    use vars qw($Source $TimeStep $Form $Forecast);
    my @sources = @_;
    $Source="ecmwf_2.5p";    # default source
    $TimeStep=1800.;         # append to traj run OK?

    my $traj;
    $Form=$in{CGI};

    my $option=&what_call();
  SWITCH: {
      $option eq "print_form" && do {
	  &print_submit_form(@sources);
	  last SWITCH;
      };
      $option eq "process_form" && do {
	  $traj=form2traj($Form);
          check_vars($traj);
	  &form_reset($traj);
	  if (@Error) {
              &print_submit_form(@sources);
	  } else {
	      &print_confirm_form($$traj{no_points});
	  }
	  last SWITCH;
      };
      $option eq "end_form" && do {
	  $traj=form2traj($Form);
          my $email=get_email_address ($User);
          logrun($User,$traj);
 	  submit_job($User,$email,$traj);
	  &print_end_form($email);
	  last SWITCH;
      };
  }
    &print_form_tail;
    return
}

#
# Read the form
sub form2traj{
    my $form=shift;
    my %traj;
    $traj{date}=$form->param("date");
    $traj{time}=$form->param("time");
    $traj{length}=$form->param("length");
    $traj{direction}=$form->param("direction");
    $traj{lon}=$form->param("longitude");
    $traj{lat}=$form->param("latitude");
    $traj{lev}=$form->param("level");
    $traj{profile}=$form->param("profile");
    $traj{cluster}=$form->param("cluster");
    $traj{radius}=$form->param("radius");
    $traj{radius_units}=$form->param("radius_units");
    $traj{release_no}=$form->param("release_no");
    $traj{release_freq}=$form->param("release_freq");
    $traj{out_freq}=$form->param("out_freq");
    $traj{vert_adv}=$form->param("vert_adv");
    $traj{time_step}=$TimeStep;
    $traj{source}=$form->param("source") or $traj{source}=$Source;
    return \%traj;
}

#
# sub form_reset
# Purpose: reset the values of the form variables
sub form_reset{
    my $traj=shift;
    $Form->param(-name=>"date", -value=>$traj->{date});
    $Form->param(-name=>"time", -value=>$traj->{time});
    $Form->param(-name=>"length", -value=>$traj->{length});
    $Form->param(-name=>"longitude", -value=>$traj->{lon});
    $Form->param(-name=>"latitude", -value=>$traj->{lat});
    $Form->param(-name=>"level", -value=>$traj->{lev});
    $Form->param(-name=>"radius", -value=>$traj->{radius});
    $Form->param(-name=>"radius_units", -value=>$traj->{radius_units});
    $Form->param(-name=>"release_no", -value=>$traj->{release_no});
    $Form->param(-name=>"release_freq", -value=>$traj->{release_freq});
    $Form->param(-name=>"out_freq", -value=>$traj->{out_freq});
    $Form->param(-name=>"vert_adv", -value=>$traj->{vert_adv});
    $Form->param(-name=>"source", -value=>$traj->{source});
    return 1
}



#
# Determine which form is needed from submission environment
sub what_call{
    my $option;
    if ($Form->param("old_form") ne "yes" or
        $Form->param("Resubmit") ne "") {
      $option="print_form";
    } elsif ( $Form->param("Confirm") eq "") {
      $option="process_form";
    } else {
      $option="end_form";
    }
    return $option;
}

#
# Prints out the submit form
sub print_submit_form{

  my @sources = @_;

  my $javascript=<<END;
var help_win;
    
function help(url) {
  help_win=window.open(url,"help_window",
          "scrollbars,resizable,height=150,width=500");
  help_win.focus();
  return false;
}
END
    my $url=$Form->url(-absolute=>1);
    print $Form->header;
    print $Form->start_html(-title=>"BADC Trajectories - Submission",
			    -style=>
			    {-src=>"$style"},
                            -script=>$javascript,
                            -onUnload=>"if (help_win != null) help_win.close();")."\n";
    print qq(<H1><IMG SRC="/graphics/logos/badc_small_logo.gif" WIDTH="150" HEIGHT="60"
ALIGN="RIGHT" ALT="BADC Logo" BORDER="0">Atmospheric Trajectories<BR clear=all></H1>\n <HR>\n);
    print qq(<A HREF="$help"onClick="return help('$help');"><EM>Help</EM></A> on submitting trajectories.<HR>\n);
    &any_error();
#    print $Form->start_form(),"\n"; # encoding problem??
  print "<FORM method='post' action=\"$url\">\n";
    print "<H2>Data Source</H2>\n";
    print "<TABLE  border=0 cellspacing=0 cellpadding=3>\n<TR>";
	print "<TD>";
    print qq(<A HREF="$help#source"
        onClick="return help('$help#source');">Type</A>:);
    print "</TD></TR>";
    print "<TR><TD>\n";
#    for my $s (@sources) {
#      my $checked=($s eq $Source);
#      print "<TR><TD><INPUT TYPE=\"radio\" NAME=\"source\" VALUE=\"$s\" CHECKED>$source_lables{$s}</TD></TR>\n";
#    }
    print $Form->radio_group(-name=>'source', 
                              -values=>\@sources,
                              -linebreak=>'true',
                              -labels=>\%source_lables);
    print "</TD></TR>\n"; 
    print "</TABLE>";
    print "<H2>Time information</H2>";
    print "<TABLE  border=0 cellspacing=0 cellpadding=3>\n<TR>";
    print qq(<TD><A HREF="$help#date"
    onClick="return help('$help#date');">Start Date</A> ([yy]yymmdd):</TD><TD>);
    print $Form->textfield(-name=>"date", -size=>"8"),
	  "</TD></TR>\n";			# 
    print qq(<TR><TD><A HREF="$help#time"
      onClick="return help('$help#time');">Start Time</A> (hours):</TD>\n);
    print "<TD>",$Form->radio_group(-name=>"time" ,
                                    -values=>["00","06","12","18"]),
          "</TD></TR>\n";
    print qq(<TR><TD><A HREF="$help#length"
      onClick="return help('$help#length');">Length of run</A> (days):</TD><TD>);
    print $Form->textfield(-name=>"length", -size=>"2"),
    "</TD></TR>\n";
    print qq(<TR><TD><A HREF="$help#direction"
      onClick="return help('$help#direction');">Trajectory
    Direction</A>:</TD>\n<TD>);
    print $Form->radio_group(-name=>"direction",
                             -values=>["Forwards","Backwards"]);
    print "</TD></TR>";
    print "<TR><TD>";
 
    print qq(<A HREF="$help#output_freq"
      onClick="return help('$help#output_freq');">Output Frequency (hours)<A>:);
    print "</TD>\n";
    print "<TD>",$Form->radio_group(-name=>"out_freq",
                                    -values=>["0.5","1","3","6","24"]),
    "</TD></TR>\n";
    print "</TABLE>\n<HR>\n";
    print "<H2> Vertical Advection</H2>\n";
    print "<TABLE border=0 cellspacing=0 cellpadding=3>\n";
    print "<TR><TD>";
    print qq(<A HREF="$help#vert_advection"
      onClick="return help('$help#vert_advection');">
       Vertical Advection Type<A>:);
    print "</TD><TD>",
                 $Form->radio_group(-name=>"vert_adv",
                                    -values=>[0,1],-linebreak=>'true',
                                    -labels=>{0=>"3D",1=>"Isentropic"}),
    "</TD></TR>\n";
    print "</TABLE>\n<HR>\n";
    print "<H2> Initial parcel positions </H2>\n";
    print "<TABLE border=0 cellspacing=0 cellpadding=3 >\n";
    print qq(<TR><TD><A HREF="$help#longitudes"
      onClick="return help('$help#longitudes');">Longitudes</A> (degrees):</TD><TD>);
    print $Form->textfield(-name=>"longitude", -size=>"50"),
    "</TD></TR>\n";
    print qq(<TR><TD><A HREF="$help#latitudes"
      onClick="return help('$help#latitudes');">Latitudes</A> (degrees):</TD><TD>);
    print $Form->textfield(-name=>"latitude", -size=>"50"),
    "</TD></TR>\n";
    print qq(<TR><TD><A HREF="$help#levels"
      onClick="return help('$help#levels');">Levels</A> (hPa for 3D or K for isentropic):</TD><TD>);
    print $Form->textfield(-name=>"level", -size=>"50"),
    "</TD></TR>\n";
    print qq(<TR><TD>Use <A HREF="$help#profile"
      onClick="return help('$help#profile');">Profile?</A></TD><TD>) ;
    print $Form->checkbox(-name=>"profile",-label=>""),"</TD></TR>\n";
    print qq(<TR><TD>Use <A HREF="$help#cluster"
      onClick="return help('$help#cluster');">Cluster</A>?</TD><TD>);
    print $Form->checkbox(-name=>"cluster",-label=>"");
    print qq(Cluster <A HREF="$help#radius"
      onClick="return help('$help#radius');">Radius</A>: );
    print $Form->textfield(-name=>"radius", -size=>"4");
    print $Form->radio_group(-name=>"radius_units",
                             -values=>["deg","km"]),"</TD></TR>\n";
    print "</TABLE>\n";
    print "<HR>\n";
    print "<H2>Multiple time release of trajectories (Optional)</H2>\n";
    print "<TABLE border=0 cellspacing=0 cellpadding=3>\n";
    print qq(<TR><TD><A HREF="$help#release"
      onClick="return help('$help#release');">
      Number</A> of releases</TD><TD>);
    print $Form->textfield(-name=>"release_no", -size=>2);
    print "</TD></TR>\n";
    print qq(<TR><TD><A HREF="$help#frequency"
      onClick="return help('$help#frequency');">
      Frequency</A> of release (hours)</TD><TD>);
    print $Form->textfield(-name=>"release_freq", -size=>3);
    print "</TD></TR>\n";    # expid needed?
    print "</TABLE>\n";
    print "<HR>\n";
    print $Form->submit(-value=>"Submit"),"\n";
    print $Form->reset(),"\n";
    print $Form->hidden(-name=>"old_form",-value=>"yes"),"\n";
    print $Form->endform,"\n";
}

#
# Print the confirm form
sub print_confirm_form{
    my $no_points=shift;
    my $abs_length=abs $Form->param("length");

    my $url=$Form->url(-absolute=>1);
    print $Form->header;
    print $Form->start_html(-title=>"BADC Trajectories - Confirm",
			    -style=>
			    {-src=>"$style"})."\n";
    print qq(<H1><IMG SRC="/graphics/logos/badc_small_logo.gif" WIDTH="150" HEIGHT="60"
ALIGN="RIGHT" ALT="BADC Logo" BORDER="0">Atmospheric Trajectories<BR clear=all></H1>\n <HR>\n);
    &any_error();
    print "<FORM method='post' action=\"$url\">\n";
#    print $Form->startform,"\n";
    print <<"EOF";
The following options have been selected.  If they are correct Confirm,
if you want to change the options Resubmit. <BR>
<HR>
<TABLE border=0 cellspacing=0 cellpadding=3>
EOF
    my %source_type=("ecmwf_2.5p","ECMWF Archive",
                         "ecmwf_2.5mf","ECMWF forecast/current analyses");
    print "<TR><TD>Wind Source:</TD><TD>",$source_lables{$Form->param("source")},"</TD></TR>\n";
    print "<TR><TD> Start Date:</TD> <TD> ",$Form->param("date"),"</TD></TR>\n";
    print "<TR><TD> Start Time:</TD> <TD> ",$Form->param("time"),"</TD></TR>\n";
    print "<TR><TD> Length of run (days):</TD> <TD>",$abs_length,"</TD></TR>\n";
    print "<TR><TD> Trajectory Direction:</TD> <TD>",$Form->param("direction"),"</TD></TR>\n";
    print "<TR><TD> Vertical Advection:</TD> <TD>",
       ("3D","Isentropic")[$Form->param("vert_adv")],
       "</TD></TR>\n";
    print qq(<TR><TD> Number of trajectories:</TD> <TD>$no_points</TD></TR>);
    if ($Form->param("release_no") > 1) {
	print "<TR><TD>Number of releases: </TD><TD>".$Form->param("release_no")."</TD></TR>\n";
        print "<TR><TD>Frequency of release (hours): </TD><TD>".$Form->param("release_freq")."</TD></TR>\n";
    }
    print "</TABLE>\n";
    print $Form->submit(-name=>"Confirm", -value=>"Confirm"),"\n";
    print $Form->submit(-name=>"Resubmit", -value=>"Resubmit"),"\n";
    print $Form->hidden(-name=>"date"),"\n";
    print $Form->hidden(-name=>"time"),"\n";
    print $Form->hidden(-name=>"length"),"\n";
    print $Form->hidden(-name=>"direction"),"\n";
    print $Form->hidden(-name=>"longitude"),"\n";
    print $Form->hidden(-name=>"latitude"),"\n";
    print $Form->hidden(-name=>"level"),"\n";
    print $Form->hidden(-name=>"profile"),"\n";
    print $Form->hidden(-name=>"cluster"),"\n";
    print $Form->hidden(-name=>"radius"),"\n";
    print $Form->hidden(-name=>"radius_units"),"\n";
    print $Form->hidden(-name=>"release_no"),"\n";
    print $Form->hidden(-name=>"release_freq"),"\n";
    print $Form->hidden(-name=>"out_freq"),"\n";
    print $Form->hidden(-name=>"vert_adv"),"\n";
    print $Form->hidden(-name=>"old_form"),"\n";
    print $Form->hidden(-name=>"source"),"\n";

    print $Form->endform,"\n";
}

sub  print_end_form{
    my $email=shift;
    print $Form->header;
    print $Form->start_html(-title=>"BADC Trajectories - Submitted",
			    -style=>
			    {-src=>"$style"})."\n";
    print qq(<H1><IMG SRC="/graphics/logos/badc_small_logo.gif" WIDTH="150" HEIGHT="60"
ALIGN="RIGHT" ALT="BADC Logo" BORDER="0">Atmospheric Trajectories<BR clear=all></H1>\n <HR>\n);
    print <<"EOF";
The trajectory job has been submitted. <BR>
You will be emailed at $email when the job has finished and the output is 
ready for plotting.<BR>
Thankyou for using the BADC trajectory service.<BR>
<BR>
<A HREF="$traj_top">Back</A> to the main trajectory page
EOF
}

sub print_form_tail {
    print "<HR>\n";
    print qq(<EM>Please report any problems with this page to:</EM>
		     <ADDRESS><A HREF="$badc_support">
BADC User Support</A>\n);
    print $Form->end_html."\n";
    
}


#
# sub get_mail
# Purpose: retrieve the email of a user from the htpasswd file
sub get_mail {
    my ($auser)=shift;
    my $amail;
    my %luser;
    my $apasswd=$ENV{"DOCUMENT_ROOT"};
    $apasswd =~ s!htdocs$!etc/htpasswd!;
    dbmopen %luser,$apasswd,0444;
    if ( $luser{$auser} ) {
	$amail=(split /:/,$luser{$auser})[2];
    }
    dbmclose %luser ;
    return $amail;
}

sub any_error {
#
# Process the errors and warnings
  if ( @Error ) {
      process_messages("error",@Error);
  } 
  if ( @Warning ){
      process_messages("warning",@Warning);
  }

  return @Error!=0;
}

sub process_messages{
#
# print the error or warning arrays
    my ($string,@message)=@_;
    print "<H2>\u$string from form processing</H2>\n";
    foreach my $message (@message){
	print "\u$string: $message<BR>\n";
    }
    print "<HR>\n";
} 
#-------------------------------------------------------------------------------
sub get_email_address {
#
# Extracts email address from passwd file for given accountID.
#
# 29/07/10 ASH Should probably extract the email address from the userdb, but this seems to work ok...
#
  my $accountID = shift;

  my $line = get_passwd_line ($accountID);
  if (not $line) {return undef}

  my ($account, $encPasswd, $uid, $gid, $info, $home, $shell) = split (/:/, $line);
#
# Extract email address and trim off any blanks (don't think there should be any,
# but just in case...)
#
  my $email = (split (/email=/, $info))[1];
  $email =~ s/^\s+//;
  $email =~ s/\s+$//;
  
  return $email;
}
#-------------------------------------------------------------------------------
sub get_passwd_line {
#
# Returns complete line of passwd file for given accountID. Returns undef if
# accountID was not found.
#
  my $accountID = shift;

#
# Make sure account name is untainted
#
  $accountID =~ /(.*)/;
  $accountID = $1;
#
# Set Userid and effective userID to 'root'. Needed in order for us to read from passwd
# file (even though program is run as 'set uid root'
#
  $UID  = 0;
  $EUID = 0;
  
  my $cmd = "grep '^$accountID:' $PASSWD_FILE";
  my $line = `$cmd`;
  chop $line;

  return $line;
     
}
#-------------------------------------------------------------------------------
sub print_no_resource {

   my $user = shift;
   
    print<<"EOF";
Content-Type: text/html

<HTML>
<HEAD>
<TITLE>Unauthroised Resource</TITLE>
</HEAD>
<BODY BGCOLOR=white>
<H2>Unauthorised Resource</H2>
<P>
You are logged on as user $user.</P>
<P>$user is not authorised to use the restricted web resource that you have 
tried to access.  You must apply for access to the restricted resource. 
</BODY>
</HTML>
EOF
     exit 0;
}

