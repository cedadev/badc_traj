#!/usr/local/bin/perl
#
# Processes trajectory log file entries and writes out in a format suitable for
# reading into the user database. Only the date and accountID are extracted.
#
# History:
#
#    21-Feb-2001 A.Harwood First version
# 
#use strict;

my $file = shift;

open LU, $file;
my $oldmonth ="";
while (<LU>) {
   chop;
   my @fields = split (",");
   
   my $day = substr($fields[0], 8, 2);
   my $month = substr($fields[0], 4, 3);
   my $year  = substr($fields[0], 20, 4);
   my $date = "$day-$month-$year";
   
   my $number = $fields[$#fields-2];
   my $user = $fields[$#fields-1];

   if ($oldmonth ne $month) {
      $totusers= keys(%user) ;
      print "$year $oldmonth $totusers,$totnumber,$totjobs\n";
      $totnumber =0 ;
      $totjobs=0;
      %user=();
      $oldmonth = $month;
   }
   
   $totnumber += $number;
   $totjobs++;
   $user{$user}=1;

}
$totusers= keys(%user) ;
print "---->$year $oldmonth $totusers,$totnumber,$totjobs\n";
