#!/usr/bin/perl
#finger.pl

$file="finger.temps";

open(DATA, "<$file");
while(<DATA>)
 {
 $line = $_;
 chomp($line);
 if($line =~ m|(\d\d)(..)(..)(..)|) {
  ($dev,$cent,$crem,$cperc) = (hex($1)/8+1,hex($2),hex($3),hex($4));
  # print "match\n";
  #temp=read_temp-.25+((count_per_c1-count_remain1)/count_per_c1)
  $temp=$cent-.25+($cperc-$crem)/$cperc;
  $temp=$temp * 9 / 5 + 32;		#to fahrenheit
  printf "%.4f",$temp;
  if ($dev == 4) {
    print "\n";
  } else {
    print "\t";
  }
 }
}
print "\n";