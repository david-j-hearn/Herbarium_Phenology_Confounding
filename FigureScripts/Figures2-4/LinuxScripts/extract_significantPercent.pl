#!/usr/bin/perl
#

$file = $ARGV[0];
$hdrsP = 0;
open(INo, "$file") or die "Could not open file $file. File should have each line with a name of a species tab file with info\n";
while($line = <INo>) {


	chomp($line);

	if($line =~ m/^\s*#/) { next; }

	($name,$file) = split(/\t/,$line);

	undef %infoH;
	undef %infoHc;

	$infoH{"name"} = $name;

open(IN, "$file") or die "Could not open file $file\n";
while($line = <IN>)
	{
	if($line =~ m/^.1. "(.+) ([\d.e-]+)\s+([\d.e-]+)"/)
		{
		$analysis = $1;
		$pVal_Date = $2;
		$pVal_Lat = $3;
		$a1 = $analysis . "_Date";
		$a2 = $analysis . "_Lat";
		if($pVal_Date < 0.05 ) { $infoH{$a1} += 1; }
		else { $infoH{$a1} += 0; }
		$infoHc{$a1}++;
		if($pVal_Lat < 0.05 ) { $infoH{$a2} += 1; }
		else { $infoH{$a2} += 0; }
		$infoHc{$a2}++;
		}
	if($line =~ m/^.1. "(.+) ([\d.e-]+)"/)
		{
		$analysis = $1;
		$pVal = $2;

		if($pVal < 0.05) { $infoH{$analysis} += 1; }
		else { $infoH{$analysis} += 0; }
		$infoHc{$analysis}++;
		}
	}
	close(IN);

	#$infoH{"LatVDOYMLat"} += 0.000001;
	#$infoH{"DateVLat"} += 0.000001;
	#$infoH{"YearVSS"} += 0.000001;

	foreach $key (keys %infoH) {
		if($key ne "name") {
			$infoH{$key} /= $infoHc{$key};
		}
	}
	

	if(!$hdrsP) {
		#print "name\tShapiro.Wilks\tLatVDOY\tLatVDOYMLat\tDateVLat\tYearVSS\tDateVDOY\tDatePLatVDOY_Date\tDatePLatVDOY_Lat\tDateVMinDOY\tDateVDOYMLat\tDateVMinDOYMLat\tDateVMinDOYMSS\tDateVMinDOYMLatMSS\n";
	print "name\tShapiro.Wilks\tDateVLat\tYearVSS\tLatVDOY\tDatePLatVDOY_Lat\tLatVDOYMLat\tDateVDOY\tDateVDOYMLat\tDatePLatVDOY_Date\tDateVMinDOY\tDateVMinDOYMLat\tDateVMinDOYMSS\tDateVMinDOYMLatMSS\n";
	$hdrsP=1;
	}

	print $infoH{"name"} . "\t" . $infoH{"Shapiro.Wilks"} . "\t" . $infoH{"DateVLat"} . "\t" . $infoH{"YearVSS"} . "\t" . $infoH{"LatVDOY"} . "\t" . $infoH{"DatePLatVDOY_Lat"} . "\t" . $infoH{"LatVDOYMLat"} . "\t" . $infoH{"DateVDOY"} . "\t" . $infoH{"DateVDOYMLat"} . "\t" . $infoH{"DatePLatVDOY_Date"} . "\t" . $infoH{"DateVMinDOY"} . "\t" . $infoH{"DateVMinDOYMLat"} . "\t" . $infoH{"DateVMinDOYMSS"} . "\t" . $infoH{"DateVMinDOYMLatMSS"} . "\n";

}
close(INo);
