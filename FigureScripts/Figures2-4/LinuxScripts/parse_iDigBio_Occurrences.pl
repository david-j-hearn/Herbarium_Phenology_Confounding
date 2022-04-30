#!/usr/bin/perl
#

use Text::CSV;


#1	coreid
#20	idigbio:eventDate
#36	idigbio:geoPoint
#69	dwc:scientificName
#71	dwc:startDayOfYear
#


my $file = $ARGV[0] or die "Need to get CSV file on the command line\n";

open(IN, $file);
print "$file\n";
while($line = <IN>) {
	chomp($line);
	if(!($line =~ /^\s*#/)) {
	($key,$value) = split(/\t/,$line);
	$infoH{$key} = $value;
}
}
close(IN);

use Text::CSV qw();
my $csv = Text::CSV->new({
    auto_diag   => 2,       # automatic error checking CSV methods
    binary      => 1,
    eol         => "\N{CR}\N{LF}\n",
    eol         => "\n",
    sep_char    => ",",
    allow_loose_quotes => 1,
}) or die 'Cannot use CSV: ' . Text::CSV->error_diag;

$file = $info{"file"};
open my $fh, '<:encoding(ASCII)', $infoH{"file"} or die "\tCould not open file\n";
open(OUT, ">" . $infoH{"output"});

print OUT "ID\tspecies\tdate\tlatitude\tlongitude\tdayOfYear\n";
while (my $row = $csv->getline($fh)) {

#1	coreid
#20	idigbio:eventDate
#36	idigbio:geoPoint
#69	dwc:scientificName
#71	dwc:startDayOfYear
#
	my $id = $row->[0];
	my $date = $row->[19];
	my $latlong = $row->[35];
	my $name = $row->[68];
	my $doy = $row->[70];

	if(!$date || lc($infoH{"scientific name"}) ne lc($name)) { 
		#print "'" . lc($name) . "' '" . lc($infoH{"scientific name"}) . "'\n";
		#print "date: '" . $date . "'\n";
	}
	else {
		#print "$id\t$date\t$latlong\t$name\t$doy\n";
		($year,$month,$day) =  parseDate($date);
		$daysInYear = calculateYearLength($year);
		$dateDec = $year + $doy/$daysInYear; 
		#print "$date\t$dateDec\n";
		($lat,$long) = parseLatLong($latlong);
		#print "($lat, $long)\n";
		if(filterLatLon($lat,$long) && filterDate($dateDec, $doy)) {
			print OUT "$id\t" . uc($name) . "\t$dateDec\t$lat\t$long\t$doy\n";
		}
		else {
			#print "NOT OK $id\t$date\t$latlong\t$name\t$doy\n";
		}


	}
		
}
close $fh;
close OUT;

sub filterDate {
	my $dateD = $_[0];
	my $doy = $_[1];
	if($doy != $infoH{"bad doy"}) { return(1); }
	return(0);
}

sub filterLatLon {
	$lat = $_[0];
	$long = $_[1];
	if($lat > $infoH{"min lat"} && $long > $infoH{"min long"} && $long < $infoH{"max long"}) { return(1); }
	return(0);
}

sub parseLatLong {
	my $latlong = $_[0];
	#{"lat": 37.716389, "lon": -84.296111}
	#
	if($latlong =~ m/..lat..\s*([\d.-]+),\s*.lon..\s*([-.\d]+).$/)
		{
			$lat = $1;
			$long = $2;
			return($lat,$long);
		}
		else {
			print "Lat / Long not the correct format: $latlong. Quitting\n";
			exit(0);
		}

}

sub calculateYearLength
	{
		my $year = $_[0];
		#leap year is divisible by 4
		if($year %4==0) 
			{
				#except if year is divisible by 100 and not divisible 400
				if($year%100 == 0 && $year%400!=0) {
					return(365);
				}
					return(366);
			}
	return(365);
	}

sub parseDate { 
	my $date = $_[0];
	#1992-04-10T00:00:00+00:00
	if($date =~ m/(\d\d\d\d)-(\d\d)-(\d\d).*/)
		{
			my $year = $1;
			my $month = $2;
			my $day = $3;
			return($year,$month,$day);
		}
	else {
		print "Couldn't parse date $date. Quitting.\n";
		exit(0);

		}
	}
sub outlier_filter { return $_[1] > 1; }

