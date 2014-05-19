
#  ----------------------------------------------------------------------------
#  "THE BEER-WARE LICENSE":
#  <ivan@sanchezortega.es> wrote this file. As long as you retain this notice you
#  can do whatever you want with this stuff. If we meet some day, and you think
#  this stuff is worth it, you can buy me a beer in return.
#  ----------------------------------------------------------------------------
# 
# 
# Multi-geo extension for TTYtter.
# 
# This extension builds upon the gpsd, geoip, place, teleport, where and where-inline extensions, trying to unifying all of them to provide an improved geolocation experience on TTYtter.
# 
# Read http://ivan.sanchezortega.es/ttytter for details on the commands provided.
# 
#
#
#
# TODO: Implement geocoder cache
# FIXME: Somehow turn on localization for geocoders. At least GeoNames allows a "lang=xx" parameter to be passed.
# FIXME: Let /place and /teleport override GPSD, at least when GPS is offline / there is no fix.
#        This shall be accomplished by setting a override flag, cleaned up upon "/place off".
#        Thus, GPSD would check that flag and NOT undef(lat) and undef(lon) if flag is on.
# TODO: Warn users that Geo::IP needs STUN::Client (as scraping whatismyip.org no longer works)
# TODO: Add Twitter's geocode API (see https://dev.twitter.com/docs/api#places-geo)
# TODO: Resolve FourSquare URLs into lat-long if tweet is not geoenabled (regexp for "lat":-1.23456,"lng":-1.23456, )

use Data::Dumper qw{Dumper};



print "-- Loading MultiGeo extension\n";





our %geocoder_cache = ();

our %store = ( "cache_misses", 0, "cache_limit", 2000, "cache_flushes", 0, "cache_hit_count", 0);

my $reversegeocoder;
my $geocoder;


# TODO: allow remote hosts for GPSD, via extpref variables.
my $gpsd;
my $gpsd_host;
my $gpsd_port;
my $gpsd_active = false;


if ($extpref_multigeo_gpsd)
{
	print "-- MultiGeo will use GPSD to locate your tweets\n";
	
	require Net::GPSD3;
	$host=shift || undef;
	$port=shift || undef;

	$gpsd=Net::GPSD3->new(host=>$host, port=>$port); #default host port as undef
	
	$gpsd_active = true;
}
else
{
	print "-- Will not use GPSD\n";
}


if ($extpref_multigeo_geoip)
{
	print "-- MultiGeo will use GeoIP to locate your tweets\n";
	print "-- This will use STUN (Simple Traversal of UDP through NATs) to determine your public IP address.\n";
	
	require Geo::IP;
	require STUN::Client;
}
else
{
	print "-- Will not use GeoIP\n";
}




if (not $extpref_multigeo_geocoder)
{
	print "-- No geocoder specified, falling back to OpenStreetMap's Nominatim.\n";
	$extpref_multigeo_geocoder = "nominatim";
}



# Which geocoder will we use?

if ($extpref_multigeo_geocoder eq "geonames")
{
	print "-- Will use GeoNames for geocoding. Have a look at http://www.geonames.org .\n";
	
	$reversegeocoder = sub
	{
		my $latitude = shift;
		my $longitude = shift;
		
		my $r = &grabjson("http://api.geonames.org/findNearbyPlaceNameJSON?formatted=true".
		"&lat=" . $latitude .
		"&lng=" . $longitude .
		"&username=ttytter&style=medium",0,1);
		
		if ($r->{'geonames'}->[0])
		{
			my $location = $r->{'geonames'}->[0]->{'name'} . ", " . $r->{'geonames'}->[0]->{'adminName1'} . ", " . $r->{'geonames'}->[0]->{'countryName'} ;
			return $location;
		}
		else
		{
			our $exception;
			&$exception(2,"*** Reverse geocoder didn't find any results for given coordinates ($latitude,$longitude). Daily capacity (30000 requests for all ttytter users) might have been reached. Check if geonames.org returns any result.\n");
			return;
		}
	};
	
	
	$geocoder = sub
	{
		my $placename = shift;
		my $latitude  ;
		my $longitude ;
		
		my $r = &grabjson("http://api.geonames.org/searchJSON?maxRows=1&formatted=true".
		"&q=" . URLEncode($placename) .
		"&username=ttytter&style=medium",0,1);
		
		if ($r->{'geonames'}->[0])
		{
			$latitude  = $r->{'geonames'}->[0]->{'lat'} ;
			$longitude = $r->{'geonames'}->[0]->{'lng'} ;
			$placename = $r->{'geonames'}->[0]->{'name'} . ", " . $r->{'geonames'}->[0]->{'adminName1'} . ", " . $r->{'geonames'}->[0]->{'countryName'} ; ;
		}
		else
		{
			our $exception;
			&$exception(2,"*** Geocoder didn't find any results for given placename ($placename). Daily capacity (30000 requests for all ttytter users) might have been reached. Check if geonames.org returns any result.\n");

		}
		
# 		my %result = ("latitude", $latitude, "longitude", $longitude, "placename", $placename);
		return [$latitude, $longitude, $placename];
	};
	
	

}
elsif  ($extpref_multigeo_geocoder eq "nominatim")
{
	print "-- Will use the OpenStreetMap geocoding service, Nominatim. Geolocation data is CC-by-sa OpenStreetMap contributors. Have a look at http://wiki.openstreetmap.org/wiki/Nominatim\n";
	
	$reversegeocoder = sub
	{
		my $latitude = shift;
		my $longitude = shift;
		
		my $r = &grabjson("http://nominatim.openstreetmap.org/reverse".
		"?lat=" . $latitude .
		"&lon=" . $longitude .
		"&format=json&user-agent=ttytter",0,1);
	
		my $location = $r->{'display_name'};
		
		return $location;
	};
	
	
	$geocoder = sub
	{
		my $placename = shift;
		my $latitude  ;
		my $longitude ;

		my $r = &grabjson("http://nominatim.openstreetmap.org/search".
		"?q=" . URLEncode($placename) . 
		"&format=json&limit=1&user-agent=ttytter",0,1);

# 			print Dumper("http://nominatim.openstreetmap.org/search".
# 				"?q=" . URLEncode($command) . 
# 				"&format=json&limit=1&user-agent=ttytter",0,1);;
# 			print Dumper($r);
# 			print Dumper($r->[0]);
		
		if ($r->[0])
		{
			$placename = $r->[0]->{'display_name'} . " (a " . $r->[0]->{'type'} . ")";
			$latitude  = $r->[0]->{'lat'};
			$longitude = $r->[0]->{'lon'};
		}
		else
		{
			our $exception;
			&$exception(2,"*** No results found for that place. Check your spelling. Or visit www.osm.org to see if the place is listed under other spelling.\n");

		}
		
# 		my %result = ("latitude", $latitude, "longitude", $longitude, "placename", $placename);
		return [$latitude, $longitude, $placename];
	};
	
	
	
}
elsif  ($extpref_multigeo_geocoder eq "twitter")
{
	print "-- Will use the Twitter geocoding service via its API v1.1\n";
	
	$reversegeocoder = sub
	{
		my $latitude = shift;
		my $longitude = shift;
		
		my $r = &grabjson("https://api.twitter.com/1.1/geo/reverse_geocode.json".
		"?lat=" . $latitude .
		"&long=" . $longitude .
		"&granularity=poi&max_results=1&user-agent=ttytter",0,0);

# 		print Dumper($r);
		
		my $location = $r->{'result'}->{'places'}->[0]->{'name'};
		
		if ($r->{'result'}->{'places'}->[0]->{'contained_within'})
		{
			$location = $location . ", " .
			            $r->{'result'}->{'places'}->[0]->{'contained_within'}->[0]->{'name'} . ", " .
			            $r->{'result'}->{'places'}->[0]->{'contained_within'}->[0]->{'country'};
		}
		
		return $location;
	};
	
	
	$geocoder = sub
	{
		my $placename = shift;
		my $latitude  ;
		my $longitude ;

		my $r = &grabjson("https://api.twitter.com/1.1/geo/search.json".
		"?query=" . URLEncode($placename) . 
		"&granularity=poi&max_results=1&user-agent=ttytter",0,0);

# 			print Dumper($r);
		
		if ($r->{'result'}->{'places'}->[0])
		{
			$placename = $r->{'result'}->{'places'}->[0]->{'name'} . " (a " . $r->{'result'}->{'places'}->[0]->{'place_type'} . ")";
			$latitude  = ($r->{'result'}->{'places'}->[0]->{'bounding_box'}->{'coordinates'}->[0]->[0]->[0] +
			              $r->{'result'}->{'places'}->[0]->{'bounding_box'}->{'coordinates'}->[0]->[2]->[0] ) /2;
			$longitude = ($r->{'result'}->{'places'}->[0]->{'bounding_box'}->{'coordinates'}->[0]->[0]->[1] +
			              $r->{'result'}->{'places'}->[0]->{'bounding_box'}->{'coordinates'}->[0]->[2]->[1] ) /2;
		}
		else
		{
			our $exception;
			&$exception(2,"*** No results found for that place..\n");

		}
		
# 		my %result = ("latitude", $latitude, "longitude", $longitude, "placename", $placename);
		return [$latitude, $longitude, $placename];
	};
	
	
	
	
	
}
elsif ($extpref_multigeo_geocoder eq "none")
{
	print "-- Will not use any geocoder at all.\n";
	
	$reversegeocoder = sub
	{
		my $latitude = shift;
		my $longitude = shift;
		
		return ;
	};
	
	$geocoder = sub
	{
		my $placename = shift;
		
		return ;
	};
}
else
{
	our $exception;
	&$exception(2,"*** Invalid geocoder specified for multigeo.pl. Please check your configuration.\n");	
	
	$reversegeocoder = sub
	{
		my $latitude = shift;
		my $longitude = shift;
		
		return ;
	};
	
	$geocoder = sub
	{
		my $placename = shift;
		
		return ;
	};
}







# Auxiliary stuff for direct geocoder (to turn placenames into URL-friendly strings)
sub URLEncode {
	my $theURL = $_[0];
	$theURL =~ s/([\W])/"%" . uc(sprintf("%2.2x",ord($1)))/eg;
	return $theURL;
};







# Prepost will handle GeoIP and GPSD.

$prepost = sub {
	our ($lat,$long,$superverbose);
	
	# Let's try GPSD first
	if ($extpref_multigeo_gpsd and $gpsd_active)
	{
		  # ...fetch lat and long from GPSD ...
		  my $poll=$gpsd->poll;
		  print Dumper($poll) if $superverbose;

		  if ($poll->active == 0) {
		    print "-- GPS offline, not using geolocation.\n";
		    $lat  = undef;
		    $long = undef;
		  } elsif ($poll->tpv->mode == 0) {
		    print "-- No GPS fix, not using geolocation.\n";
		    $lat  = undef;
		    $long = undef;
		  } else {
		    # ... and update ttytter's lat and long vars...
		#     $lat =  $poll->tpv->lat + rand() - 0.5;
		#     $long = $poll->tpv->lon + rand() - 0.5;
		    $lat =  $poll->tpv->lat ;
		    $long = $poll->tpv->lon ;
		    print "-- GPSD returned coordinates $lat,$lon\n" if ($verbose);
		  }
		
	}
	
	# Now that GPSD has had a chance to get a fix, run through GeoIP in case GPSD didn't work.
	# The order of extension loading should not affect the result of using GPSD over GeoIP, 
	
	# GeoIP is disabled for the time being, due to whatismyip.org being down. If anyone knows about a good method to fetch the computer's current IP address, please let me know. And don't say "STUN", because the CPAN packages don't work.
# 	if ($extpref_multigeo_geoip)
# 	{
# 		if(!$Lib_firstrun){	# Make sure we only tun GeoIP logic just once.
# 			$Lib_firstrun = true;
# 			
# 			
# 			if ($lat || $long )	# Alas, this will not work on Null Island
# 			{
# 				print "-- Coordinates already set, GeoIP will not run\n";
# 			}
# 			else
# 			{
# 				print "-- GeoIP extension will try to locate your public IP address now\n";
# 				$ip_addr = &backticks($baseagent, '/dev/null', undef, 'http://whatismyip.org', undef, 0, undef);
# 
# 				# FIXME: How do we make this work on win32 systems?
# 				my $gi = Geo::IP->open("/usr/share/GeoIP/GeoIPCity.dat", GEOIP_STANDARD);
# 				my $record = $gi->record_by_addr($ip_addr);
# 				
# 				if (!$record ||
# 				    !$record->latitude  ||
# 				    !$record->longitude )
# 				{
# 					print "-- GeoIP: Sorry, your IP address $ip_addr could not be geolocated.\n";
# 				}
# 				else
# 				{
# 					my $city    = $record->city;
# 					my $region  = $record->region_name;
# 					my $country = $record->country_name;
# 					   $lat     = $record->latitude;
# 					   $long    = $record->longitude;
# 					
# 					print "-- GeoIP: $ip_addr is near $city, $region, $country\n";
# 				}
# 			}
# 		}
# 	}
	
	my $tweet = shift;
	return $tweet;
};















# Addaction will handle /place, /teleport and /gpsd
$addaction = sub {
	my $command = shift;
	our ($lat,$long);

	
	# /place
	if ($command =~ s#^/place ## && length($command)) {
		
		if ($extpref_multigeo_gpsd and $gpsd_active)
		{
			print(2,"-- Automatically turning off GPSD support. Turn on again with \"/gpsd on\".\n");
			$gpsd_active = false;
		}
		
		if ($command eq "off") {
			print ("-- Turning geolocation off\n");
			undef($lat);
			undef($long);
			return 1;
		} else {
			&$utf8_encode($command);
			
			$data = &$geocoder($command);	# Returns [lat, lon, placename]
			
			if ($data->[0] ne undef and $data->[1] ne undef)
			{
# 				&$utf8_decode($location);
				my $placename = &descape($data->[2]);
				
				$lat  = $data->[0];
				$long = $data->[1];
				
				print ("-- Your next tweets will be sent at " . $lat . "," . $long . ", which is near " . $placename . "\n" );
			}
			else
			{
				print ("*** Geocoder failed, will not geolocate your next tweets.\n")
			}
			return 1;
		}
	}
	else
	{
		if ($command =~ m/^place$/)	# Empty /place command, just turn geolocation off.
		{
			print ("-- Turning geolocation off\n");
			undef($lat);
			undef($long);
			return 1;
		}
	}
	
	
	# /teleport
	if ($command =~ s#^/teleport ## && length($command)) {
		if ($extpref_multigeo_gpsd and $gpsd_active)
		{
			print(2,"-- Automatically turning off GPSD support. Turn on again with \"/gpsd on\".\n");
			$gpsd_active = false;
		}
		
		
		my $tweet = &get_tweet($command);
		if (!$tweet->{'id_str'}) {
			print $stdout "-- sorry, no such tweet (yet?).\n";
			return 1;
		}
		if ($tweet->{'user'}->{'geo_enabled'} ne 'true' ||
			($tweet->{'geo'}->{'coordinates'}->[0] eq 'undef')) {
			print $stdout "-- sorry, no geoinformation in that tweet.\n";
			return 1;
		}
		
		$lat  = $tweet->{'geo'}->{'coordinates'}->[0];
		$long = $tweet->{'geo'}->{'coordinates'}->[1];
		
		print ("-- Your next tweets will be sent at " . $lat . "," . $long . ".\n" );
		
		return 1;
	}
	
	
	# /gpsd
	if ($command =~ s#^/gpsd ## && length($command)) {
		if (not $extpref_multigeo_gpsd)
		{
			our $exception;
			&$exception(2,"*** Use either \"/gpsd on\" or \"/gpsd off\" to enable/disable GPSD, or just \"/gpsd\" to query.\n");		
			return 1;
		}
		if ($command eq "on")
		{
			print(2,"-- GPSD support is now enabled.\n");
			$gpsd_active = true;
		}
		elsif ($command eq "off")
		{
			print(2,"-- GPSD support is now disabled.\n");
			$gpsd_active = false;
		}
		else
		{
			our $exception;
			&$exception(2,"*** Use either \"/gpsd on\" or \"/gpsd off\" to enable/disable GPSD, or just \"/gpsd\" to query.\n");	
		}
		
		return 1;
	}
	elsif ($command =~ m/^gpsd$/)	# Empty /place command, just turn geolocation off.
	{
		if (not $extpref_multigeo_gpsd)
		{
			our $exception;
			&$exception(2,"*** Use either \"/gpsd on\" or \"/gpsd off\" to enable/disable GPSD, or just \"/gpsd\" to query.\n");		
			return 1;
		}
		# ...fetch lat and long from GPSD ...
		my $poll=$gpsd->poll;
		print Dumper($poll) if $debug;

		if ($poll->active == 0) {
			print "-- GPS offline.\n";
		} elsif ($poll->tpv->mode == 0) {
			print "-- No GPS fix.\n";
		} else {
			my $lat =  $poll->tpv->lat ;
			my $long = $poll->tpv->lon ;
			print "-- GPSD returned coordinates $lat,$lon\n";
		}
		return 1;
	}
	
	# No matches, run through any other command hooks.
	return 0;
};










# $handle will manage the where-inline funcionality, printing the reverse-geocoded placename with every geolocated tweet.
# Now supercharged with geocoder cache!

$handle = sub {
	my $tweet = shift;
	our $verbose;
	
	
	&defaulthandle($tweet);

	if ($tweet->{'user'}->{'geo_enabled'} ne 'true' ||
		($tweet->{'geo'}->{'coordinates'}->[0] eq 'undef')) {
# 		print $stdout "-- sorry, no geoinformation in that tweet.\n";
		return 1;
	}
	
	my $tw_lat = $tweet->{'geo'}->{'coordinates'}->[0];
	my $tw_lon = $tweet->{'geo'}->{'coordinates'}->[1];
	
	if ($geocoder_cache{$tw_lat.",".$tw_lon})
	{
		$placename = $geocoder_cache{$tw_lat.",".$tw_lon};
		print $stdout "-- Geocoder cache hit: " . $tw_lat . "," . $tw_lon . " -> " . $placename . "\n" if ($verbose);
		$store->{cache_hit_count} += 1;
	}
	else
	{
		print $stdout "-- Querying geocoder: " . $tw_lat . "," . $tw_lon . "\n" if ($verbose);
		
		$placename = &$reversegeocoder( $tw_lat , $tw_lon );
		
		$placename = &descape($placename);	# This has changed with ttytter 2.0.02 - before, utf8_decode was used.

				
		$geocoder_cache{$tw_lat.",".$tw_lon} = $placename;
		$store->{cache_misses} += 1;
		
	}

	# Basically copied from ttytter.pl's defaulthandle
	my $menu_select = $tweet->{'menu_select'};
	
	$menu_select = (length($menu_select) && !$script)
		? (($menu_select =~ /^z/) ?
			"${EM}${menu_select}+${OFF} " :
			"${menu_select}+ ")
		: '';

	print $streamout ($menu_select . " " . $placename . "\n" );
	
	return 1;
};









# Show cache statistics. Not really useful, but hey.
# $shutdown  = sub {
# 	our $verbose;
# 	our $is_background;
# 
# 	if ($is_background)
# 	{
# 		our $store;
# 		$cache_hits  = $store->{'cache_hit_count'};
# 		$cache_miss  = $store->{'cache_misses'} + ($store->{'cache_limit'} * $store->{'cache_flushes'} );
# 		$cache_flush = $store->{'cache_flushes'};
# 		$context = "background";
# 	}
# 	else
# 	{
# 		print "-- Fetching geocoder cache stats from background process\n" if $verbose;
# 		$cache_hits  = getbackgroundkey('cache_hit_count');
# 		$cache_miss  = getbackgroundkey('cache_misses') + (getbackgroundkey('cache_limit') * getbackgroundkey('cache_flushes') );
# 		$cache_flush = getbackgroundkey('cache_flushes');
# 		$context = "foreground";
# 		print "-- Fetched geocoder cache stats from background process\n" if $verbose;
# 	}
# 
# # 	$store->{deshortify_cache_misses} += ($store->{deshortify_cache_limit} * $store->{deshortify_cache_flushes});
# # 	print $stdout "-- Deshortify cache stats (misses/hits/flushes): $store->{deshortify_cache_misses}/$store->{deshortify_cache_hit_count}/$store->{deshortify_cache_flushes}\n" if $verbose;  
# # 	sendbackgroundkey('deshortify_cache_misses', getbackgroundkey('deshortify_cache_misses') + (getbackgroundkey('deshortify_cache_limit') * getbackgroundkey('deshortify_cache_flushes'))  );
# 
# 	print $stdout "-- Geocoder cache stats (misses/hits/flushes/context): $cache_hits/$cache_miss/$cache_flush/$context\n" if ($verbose);
# 
# 	return 0;
# }




