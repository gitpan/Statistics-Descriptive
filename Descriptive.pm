package Statistics::Descriptive;

##This module draws heavily from perltoot v0.4 from Tom Christiansen.

package Statistics::Descriptive::Sparse;
use strict;
use vars qw($VERSION $AUTOLOAD %fields);
use Carp;

##Define the fields to be used as methods
%fields = (
  count			=> 0,
  mean			=> 0,
  sum			=> 0,
  variance		=> undef,
  pseudo_variance	=> 0,
  min			=> undef,
  max			=> undef,
  mindex		=> undef,
  maxdex		=> undef,
  standard_deviation	=> undef,
  sample_range		=> undef,
  );

require 5.002;
$VERSION = '2.1';

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;
  my $self = {
    %fields,
    _permitted => \%fields,
  };
  bless ($self, $class);
  return $self;
}

sub add_data {
  my $self = shift;  ##Myself
  my $oldmean;
  my ($min,$mindex,$max,$maxdex);

  ##Take care of appending to an existing data set
  $min    = $self->{min}    || $_[0];
  $max    = $self->{max}    || $_[0];
  $maxdex = $self->{maxdex} || 0;
  $mindex = $self->{mindex} || 0;

  ##Calculate new mean, pseudo-variance, min and max;
  foreach (@_) {
    $oldmean = $self->{mean};
    $self->{sum} += $_;
    $self->{count}++;
    if ($_ >= $max) {
      $max = $_;
      $maxdex = $self->{count}-1;
    }
    if ($_ <= $min) {
      $min = $_;
      $mindex = $self->{count}-1;
    }
    $self->{mean} += ($_ - $oldmean) / $self->{count};
    $self->{pseudo_variance} += ($_ - $oldmean) * ($_ - $self->{mean});
  }

  $self->{min}          = $min;
  $self->{mindex}       = $mindex;
  $self->{max}          = $max;
  $self->{maxdex}       = $maxdex;
  $self->{sample_range} = $self->{max} - $self->{min};
  if ($self->{count} > 1) {
    $self->{variance}     = $self->{pseudo_variance} / ($self->{count} -1);
    $self->{standard_deviation}  = sqrt( $self->{variance});
  }
  return 1;
}

sub AUTOLOAD {
  my $self = shift;
  my $type = ref($self)
    or croak "$self is not an object";
  my $name = $AUTOLOAD;
  $name =~ s/.*://;     ##Strip fully qualified-package portion
  unless (exists $self->{'_permitted'}->{$name} ) {
    croak "Can't access `$name' field in class $type";
  }
  ##Read only method 
  return $self->{$name};
}

1;

package Statistics::Descriptive::Full;

use Carp;
use strict;
use vars qw(@ISA $a $b %fields);

@ISA = qw(Statistics::Descriptive::Sparse);

##Create a list of fields not to remove when data is updated
%fields = (
  _permitted => undef,  ##Place holder for the inherited key hash
  data       => undef,  ##Our data
  presorted  => undef,  ##Flag to indicate the data is already sorted
  _reserved  => undef,  ##Place holder for this lookup hash
);

##Have to override the base method to add the data to the object
##The proxy method from above is still valid
sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;
  my $self = $class->SUPER::new();  ##Create my self re SUPER
  $self->{data} = [];   ##Empty array ref for holding data later!
  $self->{'_reserved'} = \%fields;
  $self->{presorted} = 0;
  bless ($self, $class);  #Re-anneal the object
  return $self;
}

sub add_data {
  my $self = shift;
  my $key;
  $self->SUPER::add_data(@_);  ##Perform base statistics on the data
  push @{ $self->{data} }, @_;
  ##Clear the presorted flag
  $self->{'presorted'} = 0;
  ##Need to delete all cached keys
  foreach $key (keys %{ $self }) { # Check each key in the object
    # If it's a reserved key for this class, keep it
    next if exists $self->{'_reserved'}->{$key};
    # If it comes from the base class, keep it
    next if exists $self->{'_permitted'}->{$key};
    delete $self->{$key};          # Delete the out of date cached key
  }
  return 1;
}

sub get_data {
  my $self = shift;
  return @{ $self->{data} };
}

sub sort_data {
  my $self = shift;
  ##Sort the data in descending order
  $self->{data} = [ sort {$a <=> $b} @{$self->{data}} ];
  $self->presorted(1);
  ##Fix the maxima and minima indices
  $self->{mindex} = 0;
  $self->{maxdex} = $#{$self->{data}};
  return 1;
}

sub presorted {
  my $self = shift;
  if ($@) {  ##Assign
    $self->{'presorted'} = shift;
    return 1;
  }
  else {  ##Inquire
    return $self->{'presorted'};
  }
}

sub median {
  my $self = shift;

  ##Cached?
  return $self->{median} if defined $self->{median};

  $self->sort_data() unless $self->{'presorted'};
  my $count = $self->{count};
  if ($count % 2) {   ##Even or odd
    return $self->{median} = @{ $self->{data} }[($count-1)/2];
  }
  else {
    return $self->{median} =
	   (@{$self->{data}}[($count)/2] + @{$self->{data}}[($count-2)/2] ) / 2;
  }
}

sub trimmed_mean {
  my $self = shift;
  my $lower = shift; #Grab lower and upper bounds
  my $upper = shift || $lower; #upper bound is in arg list or is same as lower

  ##Cache
  my $thistm = join ':','tm','$lower','$upper';
  return $self->{$thistm} if defined $self->{$thistm};

  my $lower_trim = int ($self->{count}*$lower); 
  my $upper_trim = int ($self->{count}*$upper); 
  my ($val,$oldmean) = (0,0);
  my ($tm_count,$tm_mean,$index) = (0,0,$lower_trim);

  $self->sort_data() unless $self->{'presorted'};
  while ($index <= $self->{count} - $upper_trim -1) {
    $val = @{ $self->{data} }[$index];
    $oldmean = $tm_mean;
    $index++;
    $tm_count++;
    $tm_mean += ($val - $oldmean) / $tm_count;
  }
  return $self->{$thistm} = $tm_mean;
}

sub harmonic_mean {
  my $self = shift;
  return $self->{harmonic_mean} if defined $self->{harmonic_mean};
  my $hs = 0;
  for (@{ $self->{data} }) {
    return $self->{harmonic_mean} = 0 unless $_; #If any data is zero, return zero? 
    $hs += 1/$_;
  }
  return $self->{harmonic_mean} = $self->{count}/$hs;
}

sub mode {
  my $self = shift;
  return $self->{mode} if defined $self->{mode};
  my ($md,$occurances,$flag) = (0,0,1);
  my %count;
  foreach (@{ $self->{data} }) {
    $count{$_}++;
    $flag = 0 if ($count{$_} > 1);
  }
  #Distribution is flat, no mode exists
  if ($flag) {
    return undef;
  }
  foreach (keys %count) {
    if ($count{$_} > $occurances) {
      $occurances = $count{$_};
      $md = $_;
    }
  }
  return $self->{mode} = $md;
}

sub geometric_mean {
  my $self = shift;
  return $self->{geometric_mean} if defined $self->{geometric_mean};
  my $gm = 1;
  my $exponent = 1/$self->{count};
  for (@{ $self->{data} }) {
    return undef if $_ < 0;
    $gm *= $_**$exponent;
  }
  return $self->{geometric_mean} = $gm;
}

sub frequency_distribution {
  my $self = shift;
  my $element;
  return undef if $self->{count} < 2; #Must have at least two elements

  ##Cache
  return %{$self->{frequency}} if defined $self->{frequency};

  my %bins;
  my $partitions   = shift;
  my $interval = $self->{sample_range}/$partitions;
  my $iter = $self->{min};
  while (($iter += $interval) <  $self->{max}) {
    $bins{$iter} = 0;
  }
  $bins{$self->{max}} = 0;
  my @k = sort { $a <=> $b } keys %bins;
  ELEMENT:
  foreach $element (@{$self->{data}}) {
    for (@k) {
      if ($element <= $_) {
        $bins{$_}++;
        next ELEMENT;
      }
    }
  }
  return %{$self->{frequency}} = %bins;
}

sub least_squares_fit {
  my $self = shift;
  return undef if $self->{count} < 2;
  return $self->{least_squares_fit} if defined $self->{least_squares_fit};
  my @x;
  if (!defined $_[1]) {
    @x = 1..$self->{count};
  }
  else {
    @x = @_;
    if ( $self->{count} != scalar @x) {
      carp "Range and domain are of unequal length.";
      return undef;
    }
  }
  my @coefficients;
  my ($sigmaxy, $sigmax, $sigmaxx) = (0,0,0);
  my $sigmay = $self->sum;
  my $count = $self->{count};
  my $iter = 0;
  for (@x) {
    $sigmaxy += $_ * $self->{data}[$iter];
    $sigmax += $_;
    $sigmaxx += $_*$_;
    $iter++;
  }
  $coefficients[1] = ($count*$sigmaxy - $sigmax*$sigmay)/
		     ($count*$sigmaxx - $sigmax*$sigmax);
  $coefficients[0] = ($sigmay - $coefficients[1]*$sigmax)/$count;
  return @{ $self->{least_squares_fit} } = @coefficients;
}

1;

package Statistics::Descriptive;

1;

__END__

# Copyright (c) 1994,1995 Jason Kastner <jason@wagner.com>. All rights 
# reserved. This program is free software; you can redistribute it and/or 
# modify it under the same terms as Perl itself.

# Copyright (c) 1997 Colin Kuskie <colink@latticesemi.com>. All rights 
# reserved. This program is free software; you can redistribute it and/or 
# modify it under the same terms as Perl itself.

=head1 NAME

Statistics::Descriptive - Module of basic descriptive statistical functions.

=head1 SYNOPSIS

  use Statistics::Descriptive;
  $stat = Statistics::Descriptive::Full->new();
  $stat->add_data(1,2,3,4);
  $mean = $stat->mean();
  $var  = $stat->variance();
  $tm   = $stat->trimmed_mean(.25);

=head1 DESCRIPTION

This module provides basic functions used in descriptive statistics.
It has an object oriented design and supports two different types of
data storage and calculation objects: sparse and full. With the sparse
method, none of the data is stored and only a few statistical measures
are available. Using the full method, the entire data set is retained
and additional functions are available.

=head1 METHODS

=head2 Sparse Methods

=over 5

=item $stat = Statistics::Descriptive::Sparse->new();

Create a new sparse statistics object.

=item $stat->add_data(1,2,3);

Adds data to the statistics variable. The cached statistical values are 
updated automatically.

=item $stat->count();

Returns the number of data items.

=item $stat->mean();

Returns the mean of the data.

=item $stat->sum();

Returns the sum of the data.

=item $stat->variance();

Returns the variance of the data.  Division by n-1 is used.

=item $stat->standard_deviation();

Returns the standard deviation of the data. Division by n-1 is used.

=item $stat->min();

Returns the minimum value of the data set.

=item $stat->mindex();

Returns the index of the minimum value of the data set.

=item $stat->max();

Returns the maximum value of the data set.

=item $stat->maxdex();

Returns the index of the maximum value of the data set.

=item $stat->sample_range();

Returns the sample range (max - min) of the data set.

=back

=head2 Full Methods

=over 5

=item $stat = Statistics::Descriptive::Full->new();

Create a new statistics object that with
Statistics::Descriptive::Sparse as its base method so that it inherits
all the methods described above.

=item $stat->add_data(1,2,4,5);

Adds data to the statistics variable.  All of the sparse statistical
values are updated and cached.  Cached values from full methods are
deleted since they are no longer valid.  Note:  Calling add_data
with an empty array will delete all of your cached values!

=item $stat->get_data();

Returns a copy of the data array.

=item $stat->sort_data();

Sort the stored data and update the mindex and maxdex methods.

=item $stat->presorted(1);

If called with a non-zero argument, this method sets a flag that says
the data is already sorted and need not be sorted again.  Since some of
the methods in this class require sorted data, this saves some time.
If you supply sorted data to the object, call this method to prevent
the data from being sorted again. The flag is cleared whenever add_data
is called.  Calling the method without an argument returns the value of
the flag.

=item $stat->median();

Sorts the data and returns the median value of the data.

=item $stat->harmonic_mean();

Returns the harmonic mean of the data.

=item $stat->geometric_mean();

Returns the geometric mean of the data.

=item $stat->mode();

Returns the mode of the data. 

=item $stat->trimmed_mean(ltrim[,utrim]);

C<trimmed_mean(ltrim)> returns the mean with a fraction C<ltrim> 
of entries at each end dropped. C<trimmed_mean(ltrim,utrim)> 
returns the mean after a fraction C<ltrim> has been removed from the
lower end of the data and a fraction C<utrim> has been removed from the
upper end of the data.  This method sorts the data before beginning
to analyze it.

=item $stat->frequency_distribution();

C<frequency_distribution(partitions)> slices the data into C<partition>
sets and counts the number of items that fall into each partition. It
returns an associative array where the keys are the numerical values of the 
partitions used. The minimum value of the data set is not a key and
the maximum value of the data set is always a key. The number of
entries for a particular partition key are the number of items which are 
greater than the previous partition key and less then or equal to the current 
partition key. As an example, 

   $stat->add_data(1,1.5,2,2.5,3,3.5,4);
   %f = $stat->frequency_distribution(2);
   for (sort {$a <=> $b} keys %f) {
      print "key = $_, count = $f{$_}\n";
   }

prints

   key = 2.5, count = 4
   key = 4, count = 3

since there are four items less than or equal to 2.5, and 3 items
greater than 2.5 and less than 4.

=item $stat->least_squares_fit();

C<least_squares_fit()> performs a least squares fit on the data, assuming
a domain of 1,2,3... It returns an array of two elements; the value in the 
zeroth position is the constant (x^0) term and the value in the first 
position is the coeffiecient of the x^1 term. C<least_squares_fit(@x)> uses 
the values in C<@x> as the domain.

=back

=head1 REFERENCES

The Art of Computer Programming, Volume 2, Donald Knuth.

Handbook of Mathematica Functions, Milton Abramowitz and Irene Stegun.

Probability and Statistics for Engineering and the Sciences, Jay Devore.

=head1 COPYRIGHT

Copyright (c) 1997 Colin Kuskie <colink@latticesemi.com>. All rights reserved. 
This program is free software; you can redistribute it and/or modify it 
under the same terms as Perl itself.

Copyright (c) 1994,1995 Jason Kastner <jason@wagner.com>. All rights reserved. 
This program is free software; you can redistribute it and/or modify it 
under the same terms as Perl itself.

=head1 REVISION HISTORY

=item v2.0

August 1997 - Fixed errors in removing cached values (they weren't being
              removed!) and added sort_data and presorted methods.

June 1997 - Rewrote OO interface, modified function distribution,
            added mindex, maxdex.

=item v1.1

April 1995 - Added LeastSquaresFit and FrequencyDistribution.

=item v1.0 

March 1995 - Released to comp.lang.perl and placed on archive sites.

=item v.20

December 1994 - Complete rewrite after extensive and invaluable e-mail 
correspondence with Anno Siegel.

=item v.10

December 1994 - Initital concept, released to perl5-porters list.

=cut
