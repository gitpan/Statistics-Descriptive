#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 54;

use Benchmark;
use Statistics::Descriptive;

sub compare_hash_by_ranges
{
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    
    my $got_hash_ref = shift;
    my $expected = shift;
    my $blurb = shift;

    my $got = 
        [
            map { [$_, $got_hash_ref->{$_} ] }
            sort { $a <=> $b }
            keys(%$got_hash_ref)
        ]
        ;

    my $success = 1;

    if (scalar(@$expected) != scalar(@$got))
    {
        $success = 0;
        diag("Number of keys differ in hashes.");
    }
    else
    {
        COMPARE_KEYS:
        for my $idx (0 .. $#$got)
        {
            my ($got_key, $got_val) = @{$got->[$idx]};
            my ($expected_bottom, $expected_top, $expected_val)
                = @{$expected->[$idx]};
            
            if (! (    ($got_key >= $expected_bottom)
                    && ($got_key <= $expected_top)
                    && ($got_val == $expected_val)
                )
            )
            {
                $success = 0;
                diag(<<"EOF");
Key/Val pair No. $idx is out of range or wrong:
Got: [$got_key, $got_val]
Expected: [$expected_bottom, $expected_top, $expected_val]
EOF
                
                last COMPARE_KEYS;
            }
        }
    }

    ok($success, $blurb);
}

sub is_between
{
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    
    my ($have, $want_bottom, $want_top, $blurb) = @_;

    ok (
        (($have >= $want_bottom) &&
        ($want_top >= $have)),
        $blurb
    );
}


{
    # test #1
    my $stat = Statistics::Descriptive::Full->new();
    my @results = $stat->least_squares_fit();
    # TEST
    ok (!scalar(@results), "Least-squares results on a non-filled object are empty.");

    # test #2
    # data are y = 2*x - 1

    $stat->add_data( 1, 3, 5, 7 );
    @results = $stat->least_squares_fit();
    # TEST
    is_deeply (
        [@results[0..1]],
        [-1, 2],
        "least_squares_fit returns the correct result."
    );
}

{
    # test #3
    # test error condition on harmonic mean : one element zero
    my $stat = Statistics::Descriptive::Full->new();
    $stat->add_data( 1.1, 2.9, 4.9, 0.0 );
    my $single_result = $stat->harmonic_mean();
    # TEST
    ok (!defined($single_result),
        "harmonic_mean is undefined if there's a 0 datum."
    );
}

{
    # test #4
    # test error condition on harmonic mean : sum of elements zero
    my $stat = Statistics::Descriptive::Full->new();
    $stat->add_data( 1.0, -1.0 );
    my $single_result = $stat->harmonic_mean();
    # TEST
    ok (!defined($single_result),
        "harmonic_mean is undefined if the sum of the reciprocals is zero."
    );
}

{
    # test #5
    # test error condition on harmonic mean : sum of elements near zero
    my $stat = Statistics::Descriptive::Full->new();
    local $Statistics::Descriptive::Tolerance = 0.1;
    $stat->add_data( 1.01, -1.0 );
    my $single_result = $stat->harmonic_mean();
    # TEST
    ok (! defined( $single_result ),
        "test error condition on harmonic mean : sum of elements near zero"
    );
}

{
    # test #6
    # test normal function of harmonic mean
    my $stat = Statistics::Descriptive::Full->new();
    $stat->add_data( 1,2,3 );
    my $single_result = $stat->harmonic_mean();
    # TEST
    ok (scalar(abs( $single_result - 1.6363 ) < 0.001),
        "test normal function of harmonic mean",
    );
}

{
    # test #7
    # test stringification of hash keys in frequency distribution
    my $stat = Statistics::Descriptive::Full->new();
    $stat->add_data(0.1,
                    0.15,
                    0.16,
                   1/3);
    my %f = $stat->frequency_distribution(2);

    # TEST
    compare_hash_by_ranges(
        \%f,
        [[0.216666,0.216667,3],[0.3333,0.3334,1]],
        "Test stringification of hash keys in frequency distribution",
    );

    # test #8
    ##Test memorization of last frequency distribution
    my %g = $stat->frequency_distribution();
    # TEST
    is_deeply(
        \%f,
        \%g,
        "memorization of last frequency distribution"
    );
}

{
    # test #9
    # test the frequency distribution with specified bins
    my $stat = Statistics::Descriptive::Full->new();
    my @freq_bins=(20,40,60,80,100);
    $stat->add_data(23.92,
                    32.30,
                    15.27,
                    39.89,
                    8.96,
                    40.71,
                    16.20,
                    34.61,
                    27.98,
                    74.40);
    my %f = $stat->frequency_distribution(\@freq_bins);

    # TEST
    is_deeply(
        \%f,
        {
            20 => 3,
            40 => 5,
            60 => 1,
            80 => 1,
            100 => 0,
        },
        "Test the frequency distribution with specified bins"
    );
}

{
    # test #10 and #11
    # Test the percentile function and caching
    my $stat = Statistics::Descriptive::Full->new();
    $stat->add_data(-5,-2,4,7,7,18);
    ##Check algorithm
    # TEST
    is ($stat->percentile(50),
        4,
        "percentile function and caching - 1",
    );
    # TEST
    is ($stat->percentile(25),
        -2,
        "percentile function and caching - 2",
    );
}

{
    # tests #12 and #13
    # Check correct parsing of method parameters
    my $stat = Statistics::Descriptive::Full->new();
    $stat->add_data(1,2,3,4,5,6,7,8,9,10);
    # TEST
    is(
        $stat->trimmed_mean(0.1,0.1),
        $stat->trimmed_mean(0.1),
        "correct parsing of method parameters",
    );

    # TEST
    is ($stat->trimmed_mean(0.1,0),
        6,
        "correct parsing of method parameters - 2",
    );
}

{
    my $stat = Statistics::Descriptive::Full->new();

    $stat->add_data((0.001) x 6);

    # TEST
    is_between ($stat->variance(),
        0,
        0.00001,
        "Workaround to avoid rounding errors that yield negative variance."
    );

    # TEST
    is_between ($stat->standard_deviation(),
        0,
        0.00001,
        "Workaround to avoid rounding errors that yield negative std-dev."
    );
}

{
    my $stat = Statistics::Descriptive::Full->new();

    $stat->add_data(1, 2, 3, 5);

    # TEST
    is ($stat->count(),
        4,
        "There are 4 elements."
    );

    # TEST
    is ($stat->sum(),
        11,
        "The sum is 11",
    );

    # TEST
    is ($stat->sumsq(),
        39,
        "The sum of squares is 39"
    );

    # TEST
    is ($stat->min(),
        1,
        "The minimum is 1."
    );

    # TEST
    is ($stat->max(),
        5,
        "The maximum is 5."
    );
}

{
    # test #9
    # test the frequency distribution with specified bins
    my $stat = Statistics::Descriptive::Full->new();

    my @freq_bins=(20,40,60,80,100);

    $stat->add_data(23.92,
                    32.30,
                    15.27,
                    39.89,
                    8.96,
                    40.71,
                    16.20,
                    34.61,
                    27.98,
                    74.40);

    my $f_d = $stat->frequency_distribution_ref(\@freq_bins);

    # TEST
    is_deeply(
        $f_d,
        {
            20 => 3,
            40 => 5,
            60 => 1,
            80 => 1,
            100 => 0,
        },
        "Test the frequency distribution returned as a scalar reference"
    );
}

{
    # test #9
    # test the frequency distribution with specified bins
    my $stat = Statistics::Descriptive::Full->new();

    $stat->add_data(2, 4, 8);

    # TEST
    is_between(
        $stat->geometric_mean(),
        (4-1e-4),
        (4+1e-4),
        "Geometric Mean Test #1",
    )
}

{
    my $stat = Statistics::Descriptive::Full->new();
    my $expected;
    
    $stat->add_data(1 .. 9, 100);

    # TEST
    $expected = 3.11889574523909;
    is_between ($stat->skewness(),
        $expected - 1E-13,
        $expected + 1E-13,
        "Skewness of $expected +/- 1E-13"
    );

    # TEST
    $expected = 9.79924471616366;
    is_between ($stat->kurtosis(),
        $expected - 1E-13,
        $expected + 1E-13,
        "Kurtosis of $expected +/- 1E-13"
    );
    
    $stat->add_data(100 .. 110);
    
    #  now check that cached skew and kurt values are recalculated
    
    # TEST
    $expected = -0.306705104889384;
    is_between ($stat->skewness(),
        $expected - 1E-13,
        $expected + 1E-13,
        "Skewness of $expected +/- 1E-13"
    );

    # TEST
    $expected = -2.09839497356215;
    is_between ($stat->kurtosis(),
        $expected - 1E-13,
        $expected + 1E-13,
        "Kurtosis of $expected +/- 1E-13"
    );
}

{
    my $stat = Statistics::Descriptive::Full->new();

    $stat->add_data(1,2);
    my $def;

    # TEST
    $def = defined $stat->skewness() ? 1 : 0;
    is ($def,
        0,
        'Skewness is undef for 2 samples'
    );

    $stat->add_data (1);

    # TEST
    $def = defined $stat->kurtosis() ? 1 : 0;
    is ($def,
        0,
        'Kurtosis is undef for 3 samples'
    );

}

{
    # This is a fix for:
    # https://rt.cpan.org/Ticket/Display.html?id=72495
    # Thanks to Robert Messer
    my $stat = Statistics::Descriptive::Full->new();

    my $ret = $stat->percentile(100);

    # TEST
    ok (!defined($ret), 'Returns undef and does not die.');
}



#  test stats when no data have been added
{
    my $stat = Statistics::Descriptive::Full->new();
    my ($result, $str);

    #  An accessor method for _permitted would be handy,
    #  or one to get all the stats methods
    my @methods = qw {
        mean sum variance standard_deviation
        min mindex max maxdex sample_range
        skewness kurtosis median
        harmonic_mean geometric_mean
        mode least_squares_fit
        percentile frequency_distribution 
    };
    #  least_squares_fit is handled in an earlier test, so is actually a duplicate here
    
    #diag 'Results are undef when no data added';
    #  need to update next line when new methods are tested here
    # TEST:$method_count=18
    foreach my $method (sort @methods) {  
        $result = $stat->$method;
        # TEST*$method_count
        ok (!defined ($result), "$method is undef when object has no data.");
    }

    #  quantile and trimmed_mean require valid args, so don't test in the method loop
    my $method = 'quantile';
    $result = $stat->$method(1);
    # TEST
    ok (!defined ($result), "$method is undef when object has no data.");
    
    $method = 'trimmed_mean';
    $result = $stat->$method(0.1);
    # TEST
    ok (!defined ($result), "$method is undef when object has no data.");    
}

#  test SD when only one value added
{
    my $stat = Statistics::Descriptive::Full->new();
    $stat->add_data( 1 );

    my $result = $stat->standard_deviation();
    # TEST
    ok ($result == 0, "SD is zero when object has one record.");
}

# Test function returns undef in list context when no data added.
# The test itself is almost redundant.
# Fixes https://rt.cpan.org/Ticket/Display.html?id=74890
{
    my $stat = Statistics::Descriptive::Full->new();

    # TEST
    is_deeply(
        [ $stat->median(), ],
        [ undef() ],
        "->median() Returns undef in list-context.",
    );

    # TEST
    is_deeply(
        [ $stat->standard_deviation(), ],
        [ undef() ],
        "->standard_deviation() Returns undef in list-context.",
    );
}

{
    my $stats = Statistics::Descriptive::Full->new();

    $stats->add_data_with_samples([{1 => 10}, {2 => 20}, {3 => 30}, {4 => 40}, {5 => 50}]);

    # TEST
    is_deeply(
        $stats->_data(),
        [ 1, 2, 3, 4, 5 ],
        'add_data_with_samples: data set is correct',
    );

    # TEST
    is_deeply(
        $stats->_samples(),
        [ 10, 20, 30, 40, 50 ],
        'add_data_with_samples: samples are correct',
    );

}
