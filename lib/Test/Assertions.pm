package Test::Assertions;
use Carp;
use vars qw($test_no $VERSION @EXPORT %ignore %only $use_ok $test_mode $planned_tests);
use constant tests => 0;
@EXPORT = qw(DIED COMPILES EQUAL EQUALS_FILE MATCHES_FILE FILES_EQUAL ASSESS ASSESS_FILE INTERPRET tests READ_FILE WRITE_FILE);

# this is the number of the current test, for automatically
# numbering the output of ASSERT
$test_no = 0;
# this is a flag - true if we are imported in a testing mode
$test_mode = 0;

# a flag, so that alarm() is never called if it isn't present (e.g. on Windows)
use constant HAVE_ALARM => 1;
BEGIN
{
	eval
	{
		my $was = alarm 0;
		alarm $was;
	};
	undef *HAVE_ALARM, *HAVE_ALARM = sub () {0} if($@); #Change the constant!
}

$VERSION = sprintf"%d.%03d", q$Revision: 1.47 $ =~ /: (\d+)\.(\d+)/;

sub import
{
	my $pkg = shift;
	my $style = shift;
	my $callpkg = caller(0);
	foreach my $sym (@EXPORT) {
		*{"$callpkg\::$sym"} = \&{"$pkg\::$sym"};
	}

	#Select implementation of ASSERT
	if(!$style) {
		*{"$callpkg\::ASSERT"} = sub {};
	}
	elsif($style eq 'die') {
		*{"$callpkg\::ASSERT"} = \&{"$pkg\::ASSERT_die"};
	}
	elsif($style eq 'warn') {
		*{"$callpkg\::ASSERT"} = \&{"$pkg\::ASSERT_warn"};
	}
	elsif($style eq 'confess') {
		*{"$callpkg\::ASSERT"} = \&{"$pkg\::ASSERT_confess"};
	}
	elsif($style eq 'cluck') {
		*{"$callpkg\::ASSERT"} = \&{"$pkg\::ASSERT_cluck"};
	}
	elsif($style eq 'test' || $style eq 'test/ok') {
		$use_ok = $style eq 'test/ok';
		*{"$callpkg\::ASSERT"} = \&{"$pkg\::ASSERT_test"};
		*{"$callpkg\::ok"} = \&{"$pkg\::ASSERT_test"} if($style eq 'test/ok');
		*{"$callpkg\::plan"} = \&{"$pkg\::plan"};
		*{"$callpkg\::ignore"} = \&{"$pkg\::ignore"};
		*{"$callpkg\::only"} = \&{"$pkg\::only"};
		$test_mode = 1;
	}
	else {
		croak("Test::Assertions imported with unknown directive: $style");
	}
}

#For compatibility with Test::Simple
sub plan
{
	shift(); #tests
	my $number = shift();
	my ($pkg, $filename, $line, $sub) = caller(0);
	$number = _count_tests($filename)
		unless (defined($number) && $number =~ /^\d+$/);
	print "1..$number\n";
	$planned_tests = $number;
	return $number;
}

END
{
	# if we're in test mode and plan() has been called, ensure that the right number of tests have been run
	if ($test_mode && defined($planned_tests)) {
		if ($test_no != $planned_tests) {
			warn "# Looks like you planned $planned_tests tests but actually ran $test_no.\n";
		}
	}
}

#Test filtering
sub ignore
{
	%ignore = map {$_ => 1} @_;
}

sub only
{
	%only = map {$_ => 1} @_;
}


#
# Various styles
#

sub ASSERT_test ($;$)
{
	my ($test,$msg) = @_;
	my ($pkg, $filename, $line, $sub) = caller(0);
	$test_no++;
	if($ignore{$test_no} || %only && !$only{$test_no})
	{
		print "ok - skipped $test_no";
	}
	else
	{
		print ($test?"ok $test_no":"not ok $test_no at line $line in $filename");
	}
	print " ($msg)" if(defined $msg);
	print "\n";
}

sub ASSERT_die ($;$)
{
	my $test = shift;
	my $msg = shift;
	$msg="($msg)" if(defined $msg);
	my ($pkg, $filename, $line, $sub) = caller(0);
	die("Assertion failure at line $line in $filename $msg\n") unless($test);
}

sub ASSERT_warn ($;$)
{
	my $test = shift;
	my $msg = shift;
	$msg="($msg)" if(defined $msg);
	my ($pkg, $filename, $line, $sub) = caller(0);
	warn("Assertion failure at line $line in $filename $msg\n") unless($test);
}

sub ASSERT_confess ($;$)
{
	my $test = shift;
	my $msg = shift;
	$msg="($msg)" if(defined $msg);
	my ($pkg, $filename, $line, $sub);
	if (caller(1)) {
		($pkg, $filename, $line, $sub) = caller(1);
	} else {
		($pkg, $filename, $line, $sub) = caller(0);
	}
	confess("Assertion failure at line $line in $filename $msg\n") unless($test);
}

sub ASSERT_cluck ($;$)
{
	my $test = shift;
	my $msg = shift;
	$msg="($msg)" if(defined $msg);
	my ($pkg, $filename, $line, $sub);
	if (caller(1)) {
		($pkg, $filename, $line, $sub) = caller(1);
	} else {
		($pkg, $filename, $line, $sub) = caller(0);
	}
	Carp::cluck("Assertion failure at line $line in $filename $msg\n") unless($test);
}

sub DIED
{
	my ($coderef) = @_;
	eval {&$coderef};
	my $error = $@;
	TRACE("DIED: " . $error);
	return $error;
}

sub COMPILES
{
	my ($file, $strict, $strref) = @_;

	my @args = ($^X);
	push @args, '-Mstrict', '-w' if $strict;
	push @args, '-c', $file;
	my $output;
	my $ok = 0;
	if ($strref && ref($strref) eq 'SCALAR') {
		require IO::CaptureOutput;
		($output, $$strref) = IO::CaptureOutput::capture_exec(@args);
		$ok = ($$strref =~ /syntax OK/);
	} else {
		my $command = join ' ', @args;
		$output = `$command 2>&1`;
		$output =~ s/\n$//;
		$ok = ($output =~ /syntax OK/);
	}
	return wantarray? ($ok, $output) : $ok;
}

sub EQUAL
{
	require Test::More;
	my ($lhs, $rhs) = @_;
	return Test::More::eq_array([$lhs],[$rhs]);
}

sub FILES_EQUAL
{
	require File::Compare;
	my ($lhs, $rhs) = @_;
	return File::Compare::compare($lhs,$rhs)==0;
}

sub EQUALS_FILE
{
	my ($lhs, $rhs) = @_;
	return($lhs eq _read_file($rhs));
}

sub MATCHES_FILE
{
	my ($lhs, $rhs) = @_;
	my $regex = _read_file($rhs);
	return($lhs =~ /^$regex$/);
}

#
# Collation of tests
#

sub ASSESS_FILE
{
	my ($file, $verbose, $timeout) = @_;
	$timeout = 60 unless(defined $timeout);
	my @tests;
	local *FH;
	eval
	{
		alarm $timeout if HAVE_ALARM;
		open (*FH, "$file |") or die("unable to execute $file");
		@tests = <FH>;
		close FH;
	};
	alarm 0 if HAVE_ALARM;
	my $rs;
	if($@) {
		$rs = "not ok for $file ($@)\n"
	} elsif ($?) {
		$rs = "not ok for $file (exit code = $?)\n";
	} else {
		$rs = ASSESS(\@tests, $file, $verbose);
	}
	return wantarray? INTERPRET($rs) : $rs;
}

sub ASSESS
{
	my ($tests, $name, $verbose) = @_;
	my $errors = 0;
	my $total = 0;
	foreach(@$tests)
	{
		if(/^not ok/)
		{
			$errors++; $total++;
			if($verbose)
			{
				s/\n?$/ in $name\n/;
				print;
			}
		}
		elsif(/^ok/)
		{
			$total++;
		}
	}

	my $rs;
	if(defined $name) { $name = " for $name"; } else { $name = ''; }
	if($errors)
	{
		$rs = "not ok$name ($errors errors in $total tests)\n";
	}
	else
	{
		$rs = "ok$name".($verbose?" passed all $total tests ":"")."\n";
	}
	return wantarray? INTERPRET($rs) : $rs;
}

sub INTERPRET
{
	my $rs = shift;
	my ($status, $desc) = ($rs =~ /^((?:not )?ok)(.*)$/);
	$desc =~ s/^\s+//;
	$desc =~ s/^for //; #ok for x => x
	$desc =~ s/^- //; #ok - x => x
	$desc =~ s/^\((.*)\)/$1/; #ok (x) => x
	return ($status eq 'ok' || 0, $desc);
}

sub READ_FILE
{
	my $filename = shift;
	my $contents;
	eval {
		$contents = _read_file($filename);
	};
	return $contents;
}

sub WRITE_FILE
{
	my ($filename, $contents) = @_;
	my $success;
	eval {
		_write_file($filename, $contents);
		$success = 1;
	};
	return $success;
}

#
# Misc subroutines
#

sub _count_tests
{
	my $filename = shift;
	my $count = 0;
	local *LI;
	open (LI, $filename) || die("Unable to open $filename to count tests - $!");
	while(<LI>)
	{
		$count++ if(/\bASSERT[\s\(]/);
		$count++ if($use_ok && /\bok[\s\(]/);
	}
	close LI;
	return $count;
}

sub _read_file
{
	my $filename = shift;
	open (FH, $filename) || die("unable to open $filename - $!");
	local $/ = undef;
	my $data = <FH>;
	close FH;
	return $data;
}

sub _write_file
{
	my ($filename, $data) = @_;
	local *FH;
	open(FH, ">$filename") or die("Unable to open $filename - $!");
	binmode FH;
	print FH $data;
	close FH;
}

#Standard debugging stub - intended to be overridden when
#debugging is needed, e.g. by Log::Trace
sub TRACE {}

1;

=head1 NAME

Test::Assertions - a simple set of building blocks for both unit and runtime testing

=head1 SYNOPSIS

	#ASSERT does nothing
	use Test::Assertions;
	
	#ASSERT warns "Assertion failure"...
	use Test::Assertions qw(warn);
	
	#ASSERT dies with "Assertion failure"...
	use Test::Assertions qw(die);
	
	#ASSERT warns "Assertion failure"... with stack trace
	use Test::Assertions qw(cluck);
	
	#ASSERT dies with "Assertion failure"... with stack trace
	use Test::Assertions qw(confess);
	
	#ASSERT prints ok/not ok
	use Test::Assertions qw(test);
	
	#Will cause an assertion failure
	ASSERT(1 == 0);
	
	#Optional message
	ASSERT(0 == 1, "daft");
	
	#Checks if coderef dies
	ASSERT(
		DIED( sub {die()} )
	);
	
	#Check if perl compiles OK
	ASSERT(
		COMPILES('program.pl')
	);
	
	#Deep comparisons
	ASSERT(
		EQUAL(\@a, \@b),
		"lists of widgets match"	# an optional message
	);
	ASSERT(
		EQUAL(\%a, \%b)
	);
	
	#Compare to a canned value
	ASSERT(
		EQUALS_FILE($foo, 'bar.dat'),
		"value matched stored value"
	);
	
	#Compare to a canned value (regex match using file contents as regex)
	ASSERT(
		MATCHES_FILE($foo, 'bar.regex')
	);
	
	#Compare file contents
	ASSERT(
		FILES_EQUAL('foo.dat', 'bar.dat')
	);
	
	#returns 'not ok for Foo::Bar Tests (1 errors in 3 tests)'
	ASSESS(
		 ['ok 1', 'not ok 2', 'A comment', 'ok 3'], 'Foo::Bar Tests', 0
	);
	
	#Collate results from another test script
	ASSESS_FILE("test.pl");
	
	#File routines
	$success = WRITE_FILE('bar.dat', 'hello world');
	ASSERT( WRITE_FILE('bar.dat', 'hello world'), 'file was written');
	$string = READ_FILE('example.out');
	ASSERT( READ_FILE('example.out'), 'file has content' );

The helper routines don't need to be used inside ASSERT():

	if ( EQUALS_FILE($string, $filename) ) {
		print "File hasn't changed - skipping\n";
	} else {
		my $rc = run_complex_process($string);
		print "File changed - string was reprocessed with result '$rc'\n";
	}
	
	($boolean, $output) = COMPILES('file.pl');
	# or...
	my $string;
	($boolean, $standard_output) = COMPILES('file.pl', 1, \$string);
	# $string now contains standard error, separate from $standard_output

In test mode:

	use Test::Assertions qw(test);
	plan tests => 4;
	plan tests;					#will attempt to deduce the number
	only (1,2);					#Only report ok/not ok for these tests
	ignore 2;					#Skip this test

	#In test/ok mode...
	use Test::Assertions qw(test/ok);
	ok(1);						#synonym for ASSERT

=head1 DESCRIPTION

Test::Assertions provides a convenient set of tools for constructing tests, such as unit tests or run-time assertion
checks (like C's ASSERT macro).
Unlike some of the Test:: modules available on CPAN, Test::Assertions is not limited to unit test scripts;
for example it can be used to check output is as expected within a benchmarking script.
When it is used for unit tests, it generates output in the standard form for CPAN unit testing (under Test::Harness).

The package's import method is used to control the behaviour of ASSERT: whether it dies,
warns, prints 'ok'/'not ok', or does nothing.

In 'test' mode the script also exports plan(), only() and ignore() functions.
In 'test/ok' mode an ok() function is also exported for compatibility with Test/Test::Harness.
The plan function attempts to count the number of tests if it isn't told a number (this works fine in simple
test scripts but not in loops/subroutines). In either mode, a warning will be emitted if the planned number
of tests is not the same as the number of tests actually run, e.g.

	# Looks like you planned 2 tests but actually ran 1.

=head2 METHODS

=over 4

=item plan $number_of_tests

Specify the number of tests to expect. If $number_of_tests isn't supplied, ASSERTION tries to deduce the number
itself by parsing the calling script and counting the number of calls to ASSERT.
It also returns the number of tests, should you wish to make use of that figure at some point.
In 'test' and 'test/ok' mode a warning will be emitted if the actual number of tests does not match the number planned,
similar to Test::More.

=item only(@test_numbers)

Only display the results of these tests

=item ignore(@test_numbers)

Don't display the results of these tests

=item ASSERT($bool, $comment)

The workhorse function.  Behaviour depends on how the module was imported.
$comment is optional.

=item ASSESS(@result_strings)

Collate the results from a set of tests.
In a scalar context returns a result string starting with "ok" or "not ok"; in a list context returns 1=pass or 0=fail, followed by a description.

 ($bool, $desc) = ASSESS(@args)
 
is equivalent to

 ($bool, $desc) = INTERPRET(scalar ASSESS(@args))

=item ASSESS_FILE($file, $verbose, $timeout)

 $verbose is an optional boolean
 default timeout is 60 seconds (0=never timeout)

In a scalar context returns a result string; in a list context returns 1=pass or 0=fail, followed by a description.
The timeout uses alarm(), but has no effect on platforms which do not implement alarm().

=item ($bool, $desc) = INTERPRET($result_string)

Inteprets a result string.  $bool indicates 1=pass/0=fail; $desc is an optional description.

=item $bool = EQUAL($item1, $item2)

Deep comparison of 2 data structures (i.e. references to some kind of structure) or scalars.

=item $bool = EQUALS_FILE($string, $filename)

Compares a string with a canned value in a file.

=item $bool = MATCHES_FILE($string, $regexfilename)

Compares a value with a regex that is read from a file. The regex has the '^' anchor prepended and the '$' anchor appended,
after being read in from the file.
Handy if you have random numbers or dates in your output.

=item $bool = FILES_EQUAL($filename1, $filename2)

Test if 2 files' contents are identical

=item $bool = DIED($coderef)

Test if the coderef died

=item COMPILES($filename, $strict, $scalar_reference)

Test if the perl code in $filename compiles OK, like perl -c.
If $strict is true, tests with the options -Mstrict -w.

In scalar context it returns 1 if the code compiled, 0 otherwise. In list context it returns the same boolean,
followed by the output (that is, standard output and standard error B<combined>) of the syntax check.

If $scalar_reference is supplied and is a scalar reference then the standard output and standard
error of the syntax check subprocess will be captured B<separately>. Standard error
will be put into this scalar - IO::CaptureOutput is loaded on demand to do this -
and standard output will be returned as described above.

=item $contents = READ_FILE($filename)

Reads the specified file and returns the contents.
Returns undef if file cannot be read.

=item $success = WRITE_FILE($filename, $contents)

Writes the given contents to the specified file.
Returns undef if file cannot be written.

=back

=head1 DEPENDENCIES

 Carp
 Test::More (loaded on demand)
 File::Compare (loaded on demand)
 IO::CaptureOutput (loaded on demand)

=head1 RELATED MODULES

=over 4

=item *

Test and Test::Simple

=item *

Test::More

=item *

Carp::Assert

=back

=head1 SEE ALSO

L<Test::Assertions::Manual> - A guide to using Test::Assertions

=head1 VERSION

$Revision: 1.47 $ on $Date: 2005/01/12 13:58:13 $ by $Author: johna $

=head1 AUTHOR

John Alden with additions from Piers Kent and Simon Flack 
<cpan _at_ bbc _dot_ co _dot_ uk>

=head1 COPYRIGHT

(c) BBC 2005. This program is free software; you can redistribute it and/or modify it under the GNU GPL.

See the file COPYING in this distribution, or http://www.gnu.org/licenses/gpl.txt 

=cut
