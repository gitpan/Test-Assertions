use Test::More;
eval "use Test::Pod::Coverage 1.00";
if ( $@ ) {
    plan skip_all => "Test::Pod::Coverage 1.00 required for testing POD Coverage";
}
else {
    plan tests => 1;
}
for my $module ( Test::Pod::Coverage::all_modules() ) {
    my @private = (qr/^[A-Z_]+$/);
    @private = (qr/^tests$/, qr/^ASSERT_/, qr/^TRACE|HAVE_ALARM$/) if($module eq 'Test::Assertions');
    pod_coverage_ok($module, { also_private => \@private }); #Ignore all caps
}
