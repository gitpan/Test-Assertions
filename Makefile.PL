use ExtUtils::MakeMaker;

WriteMakefile(
              NAME          => 'Test::Assertions',
              VERSION_FROM  => 'lib/Test/Assertions.pm',
              PREREQ_PM => {
                            Carp => 0,
                           },
              ABSTRACT_FROM => 'lib/Test/Assertions.pm',
              AUTHOR        => 'British Broadcasting Corporation',
             );
