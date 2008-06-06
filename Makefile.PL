use ExtUtils::MakeMaker;

use 5.008;

WriteMakefile
 ( NAME	 	=> 'XML::Compile::Cache'
 , VERSION	=> '0.10'
 , PREREQ_PM    =>
   { XML::Compile              => 0.84
   , XML::Compile::Tester      => 0.02
   , Log::Report               => 0.19

   , Test::More                => 0.54
   , Test::Pod                 => '1.00'
   }

 , EXE_FILES    => []
 , AUTHOR       => 'Mark Overmeer'
 , ABSTRACT     => 'Cache compiled XML translators'
 , LICENSE      => 'perl'
 );

### used by oodist during production of distribution
sub MY::postamble { <<'__POSTAMBLE' }

# for DIST
RAWDIR          = ../public_html/xml-compile-cache/raw
DISTDIR         = ../public_html/xml-compile-cache/source
LICENSE         = artistic
SKIP_LINKS	= XML::LibXML

# for POD
FIRST_YEAR      = 2008
EMAIL           = perl@overmeer.net
WEBSITE         = http://perl.overmeer.net/xml-compile/
PODTAIL		= ../XML-shared-podtail
__POSTAMBLE
