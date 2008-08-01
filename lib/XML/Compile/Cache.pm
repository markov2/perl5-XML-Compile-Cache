use warnings;
use strict;

package XML::Compile::Cache;
use base 'XML::Compile::Schema';

use Log::Report 'xml-compile-cache', syntax => 'SHORT';

use XML::Compile::Util   qw/pack_type unpack_type/;
use List::Util           qw/first/;

=chapter NAME
XML::Compile::Cache - Cache compiled XML translators

=chapter SYNOPSIS

 my $cache = XML::Compile::Cache->new(...);

 $cache->declare('READER',  $type,  @options);
 $cache->declare(RW     => \@types, @options);
 $cache->declare(WRITER =>  $type, \@options);

 $cache->compileAll;
 $cache->compileAll('RW');

 # get the cached code ref for the reader
 my $reader = $cache->reader($type);
 use Data::Dumper;
 print Dumper $reader->($xml);

 # get the cached code ref for the writer, and use it
 my $xml = $cache->writer($type)->($doc, $perl);
 print $xml->toString(1);

 # use the base-class uncached, the XML::Compile::Schema
 my $do = $cache->compile(READER => $type, @opts);

=chapter DESCRIPTIONS
With M<XML::Compile::Schema::compile()> (from the SUPER class of this one)
you can construct translators from XML to Perl and back.  These
translators are code references, which are "expensive" to create, but
"cheap" in use; call them as often as you want.

When the schemas grow larger, it gets harder to see which code reference
have already be created and which not.  And, these code references need
compile options which you do not want to distribute over your whole
program.  Finally, in a daemon application, you do not want to create
the translators when used (which can be in every client again), but once
during the initiation of the daemon.

=chapter METHODS

=section Constructors

=c_method new OPTIONS

=option  prefixes HASH|ARRAY-of-PAIRS
=default prefixes <smart>
Define prefix name to name-space mappings.  Passed to M<compile(prefixes)>
for each reader and writer, but also used to permit M<findName()> to
accept types which use a prefix.

Specify an ARRAY of (prefix, name-space) pairs, or a HASH which maps
name-spaces to prefixes (HASH order is reversed from ARRAY order!)  When
you wish to collect the results, like usage counts, of the translation
processing, you will need to specify a HASH.

 prefixes => [ mine => $myns, your => $yourns ]
 prefixes => { $myns => 'mine', $yourns => 'your' }

 # the previous is short for:
 prefixes => { $myns => [ uri => $myns, prefix => 'mine', used => 0 ]
             , $yourns => [ uri => $yourns, prefix => 'your', ...] }

=option  opts_rw HASH|ARRAY-of-PAIRS
=default opts_rw []
Options added to both READERs and WRITERS.  Options which are passed
with M<declare()> and C<opts_readers> or C<opts_writers> will overrule
these.

=option  opts_readers HASH|ARRAY-of-PAIRS
=default opts_readers []

=option  opts_writers HASH|ARRAY-of-PAIRS
=default opts_writers []

=option  allow_undeclared BOOLEAN
=default allow_undeclared <false>
When true, you may call the reader or writer with types which were
not registered with M<declare()>.  In that case, the reader or
writer may also get options passed for the compiler, as long as
they are consistent over each use of the type.

=option  any_element CODE|'TAKE_ALL'|'SKIP_ALL'|'ATTEMPT'
=default any_element 'SKIP_ALL'
ATTEMPT will convert all any elements, applying the reader for
each element found.

=option  any_attribute CODE|'TAKE_ALL'|'SKIP_ALL'|'ATTEMPT'
=default any_attribute 'SKIP_ALL'
=cut

sub init($)
{   my ($self, $args) = @_;
    $self->SUPER::init($args);

    $self->{XCC_opts}   = delete $args->{opts_rw}      || [];
    $self->{XCC_ropts}  = delete $args->{opts_readers} || [];
    $self->{XCC_wopts}  = delete $args->{opts_writers} || [];
    $self->{XCC_undecl} = delete $args->{allow_undeclared} || 0;

    $self->{XCC_rcode}  = {};
    $self->{XCC_wcode}  = {};
    $self->{XCC_dropts} = {};
    $self->{XCC_dwopts} = {};
    $self->{XCC_uropts} = {};
    $self->{XCC_uwopts} = {};

    $self->{XCC_readers} = {};
    $self->{XCC_writers} = {};

    $self->{XCC_prefix}  = $self->_namespaceTable(delete $args->{prefixes});

    if(my $anyelem = $args->{any_element})
    {   if($anyelem eq 'ATTEMPT')
        {   push @{$self->{XCC_ropts}}
              , any_element => sub {$self->_convertAnyElementReader(@_)};
        }
    }

    $self;
}

#----------------------

=section Accessors

=method prefixes [PAIRS]
Returns the HASH with prefix to name-space translations.  You should not
modify the returned HASH, but can provide PAIRS of additional prefix to
namespace relations.
=cut

sub prefixes(@)
{   my $self = shift;
    my $p    = $self->{XCC_prefix};
    while(@_)
    {   my ($prefix, $ns) = (shift, shift);
        $p->{$ns} = { uri => $ns, prefix => $prefix, used => 0 };
    }
    $p;
}

=method allowUndeclared [BOOLEAN]
Whether it is permitted to create readers and writers which are not
declared cleanly.
=cut

sub allowUndeclared(;$)
{   my $self = shift;
    @_ ? ($self->{XCC_undecl} = shift) : $self->{XCC_undecl};
}

#----------------------

=section Compilers

=method compileAll ['READER'|'WRITER'|'RW', [NAMESPACE]]
Compile all the declared readers and writers (default 'RW').  You may
also select to pre-compile only the READERs or only the WRITERs.  The
selection can be limited further by specifying a namespace.

By default, the processors are only compiled when used.  This method is
especially useful in a daemon process, where preparations can take as
much time as they want to... and running should be as fast as possible.
=cut

sub compileAll(;$$)
{   my ($self, $need, $usens) = @_;
    my ($need_r, $need_w) = $self->_need($need);

    if($need_r)
    {   while(my($type, $opts) = each %{$self->{XCC_dropts}})
        {   if(defined $usens)
            {   my ($myns, $local) = unpack_type $type;
                next if $usens eq $myns;
            }
            $self->{XCC_rcode}{$type} ||= $self->_createReader($type, @$opts);
        }
    }

    if($need_w)
    {   while(my($type, $opts) = each %{$self->{XCC_dwopts}})
        {   if(defined $usens)
            {   my ($myns, $local) = unpack_type $type;
                next if $usens eq $myns;
            }
            $self->{XCC_wcode}{$type} ||= $self->_createWriter($type, @$opts);
        }
    }
}

=method reader TYPE|NAME, OPTIONS
Returns the reader for the TYPE, which may be specified as prefixed NAME
(see M<findName()>).  OPTIONS are only permitted if M<new(allow_undeclared)>
is true, and the same as the previous call to this method.

The reader will be compiled the first time that it is used, and that
same CODE reference will be returned each next request for the same
type.

=examples
  my $schema = XML::Compile::Cache->new(\@xsd,
     prefixes => [ gml => $GML_NAMESPACE ] );
  my $data   = $schema->reader('gml:members')->($xml);

  my $getmem = $schema->reader('gml:members');
  my $data   = $getmem->($xml);
=cut

sub reader($@)
{   my ($self, $name) = (shift, shift);
    my $type    = $self->findName($name);
    my $readers = $self->{XCC_readers};

    if(exists $self->{XCC_dropts}{$type})
    {   warn __x"ignoring options to pre-declared reader {name}"
          , name => $name if @_;

        return $readers->{$type}
            if $readers->{$type};
    }
    elsif($self->{XCC_undecl})
    {   if(my $ur = $self->{XCC_uropts}{$type})
        {   my $differs = @$ur != @_;
            unless($differs)
            {   for(my $i=0; $i<@$ur; $i++)
                {   $differs++ if $ur->[$i] ne $_[$i];
                }
            }

            error __x"undeclared reader {name} used with different options: before ({was}), now ({this})"
              , name => $name, was => $ur, this => \@_
                  if $differs;
        }
        else
        {   $self->{XCC_uropts}{$type} = \@_;
        }
    }
    elsif(exists $self->{XCC_dwopts}{$type})
    {   error __x"type {name} is only declared as writer", name => $name;
    }
    else
    {   error __x"type {name} is not declared", name => $name;
    }

    $readers->{$type} ||= $self->_createReader($type, @_);
}

sub _createReader($@)
{   my ($self, $type) = (shift, shift);
    trace "create reader for $type";

    my @opts = $self->_merge_opts
     ( {prefixes => $self->prefixes}
     , $self->{XCC_opts}, $self->{XCC_ropts}
     , \@_
     );

    $self->compile(READER => $type, @opts);
}

=method writer TYPE|NAME
Returns the writer for the TYPE, which may be specified as prefixed NAME
(see M<findName()>).  OPTIONS are only permitted if M<new(allow_undeclared)>
is true, and the same as the previous call to this method.

The writer will be compiled the first time that it is used, and that
same CODE reference will be returned each next request for the same
type.

=examples
  my $xml = $cache->writer('gml:members')->($doc, $data);

  my $doc = XML::LibXML::Document->new('1.0', 'UTF-8');
  my $wr  = $cache->writer('gml:members');
  my $xml = $wr->($doc, $data);
  $doc->setDocumentElement($xml);
  print $doc->toString(1);
=cut

sub writer($)
{   my ($self, $name) = (shift, shift);
    my $type = $self->findName($name);
    my $writers = $self->{XCC_writers};

    if(exists $self->{XCC_dwopts}{$type})
    {   warn __x"ignoring options to pre-declared writer {name}"
          , name => $name if @_;

        return $writers->{$type}
            if $writers->{$type};
    }
    elsif($self->{XCC_undecl})
    {   if(my $ur = $self->{XCC_uwopts}{$type})
        {   my $differs = @$ur != @_;
            unless($differs)
            {   for(my $i=0; $i<@$ur; $i++)
                {   $differs++ if $ur->[$i] ne $_[$i];
                }
            }

            error __x"undeclared writer {name} used with different options: before ({was}), now ({this})"
              , name => $name, was => $ur, this => \@_
                  if $differs;
        }
        else
        {   $self->{XCC_uwopts}{$type} = \@_;
        }
    }
    elsif(exists $self->{XCC_dropts}{$type})
    {   error __x"type {name} is only declared as reader", name => $name;
    }
    else
    {   error __x"type {name} is not declared", name => $name;
    }

    $writers->{$type} ||= $self->_createWriter($type, @_);
}

sub _createWriter($)
{   my ($self, $type) = @_;
    
    trace "create writer for $type";
    my @opts = $self->_merge_opts
      ( {prefixes => $self->prefixes}
      , $self->{XCC_opts}, $self->{XCC_wopts}
      , \@_
      );
    $self->compile(WRITER => $type, @opts);
}

# Create a list with options, from a list of ARRAYs and HASHES.  The last
# ARRAY or HASH with a certain option value overwrites all previous values.

sub _merge_opts(@)
{   my $self = shift;
    map { !defined $_ ? () : ref $_ eq 'ARRAY' ? @$_ : %$_ } @_;
}

sub _need($)
{    $_[1] eq 'READER' ? (1,0)
   : $_[1] eq 'WRITER' ? (0,1)
   : $_[1] eq 'RW'     ? (1,1)
   : error __x"use READER, WRITER or RW, not {dir}", dir => $_[1];
}

#----------------------

=section Administration

=method declare 'READER'|'WRITER'|'RW', TYPE|ARRAY-of-TYPES, OPTIONS
Register that the indicated TYPE (or TYPES) may be used, and needs to
be translated with the OPTIONS (either specified as ARRAY or LIST).
Specify whether it may get used as READER, WRITER, or both (RW).  If the
READER and WRITER need different options, then you need to declare them
seperately; in that case you cannot use RW.

The TYPE should be understood by M<findName()>, so may be prefixed.

=example
  $cache->declare(READER => 'pref:count', sloppy_integers => 1)
        ->declare(RW     => '{myns}mylocal');

  $cache->declare(WRITER => [ 'xsd:int', '{http://}aap' ]);
=cut

sub declare($$@)
{   my ($self, $need, $names, @opts) = @_;
    my $opts = @opts==1 ? shift @opts : \@opts;
    $opts = [ %$opts ] if ref $opts eq 'HASH';

    my ($need_r, $need_w) = $self->_need($need);

    foreach my $name (ref $names eq 'ARRAY' ? @$names : $names)
    {   my $type = $self->findName($name);
        if($need_r)
        {   defined $self->{XCC_dropts}{$type}
               and warning __x"reader {name} declared again", name => $name;
            $self->{XCC_dropts}{$type} = $opts;
        }

        if($need_w)
        {   defined $self->{XCC_dwopts}{$type}
               and warning __x"writer {name} declared again", name => $name;
            $self->{XCC_dwopts}{$type} = $opts;
        }
    }

    $self;
}

=method findName NAME
Translate the NAME specification into a schema type.  The NAME
can be a full type (like '{namespace}localname', usually created
with M<XML::Compile::Util::pack_type()>) or a prefixed type (like
'myms:localname', defined with M<new(prefixes)> or M<prefixes()>).

=cut

sub findName($)
{   my ($self, $name) = @_;
defined $name or panic "findName";

    my ($type, $ns, $local);
    if($name =~ m/^\{/)
    {   $type = $name;
    }
    else
    {   (my $prefix,$local) = $name =~ m/^(\w*)\:(\S+)$/ ? ($1,$2) : ('',$name);
        my $p  = $self->{XCC_prefix};
        my $ns = first { $p->{$_}{prefix} eq $prefix } keys %$p;
        defined $ns
            or error __x"unknown name prefix for `{name}'", name => $name;

        $type  = pack_type $ns, $local;
    }

    $type;
}

=method printIndex [FILEHANDLE], OPTIONS

=option  show_declared BOOLEAN
=default show_declared <true>
Add an indicator to each line, about whether readers and writers are
declare for the type.  Declared readers and writers will show flags
C<r> and C<w> respectively.  Compiled readers and writers carry a
C<R> and/or C<W>.

=cut

sub printIndex(@)
{   my $self = shift;
    my $fh   = @_ % 2 ? shift : select;
    my %args = @_;
    my $decl = exists $args{show_declared} ? delete $args{show_declared} : 1;

    return $self->SUPER::printIndex($fh, %args)
        unless $decl;

    my $output = '';
    open my($out), '>', \$output;

    $self->SUPER::printIndex($out, %args);

    close $out;
    my @output = split /(?<=\n)/, $output;
    my $ns     = '';
    foreach (@output)
    {   $ns = $1 if m/^namespace\:\s+(\S+)/;
        my $local = m/^\s+(\S+)\s*$/ ? $1 : next;
        my $type  = pack_type $ns, $local;

        substr($_, 1, 1)
          = $self->{XCC_readers}{$type} ? 'R'
          : $self->{XCC_dropts}{$type}  ? 'r' : ' ';

        substr($_, 2, 1)
          = $self->{XCC_writers}{$type} ? 'W'
          : $self->{XCC_dwopts}{$type}  ? 'w' : ' ';
    }
    print $fh @output;
}

#---------------
# Convert ANY elements and attributes

sub _convertAnyElementReader(@)
{   my ($self, $type, $nodes, $path, $args) = @_;

    my $reader  = try { $self->reader($type) };
    !$@ && $reader or return ($type => $nodes);

    my @nodes   = ref $nodes eq 'ARRAY' ? @$nodes : $nodes;
    my @convert = map {$reader->($_)} @nodes;
    ($type => (ref $nodes eq 'ARRAY' ? $convert[0] : \@convert));
}


1;
