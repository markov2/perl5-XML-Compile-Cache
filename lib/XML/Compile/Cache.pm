use warnings;
use strict;

package XML::Compile::Cache;
use base 'XML::Compile::Schema';

use Log::Report 'xml-compile-cache', syntax => 'SHORT';

use XML::Compile::Util   qw/pack_type unpack_type/;
use List::Util           qw/first/;
use XML::LibXML::Simple  qw/XMLin/;

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
 my $reader = $cache->reader($type, @opts);
 use Data::Dumper;
 print Dumper $reader->($xml);

 # get the cached code ref for the writer, and use it
 my $doc = XML::LibXML::Document->new('1.0', 'UTF-8');
 my $xml = $cache->writer($type)->($doc, $perl);
 print $xml->toString(1);

 # use the base-class uncached, the XML::Compile::Schema
 my $do = $cache->compile(READER => $type, @opts);

=chapter DESCRIPTIONS
C<XML::Compile::Cache> is the smart brother of M<XML::Compile::Schema>;
it keeps track of your compiled readers and writers, and also helps
you administer the parameters to handle compilation.  Besides, it
lat you use easy prefixes instead of full namespaces.

With M<XML::Compile::Schema::compile()> (defined in the SUPER class of
this module) you can construct translators from XML to Perl and back.
These translators are code references, which are "expensive" to create,
but "cheap" in use; call them as often as you want.  This module helps
you administer them.

When the schemas grow larger, it gets harder to see which code reference
have already be created and which not. And, these code references need
compile options which you do not want to distribute over your whole
program.  Finally, in a daemon application, you do not want to create
the translators when used (which can be in every client again), but once
during the initiation of the daemon.

One of the most important contributions to the compile management, is
the addition of smart prefix handling. This means that you can use
prefixed names in stead of full types, often created with
M<XML::Compile::Util::pack_type()>.

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

=option  any_element CODE|'TAKE_ALL'|'SKIP_ALL'|'ATTEMPT'|'SLOPPY'
=default any_element 'SKIP_ALL'
reader: C<ATTEMPT> will convert all any elements, applying the reader for
each element found. When an element is not found in a schema, it will
be included as M<XML::LibXML::Element> node.

[0.93] reader: With C<SLOPPY>, first automatic typed conversion is
attempted. But is the type is not known, M<XML::LibXML::Simple::XMLin()>
is called to the resque.
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

    my $p = $self->{XCC_namespaces}
          = $self->_namespaceTable(delete $args->{prefixes});
    my %a = map { ($_->{prefix} => $_) } values %$p;
    $self->{XCC_prefixes} = keys %$p ? \%a : $p;

    if(my $anyelem = $args->{any_element})
    {   my $code = $anyelem eq 'ATTEMPT' ? sub {$self->_convertAnyTyped(@_)}
                 : $anyelem eq 'SLOPPY'  ? sub {$self->_convertAnySloppy(@_)}
                 :                         $anyelem;

        if(ref $self->{XCC_ropts} eq 'ARRAY')
             { push @{$self->{XCC_ropts}}, any_element => $code }
        else { $self->{XCC_ropts}{any_element} = $code }
    }

    $self;
}

#----------------------

=section Accessors

=method prefixes [PAIRS|ARRAY|HASH]
Returns the HASH with prefix to name-space translations.  You should not
modify the returned HASH.  New PAIRS of prefix to namespace relations
can be passed.

[0.14]
If a name-space appears for the second time, then the new prefix will be
recognized by M<findName()>, but not used in the output.  When the prefix
already exists for a different namespace, then an error will be casted.

[0.90]
You may also provide an ARRAY of pairs or a HASH.
=cut

sub prefixes(@)
{   my $self = shift;
    my ($p, $a) = @$self{ qw/XCC_namespaces XCC_prefixes/ };
    @_ or return $p;

    my @pairs
      = @_ > 1               ? @_
      : ref $_[0] eq 'ARRAY' ? @{$_[0]}
      : ref $_[0] eq 'HASH'  ? %{$_[0]}
      : error __x"prefixes() expects list of PAIRS, and ARRAY or a HASH";

    while(@pairs)
    {   my ($prefix, $ns) = (shift @pairs, shift @pairs);
        $p->{$ns} ||= { uri => $ns, prefix => $prefix, used => 0 };

        if(my $def = $a->{$prefix})
        {   if($def->{uri} ne $ns)
            {   error __x"prefix {prefix} already refers to {uri}, cannot use it for {ns}"
                  , prefix => $prefix, uri => $def->{uri}, ns => $ns;
            }
        }
        else
        {   $a->{$prefix} = $p->{$ns};
            trace "register prefix $prefix for '$ns'";
        }
    }
    $p;
}

=method prefix PREFIX
Lookup a prefix definition.  This returns a HASH with namespace info.
=cut

sub prefix($) { $_[0]->{XCC_prefixes}{$_[1]} }

=method prefixFor URI
Lookup the preferred prefix for the URI.
=cut

sub prefixFor($)
{   my $def = $_[0]->{XCC_namespaces}{$_[1]} or return ();
    $def->{used}++;
    $def->{prefix};
}

=method prefixed TYPE
Translate the fully qualified TYPE into a prefixed version.  Will produce
an error if the namespace is unknown.
=cut

sub prefixed($)
{   my ($self, $type) = @_;
    my ($ns, $local) = unpack_type $type;
    $ns or return $local;
    my $prefix = $self->prefixFor($ns);
    defined $prefix
        or error __x"no prefix known for namespace {ns}", ns => $ns;

    length $prefix ? "$prefix:$local" : $local;
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
    my ($need_r, $need_w) = $self->_need($need || 'RW');

    if($need_r)
    {   while(my($type, $opts) = each %{$self->{XCC_dropts}})
        {   if(defined $usens)
            {   my ($myns, $local) = unpack_type $type;
                next if $usens eq $myns;
            }
            $self->{XCC_rcode}{$type} ||= $self->compile(READER=>$type,@$opts);
        }
    }

    if($need_w)
    {   while(my($type, $opts) = each %{$self->{XCC_dwopts}})
        {   if(defined $usens)
            {   my ($myns, $local) = unpack_type $type;
                next if $usens eq $myns;
            }
            $self->{XCC_wcode}{$type} ||= $self->compile(WRITER=>$type,@$opts);
        }
    }
}

=method reader TYPE|NAME, OPTIONS
Returns the reader CODE for the TYPE or NAME (see M<findName()>).
OPTIONS are only permitted if M<new(allow_undeclared)> is true, and the
same as the previous call to this method.

The reader will be compiled the first time that it is used, and that
same CODE reference will be returned each next request for the same
TYPE.  Call M<compileAll()> to have all readers compiled by force.

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
    {   trace __x"ignoring options to pre-declared reader {name}"
          , name => $name if @_;

        return $readers->{$type}
            if $readers->{$type};
    }
    elsif($self->allowUndeclared)
    {   if(my $ur = $self->{XCC_uropts}{$type})
        {   my $differs = @$ur != @_;
            unless($differs)
            { for(my $i=0; $i<@$ur; $i++) {$differs++ if $ur->[$i] ne $_[$i]} } 

            # do not use cached version when options differ
            return $self->compile(READER => $type, @_)
                if $differs;
        }
        else
        {   $self->{XCC_uropts}{$type} = \@_;
        }
    }
    elsif(exists $self->{XCC_dwopts}{$type})
         { error __x"type {name} is only declared as writer", name => $name }
    else { error __x"type {name} is not declared", name => $name }

    $readers->{$type} ||= $self->compile(READER => $type, @_);
}

=method writer TYPE|NAME
Returns the writer CODE for the TYPE or NAME (see M<findName()>).
OPTIONS are only permitted if M<new(allow_undeclared)> is true, and the
same as the previous call to this method.

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
    {   trace __x"ignoring options to pre-declared writer {name}"
          , name => $name if @_;

        return $writers->{$type}
            if $writers->{$type};
    }
    elsif($self->{XCC_undecl})
    {   if(my $ur = $self->{XCC_uwopts}{$type})
        {   my $differs = @$ur != @_;
            unless($differs)
            { for(my $i=0; $i<@$ur; $i++) {$differs++ if $ur->[$i] ne $_[$i]} }

            # do not use cached version when options differ
            return $self->compile(WRITER => $type, @_)
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

    $writers->{$type} ||= $self->compile(WRITER => $type, @_);
}

sub template($$)
{   my ($self, $action, $name) = (shift, shift, shift);
    my $type   = $self->findName($name);

    my @rwopts = $action eq 'PERL'
      ? ($self->{XCC_ropts}, $self->{XCC_dropts}{$type})
      : ($self->{XCC_wopts}, $self->{XCC_dwopts}{$type});

    my @opts = $self->mergeCompileOptions($self->{XCC_opts}, @rwopts, \@_);

    $self->SUPER::template($action, $type, @opts);
}

# Create a list with options for X::C::Schema::compile(), from a list of ARRAYs
# and HASHES with options.  The later options overrule the older, but in some
# cases, the new values are added.  This method knows how some of the options
# of ::compile() behave.  [last update X::C v0.98]

sub mergeCompileOptions($$$)
{   my ($self, $action, $type, $opts) = @_;
    my @action_opts
      = $action eq 'READER'
      ? ($self->{XCC_ropts}, $self->{XCC_dropts}{$type})
      : ($self->{XCC_wopts}, $self->{XCC_dwopts}{$type});

    my %p    = %{$self->{XCC_namespaces}};
    my %opts = (prefixes => \%p, hooks => [], typemap => {}, xsi_type => {});

    # flatten list of parameters
    my @take = map {!defined $_ ? () : ref $_ eq 'ARRAY' ? @$_ : %$_ }
        $self->{XCC_opts}, @action_opts, $opts;

    while(@take)
    {   my ($opt, $val) = (shift @take, shift @take);
        defined $val or next;

        if($opt eq 'prefixes')
        {   my $t = $self->_namespaceTable($val, 1, 0);  # expand
            @p{keys %$t} = values %$t;   # overwrite old def if exists
        }
        elsif($opt eq 'hooks' || $opt eq 'hook')
        {   my $hooks = $self->_cleanup_hooks($val);
            unshift @{$opts{hooks}}, ref $hooks eq 'ARRAY' ? @$hooks : $hooks
                if $hooks;
        }
        elsif($opt eq 'typemap')
        {   $val ||= {};
            @{$opts{typemap}}{keys %$val} = values %$val;
        }
        elsif($opt eq 'key_rewrite')
        {   unshift @{$opts{key_rewrite}}, ref $val eq 'ARRAY' ? @$val : $val;
        }
        elsif($opt eq 'xsi_type')
        {   while(my ($t, $a) = each %$val)
            {   my @a = ref $a eq 'ARRAY' ? @$a : $a;
                push @{$opts{xsi_type}{$self->findName($t)}}
                   , map $self->findName($_), @a;
            }
        }
        else
        {   $opts{$opt} = $val;
        }
    }

    %opts;
}

# rewrite hooks
sub _cleanup_hooks($)
{   my ($self, $hooks) = @_;
    $hooks or return;

    foreach my $hook (ref $hooks eq 'ARRAY' ? @$hooks : $hooks)
    {   my $types = $hook->{type} or next;
        $hook->{type} =
           [ map {ref $_ eq 'Regexp' ? $_ : $self->findName($_)}
                       ref $types eq 'ARRAY' ? @$types : $types ];
    }
    $hooks;
}

sub _need($)
{    $_[1] eq 'READER' ? (1,0)
   : $_[1] eq 'WRITER' ? (0,1)
   : $_[1] eq 'RW'     ? (1,1)
   : error __x"use READER, WRITER or RW, not {dir}", dir => $_[1];
}

# support prefixes on types
sub addHook(@)
{   my $self = shift;
    my $hook = @_ > 1 ? {@_} : shift;
    $self->_cleanup_hooks($hook);
    $self->SUPER::addHook($hook);
}

sub compile($$@)
{   my ($self, $action, $elem) = splice @_, 0, 3;
    defined $elem
        or error __x"compile() requires action and type parameters";

    $self->SUPER::compile
      ( $action => $self->findName($elem)
      , $self->mergeCompileOptions($action, $elem, \@_)
      );
}

sub compileType($$@)
{   my ($self, $action, $type) = splice @_, 0, 3;
    defined $type
        or error __x"compileType() requires action and type parameters";

    $self->SUPER::compileType
      ( $action => $self->findName($type)
      , $self->mergeCompileOptions($action, $type, \@_)
      );
}

#----------------------
=section Administration

=method declare 'READER'|'WRITER'|'RW', TYPE|ARRAY-of-TYPES, OPTIONS

=cut

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
        trace "declare $type";

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
Translate the NAME specification into a schema defined full type.
The NAME can be a full type (like '{namespace}localname', usually
created with M<XML::Compile::Util::pack_type()>) or a prefixed type
(like 'myns:localname', where C<myns> is defined via M<new(prefixes)>
or M<prefixes()>).

When the form is 'myns:' (so without local name), the namespace uri is
returned.

=examples of findName()
  $schema->prefixes(pre => 'http://namespace');

  my $type = $schema->findName('pre:name');
  print $type;   # {http://namespace}name

  my $ns   = $schema->findName('pre:');
  print $ns;     # http://namespace

  my $type = $schema->findName('{somens}name');
  print $type;   # {somens}name    [a no-op]
=cut

sub findName($)
{   my ($self, $name) = @_;
    defined $name
        or panic "findName called without name";

    return $name if $name =~ m/^\{/;

    my ($prefix,$local) = $name =~ m/^([\w-]*)\:(\S*)$/ ? ($1,$2) : ('',$name);
    my $def = $self->{XCC_prefixes}{$prefix};
    unless($def)
    {   return $name if $prefix eq '';   # namespace-less
        trace __x"known prefixes: {prefixes}"
          , prefixes => [ sort keys %{$self->{XCC_prefixes}} ];
        error __x"unknown name prefix for `{name}'", name => $name;
    }

    length $local ? pack_type($def->{uri}, $local) : $def->{uri};
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
    $fh->print(@output);
}

#---------------
# Convert ANY elements and attributes

sub _convertAnyTyped(@)
{   my ($self, $type, $nodes, $path, $read, $args) = @_;

    my $key     = $read->keyRewrite($type);
    my $reader  = try { $self->reader($type) };
    if($@)
    {   trace "cannot auto-convert 'any': ".$@->wasFatal->message;
        return ($key => $nodes);
    }
    trace "auto-convert known type 'any' $type";

    my @nodes   = ref $nodes eq 'ARRAY' ? @$nodes : $nodes;
    my @convert = map {$reader->($_)} @nodes;

    ($key => @convert==1 ? $convert[0] : \@convert);
}

sub _convertAnySloppy(@)
{   my ($self, $type, $nodes, $path, $read, $args) = @_;

    my $key     = $read->keyRewrite($type);
    my $reader  = try { $self->reader($type) };
    if($@)
    {   # unknown type or untyped...
        my @convert = map {XMLin $_} @$nodes;
        return ($key => @convert==1 ? $convert[0] : \@convert);
    }
    else
    {   trace "auto-convert known 'any' $type";
        my @nodes   = ref $nodes eq 'ARRAY' ? @$nodes : $nodes;
        my @convert = map {$reader->($_)} @nodes;

        ($key => @convert==1 ? $convert[0] : \@convert);
    }
}

1;
