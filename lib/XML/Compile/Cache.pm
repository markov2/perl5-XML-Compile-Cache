use warnings;
use strict;

package XML::Compile::Cache;
use base 'XML::Compile::Schema';

use Log::Report 'xml-compile-cache', syntax => 'SHORT';

use XML::Compile::Util   qw/pack_type unpack_type/;
use List::Util           qw/first/;
use Scalar::Util         qw/weaken/;
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

=c_method new %options

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

=option  typemap HASH|ARRAY
=default typemap {}

=option  xsi_type HASH|ARRAY
=default xsi_type {}

=option  opts_rw HASH|ARRAY-of-PAIRS
=default opts_rw []
Options added to both READERs and WRITERS.  Options which are passed
with M<declare()> and C<opts_readers> or C<opts_writers> will overrule
these.  See M<addCompileOptions()>.

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
=default any_element 'ATTEMPT'
See M<anyElement()>.

[1.02] the default is to ATTEMPT compiling any handlers automatically.
Before version 1.02, the default was to SKIP_ALL elements which would
match the occurs and namespace restrictions of the any specification.
However, that fails for reperative blocks (for instance, it fails for
an choice which may occur unbounded times)
=cut

sub init($)
{   my ($self, $args) = @_;
    $self->addPrefixes($args->{prefixes});

    $self->SUPER::init($args);

    $self->{XCC_opts}   = delete $args->{opts_rw}      || [];
    $self->{XCC_ropts}  = delete $args->{opts_readers} || [];
    $self->{XCC_wopts}  = delete $args->{opts_writers} || [];
    $self->{XCC_undecl} = delete $args->{allow_undeclared} || 0;

    $self->{XCC_rcode}  = {};  # compiled code refs
    $self->{XCC_wcode}  = {};
    $self->{XCC_dropts} = {};  # declared opts
    $self->{XCC_dwopts} = {};
    $self->{XCC_uropts} = {};  # undeclared opts
    $self->{XCC_uwopts} = {};

    $self->{XCC_readers} = {};
    $self->{XCC_writers} = {};

    $self->typemap($args->{typemap});
    $self->xsiType($args->{xsi_type});
    $self->anyElement($args->{any_element} || 'ATTEMPT');

    $self;
}

#----------------------

=section Accessors

=method typemap [HASH|ARRAY|PAIRS]
[0.98] Add global knowledge on typemaps.  Returns the typemap.
=cut

sub typemap(@)
{   my $self = shift;
    my $t    = $self->{XCC_typemap} ||= {};
    my @d    = @_ > 1 ? @_ : !defined $_[0] ? ()
             : ref $_[0] eq 'HASH' ? %{$_[0]} : @{$_[0]};
    while(@d) { my $k = $self->findName(shift @d); $t->{$k} = shift @d }
    $t;
}

=method addXsiType [HASH|ARRAY|LIST]
[1.01] add global xsi_type declarations.  Returns the xsiType set.
The ARRAY or LIST contains pairs, just like the HASH.

The value component can be 'AUTO' to automatically detect the C<xsi:type>
extensions.  This does only work for complex types.

=cut

sub addXsiType(@)
{   my $self = shift;
    my $x    = $self->{XCC_xsi_type} ||= {};
    my @d    = @_ > 1 ? @_ : !defined $_[0] ? ()
             : ref $_[0] eq 'HASH' ? %{$_[0]} : @{$_[0]};

    while(@d)
    {   my $k = $self->findName(shift @d);
        my $a = shift @d;
        $a = $self->namespaces->autoexpand_xsi_type($k) || []
            if $a eq 'AUTO';

        push @{$x->{$k}}
          , ref $a eq 'ARRAY' ? (map $self->findName($_), @$a)
          :                     $self->findName($a);
    }

    $x;
}
*xsiType = \&addXsiType;

=method allowUndeclared [BOOLEAN]
Whether it is permitted to create readers and writers which are not
declared cleanly.
=cut

sub allowUndeclared(;$)
{   my $self = shift;
    @_ ? ($self->{XCC_undecl} = shift) : $self->{XCC_undecl};
}

=method anyElement 'ATTEMPT'|'SLOPPY'|'SKIP_ALL'|'TAKE_ALL'|CODE
[as method since 0.99] How to process ANY elements, see also
M<new(any_element)>.

Reader: C<ATTEMPT> will convert all any elements, applying the reader for
each element found. When an element is not found in a schema, it will
be included as M<XML::LibXML::Element> node.

[0.93] Reader: With C<SLOPPY>, first automatic typed conversion is
attempted. But is the type is not known, M<XML::LibXML::Simple::XMLin()>
is called to the resque.
=cut

sub anyElement($)
{   my ($self, $anyelem) = @_;

    # the "$self" in XCC_ropts would create a ref-cycle, causing a
    # memory leak.
    my $s = $self; weaken $s;

    my $code
      = $anyelem eq 'ATTEMPT' ? sub {$s->_convertAnyTyped(@_)}
      : $anyelem eq 'SLOPPY'  ? sub {$s->_convertAnySloppy(@_)}
      :                         $anyelem;
     
    $self->addCompileOptions(READERS => any_element => $code);
    $code;
}

#----------------------

=section Prefix management

The cache layer on top of M<XML::Compile::Schema> adds smart use of
prefixes.  Of course, smartness comes with a small performance cost,
but the code gets much cleaner.

=method addPrefixes [PAIRS|ARRAY|HASH]
The X::C logic does auto-detect prefix/namespaces combinations from
the XML, but does not search extensively for namespace declarations.
Also, sometimes the same namespace is used with different prefixes.
Sometimes, the same prefix is used for different namesapces.  To complete
the list, or control the actual prefix being used, you explicitly declare
combinations.

The B<best way> to add prefixes is via M<new(prefixes)>, which will give
your names preference over the names found in the schema's which get loaded.
For instance, use C<< ::WSDL->new(prefixes => [ $prefix => $ns ] >>

[0.995] Returns the HASH with prefix to name-space translations.  You should
not modify the returned HASH: new PAIRS of prefix to namespace relations
can be passed as arguments.

[0.14] If a name-space appears for the second time, then the new
prefix will be recognized by M<findName()>, but not used in the output.
When the prefix already exists for a different namespace, then an error
will be casted.

[0.90] You may also provide an ARRAY of pairs or a HASH.
=cut

sub addPrefixes(@)
{   my $self  = shift;
    my $p     = $self->{XCC_namespaces} ||= {};
    my $first = shift;
    @_ or defined $first
        or return $p;

    my @pairs
      = @_                    ? ($first, @_)
      : ref $first eq 'ARRAY' ? @$first
      : ref $first eq 'HASH'  ? %$first
      : error __x"prefixes() expects list of PAIRS, an ARRAY or a HASH";
# warn "new prefixes: @pairs\n";

    my $a    = $self->{XCC_prefixes} ||= {};
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


=method prefixes [$params]
Return prefixes table.  The $params are deprecated since [0.995], see
M<addPrefixes()>.
=cut

sub prefixes(@)
{   my $self = shift;
    return $self->addPrefixes(@_) if @_;
    $self->{XCC_namespaces} || {};
}

=method prefix $prefix
Lookup a prefix definition.  This returns a HASH with namespace info.
=cut

sub prefix($) { $_[0]->{XCC_prefixes}{$_[1]} }

# [0.995] should this be public?
sub byPrefixTable() { shift->{XCC_prefixes} }

=method prefixFor $uri
Lookup the preferred prefix for the $uri.
=cut

sub prefixFor($)
{   my $def = $_[0]->{XCC_namespaces}{$_[1]} or return ();
    $def->{used}++;
    $def->{prefix};
}

=method learnPrefixes $node
[0.993] Take all the prefixes defined in the $node, and M<XML::LibXML::Element>.
This is not recursive: only on those defined at the top $node.
=cut

sub learnPrefixes($)
{   my ($self, $node) = @_;
    my $namespaces = $self->prefixes;

  PREFIX:
    foreach my $ns ($node->getNamespaces)  # learn preferred ns
    {   my ($prefix, $uri) = ($ns->getLocalName, $ns->getData);
        next if !defined $prefix || $namespaces->{$uri};

        if(my $def = $self->prefix($prefix))
        {   next PREFIX if $def->{uri} eq $uri;
        }
        else
        {   $self->addPrefixes($prefix => $uri);
            next PREFIX;
        }

        $prefix =~ s/0?$/0/;
        while(my $def = $self->prefix($prefix))
        {   next PREFIX if $def->{uri} eq $uri;
            $prefix++;
        }
        $self->addPrefixes($prefix => $uri);
    }
}

sub addSchemas($@)
{   my ($self, $xml) = (shift, shift);
    $self->learnPrefixes($xml);
    $self->SUPER::addSchemas($xml, @_);
}

=method prefixed $type|<$ns,$local>
Translate the fully qualified $type into a prefixed version.  Will produce
undef if the namespace is unknown.

[0.993] When your $type is not in packed form, you can specify a namespace
and $local type name as separate arguments.

=example
   print $schema->prefixed($type) || $type;
   print $schema->prefixed($ns, $local);
=cut

sub prefixed($;$)
{   my $self = shift;
    my ($ns, $local) = @_==2 ? @_ : unpack_type(shift);
    $ns or return $local;
    my $prefix = $self->prefixFor($ns);
    defined $prefix
        or error __x"no prefix known for namespace `{ns}', use addPrefixes()"
            , ns => $ns;

    length $prefix ? "$prefix:$local" : $local;
}

#----------------------

=section Compilers

The name of this module refers to its power to administer compiled
XML encoders (writers) and decoders (readers).  This means that
your program only need to pass on a ::Cache object (for instance
a M<XML::Compile::WSDL11>, not a CODE reference for each compiled
translator.

=method compileAll ['READERS'|'WRITERS'|'RW', [$ns]]
Compile all the declared readers and writers with the default 'RW').  You may
also select to pre-compile only the READERS or only the WRITERS.  The
selection can be limited further by specifying a $ns.

By default, the processors are only compiled when used.  This method is
especially useful in a B<daemon process>, where preparations can take as
much time as they want to... and running should be as fast as possible.
=cut

sub compileAll(;$$)
{   my ($self, $need, $usens) = @_;
    my ($need_r, $need_w) = $self->_need($need || 'RW');

    if($need_r)
    {   foreach my $type (keys %{$self->{XCC_dropts}})
        {   if(defined $usens)
            {   my ($myns, $local) = unpack_type $type;
                next if $usens eq $myns;
            }
            $self->{XCC_rcode}{$type} ||= $self->compile(READER=>$type);
        }
    }

    if($need_w)
    {   foreach my $type (keys %{$self->{XCC_dwopts}})
        {   if(defined $usens)
            {   my ($myns, $local) = unpack_type $type;
                next if $usens eq $myns;
            }
            $self->{XCC_wcode}{$type} ||= $self->compile(WRITER => $type);
        }
    }
}

=method reader $type|$name, %options
Returns the reader CODE for the $type or $name (see M<findName()>).
%options are only permitted if M<new(allow_undeclared)> is true, and the
same as the previous call to this method.

The reader will be compiled the first time that it is used, and that
same CODE reference will be returned each next request for the same
$type.  Call M<compileAll()> to have all readers compiled by force.

=examples
  my $schema = XML::Compile::Cache->new(\@xsd,
     prefixes => [ gml => $GML_NAMESPACE ] );
  my $data   = $schema->reader('gml:members')->($xml);

  my $getmem = $schema->reader('gml:members');
  my $data   = $getmem->($xml);
=cut

sub _same_params($$)
{   my ($f, $s) = @_;
    @$f==@$s or return 0;
    for(my $i=0; $i<@$f; $i++)
    {   return 0 if !defined $f->[$i] ? defined $s->[$i]
                  : !defined $s->[$i] ? 1 : $f->[$i] ne $s->[$i];
    }
    1;
}

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
        {   # do not use cached version when options differ
            _same_params $ur, \@_
                or return $self->compile(READER => $type, @_);
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

=method writer $type|$name
Returns the writer CODE for the $type or $name (see M<findName()>).
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
        {   # do not use cached version when options differ
            _same_params $ur, \@_
                or return $self->compile(WRITER => $type, @_)
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

sub template($$@)
{   my ($self, $action, $name) = (shift, shift, shift);
    $action =~ m/^[A-Z]*$/
        or error __x"missing or illegal action parameter to template()";

    my $type  = $self->findName($name);
    my @opts = $self->mergeCompileOptions($action, $type, \@_);
    $self->SUPER::template($action, $type, @opts);
}

=method addCompileOptions ['READERS'|'WRITERS'|'RW'], %options
[0.99] You may provide global compile options with M<new(opts_rw)>,
C<opts_readers> and C<opts_writers>, but also later using this method.
=cut

sub addCompileOptions(@)
{   my $self = shift;
    my $need = @_%2 ? shift : 'RW';

    my $set
      = $need eq 'RW'      ? $self->{XCC_opts}
      : $need eq 'READERS' ? $self->{XCC_ropts}
      : $need eq 'WRITERS' ? $self->{XCC_wopts}
      : error __x"addCompileOptions() requires option set name, not {got}"
          , got => $need;

    if(ref $set eq 'HASH')
         { while(@_) { my $k = shift; $set->{$k} = shift } }
    else { push @$set, @_ }
    $set;
}

# Create a list with options for X::C::Schema::compile(), from a list of ARRAYs
# and HASHES with options.  The later options overrule the older, but in some
# cases, the new values are added.  This method knows how some of the options
# of ::compile() behave.  [last update X::C v0.98]

sub mergeCompileOptions($$$)
{   my ($self, $action, $type, $opts) = @_;

    my @action_opts
      = ($action eq 'READER' || $action eq 'PERL')
      ? ($self->{XCC_ropts}, $self->{XCC_dropts}{$type})
      : ($self->{XCC_wopts}, $self->{XCC_dwopts}{$type});

    my %p    = %{$self->{XCC_namespaces}};
    my %t    = %{$self->{XCC_typemap}};
    my %x    = %{$self->{XCC_xsi_type}};
    my %opts = (prefixes => \%p, hooks => [], typemap => \%t, xsi_type => \%x);

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
            if(ref $val eq 'ARRAY')
            {   while(@$val)
                {   my $k = $self->findName(shift @$val); 
                    $t{$k} = shift @$val;
                }
            }
            else
            {   while(my($k, $v) = each %$val)
                {   $t{$self->findName($k)} = $v;
                }
            }
        }
        elsif($opt eq 'key_rewrite')
        {   unshift @{$opts{key_rewrite}}, ref $val eq 'ARRAY' ? @$val : $val;
        }
        elsif($opt eq 'xsi_type')
        {   while(my ($t, $a) = each %$val)
            {   my @a = ref $a eq 'ARRAY' ? map($self->findName($_), @$a)
                  : $self->findName($a);
                push @{$x{$self->findName($t)}},  @a;
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

my %need = (READER => [1,0], WRITER => [0,1], RW => [1,1]);
$need{READERS} = $need{READER};
$need{WRITERS} = $need{WRITER};

sub _need($)
{   my $need = $need{$_[1]}
       or error __x"use READER, WRITER or RW, not {dir}", dir => $_[1];
    @$need;
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

=method declare <'READER'|'WRITER'|'RW'>, <$type|ARRAY>, %options
Register that the indicated $type (or ARRAY of them) may be used, and needs to
be translated with the %options (either specified as ARRAY or PAIRS).
Specify whether it may get used as READER, WRITER, or both (RW).  If the
READER and WRITER need different options, then you need to declare them
separately; in that case you cannot use RW.

The $type should be understood by M<findName()>, so may be prefixed.

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
        trace "declare $type $need";

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

=method findName $name
Translate the $name specification into a schema defined full type.
The $name can be a full type (like '{namespace}localname', usually
created with M<XML::Compile::Util::pack_type()>) or a prefixed type
(like 'myns:localname', where C<myns> is defined via M<new(prefixes)>
or M<prefixes()>).

When the form is 'myns:' (so without local name), the namespace uri is
returned.

=examples of findName()
  $schema->addPrefixes(pre => 'http://namespace');

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
        error __x"unknown name prefix `{prefix}' for `{name}'"
           , prefix => $prefix, name => $name;
    }

    length $local ? pack_type($def->{uri}, $local) : $def->{uri};
}

=method printIndex [$fh], %options

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
{   my ($self, $type, $nodes, $path, $read) = @_;

{no warnings;
defined $read or warn join ';', caller(0);
}
    my $key     = $read->keyRewrite($type);
    my $reader  = try { $self->reader($type) };
    if($@)
    {   trace "cannot auto-convert 'any': ".$@->wasFatal->message;
        return ($key => $nodes);
    }
    trace "auto-convert known type for 'any': $type";

    my @nodes   = ref $nodes eq 'ARRAY' ? @$nodes : $nodes;
    my @convert = map $reader->($_), @nodes;
    ($key => (@convert==1 ? $convert[0] : \@convert) );
}

sub _convertAnySloppy(@)
{   my ($self, $type, $nodes, $path, $read) = @_;

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
