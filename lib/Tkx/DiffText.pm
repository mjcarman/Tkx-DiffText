#===============================================================================
# Tkx/DiffText.pm
# Michael J. Carman <mjcarman@cpan.org>
# Last Modified: 1/05/2009 10:24AM
#===============================================================================
use 5.006;
package Tkx::DiffText;
use strict;
use warnings;

use Tkx;
use Tkx::ROText;
use Tie::Tk::Text;

use base qw(Tkx::widget Tkx::MegaConfig);

our $VERSION = '0.01';

__PACKAGE__->_Mega("tkx_DiffText");
__PACKAGE__->_Config(
	# overall widget size
	-width            => [qw'.wf width  Width  780'],
	-height           => [qw'.hf height Height 580'],
	# Everthing else will probably require using METHOD dispatches :(
);

# TBD: Finish porting config options
#	$self->ConfigSpecs(
#		# overall widget size
#		-width            => [{-width      => $wf }, qw'width  Width  780'],
#		-height           => [{-height     => $hf }, qw'height Height 580'],
#
#		# aliases for controlling gutter configuration
#		-gutterbackground => [{-background => [_gw('gutter', @p)]}, qw'background Background', '#f0f0f0'],
#		-gutterforeground => [{-foreground => [_gw('gutter', @p)]}, qw'foreground Foreground', '#5c5c5c'],
#
#		# We want the gutter to look like it's part of the main ROText widget, 
#		# not a seperate one. To do that we set -borderwidth to 0 on the widgets 
#		# and use the frames enclosing them to provide the borders.
#		-relief           => [[_gw('frame', @p)], qw'relief      Relief      sunken'],
#		-borderwidth      => [[_gw('frame', @p)], qw'borderwidth borderWidth 2'     ],
#
#		# pass most options through to the ROText widgets.
#		# Sometimes to just the text ones, sometimes to the gutters too.
#		DEFAULT           => [[_gw('text', @p)]],
#		-background       => [[_gw('text', @p), $dm]], # DEFAULT doesn't catch fg/bg?
#		-foreground       => [[_gw('text', @p), $dm]],
#		-font             => [[_gw('text', @p), _gw('gutter', @p)]], # sync gutter font to text for vertical alignment
#		-pady             => [[_gw('text', @p), _gw('gutter', @p)]], # pad gutter too for y, (valign) but not for x
#		-wrap             => [[_gw('text', @p)], qw'wrap Wrap none'],
#	);
#
#	$self->Advertise(text_a       => $p[0]->{text}      );
#	$self->Advertise(gutter_a     => $p[0]->{gutter}    );
#	$self->Advertise(xscrollbar_a => $p[0]->{xscrollbar});
#	$self->Advertise(yscrollbar_a => $p[0]->{yscrollbar});
#
#	$self->Advertise(text_b       => $p[1]->{text}      );
#	$self->Advertise(gutter_b     => $p[1]->{gutter}    );
#	$self->Advertise(xscrollbar_b => $p[1]->{xscrollbar});
#	$self->Advertise(yscrollbar_b => $p[1]->{yscrollbar});
#
#	$self->Advertise(canvas => $dm);

my $initialized;
my $tile_available;


#-------------------------------------------------------------------------------
# Method  : _ClassInit
# Purpose : Class initialization.
# Notes   : 
#-------------------------------------------------------------------------------
sub _ClassInit {
	# determine availability of themed widgets
	$tile_available = eval { Tkx::package_require('tile') };

	# This module is pretty crippled if we can't do diffs, but let's degrade
	# nicely instead of dieing.
	eval {
		require Algorithm::Diff;
		require Tie::Tk::Text;
	};
	*compare = $@ ? \&_pad_text : \&_compare_text;

	# Only needed to resolve passing data inputs as *FH{IO} or *FH{FILEHANDLE}
	eval {require IO::File};
	
	$initialized++;
}


#-------------------------------------------------------------------------------
# Method  : _Populate
# Purpose : Create a DiffText composite widget.
# Notes   : 
#-------------------------------------------------------------------------------
sub _Populate {
	my $class  = shift;
	my $widget = shift;
	my $path   = shift;
	my %opt    = (
		-tile   => 1,
		-gutter => 1,
		-orient => 'vertical',
		-map    => 'scaled',
		# Is there a way to pull these (preferably automatically) from the defaults
		# specified in the config spec above? (Otherwise, what's the point of it?)
		-width  => 780,
		-height => 580,
		@_
	);

	_ClassInit() unless $initialized;

	# create the megawidget
	my $self = ($tile_available && $opt{-tile})
		? $class->new($path)->_parent->new_ttk__frame(-name => $path, -class => 'Tkx_FindBar')
		: $class->new($path)->_parent->new_frame(-name => $path, -class => 'Tkx_FindBar');
	$self->_class($class);

	# initialize instance data
	my $data = $self->_data();

	$data->{diffcolors} = {
		add => [-background => '#ccffcc'],
		del => [-background => '#ffcccc'],
		mod => [-background => '#aed7ff'],
		pad => [-background => '#f0f0f0'],
		cur => [-background => '#ffff80'],
	};

	if ($opt{-diffcolors}) {
		while (my ($k, $v) = each %{$opt{-diffcolors}}) {
			$data->{diffcolors}{$k} = $v;
		}
		delete $opt{-diffcolors};
	}

	$data->{-tile}        = $tile_available && delete $opt{-tile};
	$data->{-gutter}      = delete $opt{-gutter};
	$data->{-orient}      = delete $opt{-orient};
	$data->{_map}{type}   = delete $opt{-map};
# TBD
#	$data->{_map}{colors} = _get_map_colors($data->{diffcolors});

	Tkx::bind($self, '<Configure>', [\&_rescale_map, $self]);

	# I'm not sure whether it's a bug of a feature, but frames with contents
	# always collapse down to just what's needed by the widgets they contain.
	# This makes setting the width and height of the composite widget worthless.
	# Empty frames, on the other hand, *do* respect height and width settings.
	# We exploit this by creating invisible frames along the top and left edges
	# of the composite widget and using them to control its (minimum) size.
	my $f = $self;
	$self->_frame($f, -name => 'wf', -borderwidth => 0)->g_pack(-side => 'top'); 
	$self->_frame($f, -name => 'hf', -borderwidth => 0)->g_pack(-side => 'left');

	# The difference map
	my $dm = $f->new_canvas(
		-name               => 'map',
		-width              => 20, #
		-height             => 1,  # fills to match text areas
		-takefocus          => 0,  #
		-highlightthickness => 0,  #
	);

# TBD: Note that the $self arg has been moved because of positional requirements
# when using Tkx::Ev()
#	$dm->Tk::bind('<Button-1>', [\&_mapjump, $self, Tk::Ev('x'), Tk::Ev('y')]);
	Tkx::bind($dm, '<Button-1>', [\&_mapjump, Tkx::Ev('%x', '%y'), $self]);

	unless ($data->{_map}{type} eq 'none') {
		$data->{_map}{height} = 0;
		$data->{_map}{scale}  = 1;
		$dm->g_pack(-side => 'left', -fill => 'y');
	}

	my @p = (
		$self->_make_pane($f, 'a'),
		$self->_make_pane($f, 'b'),
	);

	$data->{_textarray} = {
		a => $p[0]->{textarray},
		b => $p[1]->{textarray},
	};
	$data->{_scroll_lock} = 0;

	$data->{_diffloc} = [];
	$data->{_current} = undef;

	# Set up synchronized scrolling between panes. It can be turned on/off by
	# toggling the _scroll_lock flag.
	for my $i (0 .. 1) {
		foreach my $w (@{$p[$i]->{scroll_locked}}) {
#			$w->configure(-yscrollcommand =>
#				[\&_scroll_panes, $w, \@p, $i, $self]);
		}
	}

	while (my ($opt, $val) = each %opt) {
		$self->configure($opt => $val);
	}

	return $self;
}


#-------------------------------------------------------------------------------
# Method  : _make_pane
# Purpose : Create a frame with a text widget, gutter, and scrollbars
# Notes   : 
#-------------------------------------------------------------------------------
sub _make_pane {
	my $self = shift;
	my $pw   = shift; # parent widget
	my $name = shift;
	my $data = $self->_data;

	# where to pack pane
	my $side = $data->{-orient} eq 'horizontal' ? 'top' : 'left';

	my $f = $self->_frame($pw, -name => $name);

	$f->g_pack(
		-side   => $side,
		-fill   => 'both',
		-expand => 1,
	);

	my $vsb = $self->_scrollbar($f, -name => 'xsb', -orient => 'vertical');
	my $hsb = $self->_scrollbar($f, -name => 'ysb', -orient => 'horizontal');
	my $gw  = $f->new_tkx_ROText(
		-name        => 'gutter',
		-height      => 1, # height fills to match text areas
		-width       => 1, # just for starters
		-borderwidth => 0,
		-state       => 'disabled',
		-wrap        => 'none',
	);
	my $tw  = $f->new_tkx_ROText(
		-name           => 'text',
		-width          => 1, # size controlled via parent so that panes are
		-height         => 1, # always balanced even when window resized.
		-borderwidth    => 0,
		-wrap           => 'none',
		-xscrollcommand => "$hsb set",
		-yscrollcommand => "$vsb set",
	);
	

	Tkx::grid($gw, $tw, $vsb, -sticky => 'nsew');
	Tkx::grid($hsb, '-', -sticky => 'ew');
	Tkx::grid('rowconfigure',    $f, 0, '-weight', 1);
	Tkx::grid('columnconfigure', $f, 1, '-weight', 1);

	my @text; tie @text, 'Tie::Tk::Text', $tw;

	my $diffcolors = $data->{diffcolors};
	$gw->tag('configure', 'pad', @{[]});
	$tw->tag('configure', 'pad', @{$diffcolors->{pad}});
	$tw->tag('configure', 'add', @{$diffcolors->{add}});
	$tw->tag('configure', 'del', @{$diffcolors->{del}});
	$tw->tag('configure', 'mod', @{$diffcolors->{mod}});
	$tw->tag('configure', 'cur', @{$diffcolors->{cur}});
	$tw->tag('raise', 'sel');

	$hsb->configure(-command => [$tw->_mpath('xview'), 'xview']);

	my @slw;  # scroll-locked widgets

	if ($data->{-gutter}) {
		@slw = ($tw, $gw);
	}
	else {
		@slw = $tw;  # Don't lock the gutter if it's not visible!
		Tkx::grid('forget', $gw) unless $data->{-gutter};
	}

	# TBD: This must be translated to TCL, as the scrollbar bindings happen in Tcl/Tk
	# scrollbar controls both text and gutter (if visible)
#	$vsb->configure(-command => sub { $_->yview(@_) foreach (@slw) });
	$vsb->configure(-command => [$tw->_mpath('yview'), 'yview']);

	# widgets will have their yscrollcommand set *after* we're done creating
	# all of the panes so that we can regulate the synchronized scrolling
	# between them.

	return {
		frame         => $f,
		yscrollbar    => $vsb,
		xscrollbar    => $hsb,
		gutter        => $gw,
		text          => $tw,
		textarray     => \@text,
		scroll_locked => \@slw,
	};

}


#-------------------------------------------------------------------------------
# Subroutine : _frame
# Purpose    : Create a frame widget using the tile setting.
# Notes      : 
#-------------------------------------------------------------------------------
sub _frame {
	my $self   = shift;
	my $parent = shift;

	return $self->_data->{-tile}
		? $parent->new_ttk__frame(@_)
		: $parent->new_frame(@_);
}


#-------------------------------------------------------------------------------
# Subroutine : _scrollbar
# Purpose    : Create a scrollbar widget using the tile setting.
# Notes      : 
#-------------------------------------------------------------------------------
sub _scrollbar {
	my $self   = shift;
	my $parent = shift;

	return $self->_data->{-tile}
		? $parent->new_ttk__scrollbar(@_)
		: $parent->new_scrollbar(@_);
}


sub _rescale_map {}
sub _scroll_panes {}
sub _reset_tags() {}

#-------------------------------------------------------------------------------
# Method  : load
# Purpose : Load data into one of the text panes.
# Notes   : 
#-------------------------------------------------------------------------------
sub load {
	my $self  = $_[0];
	my $where = lc $_[1];
	my $ok    = 1;

	unless ($where =~ /^[ab]$/) {
		carp("Invalid load destination '$_[1]'");
		return;
	}

	$self->_data->{_scroll_lock} = 0;

	$self->_kid('map')->delete('all');
	$self->_reset_tags('a');
	$self->_reset_tags('b');

	my $tw = $self->_kid("$where.text");
	my $gw = $self->_kid("$where.gutter");
	my $ta = $self->_data->{_textarray}{$where};

	$gw->configure(-state => 'normal');
	$gw->delete('1.0', 'end');
	$tw->delete('1.0', 'end');

	Tkx::update('idletasks');

	# Accept naive user input
	$_[2] = ${$_[2]} if ref $_[2] eq 'REF';  # \*FH{IO} instead of *FH{IO}
	$_[2] = *{$_[2]} if ref $_[2] eq 'GLOB'; # \*FH     instead of *FH

	if (ref $_[2]) {
		if (ref $_[2] eq 'ARRAY') {
			# assume lines of file data
			$tw->insert('end', $_) foreach @{$_[2]};
		}
		elsif ($_[2]->can('getline')) {
			# IO::File must be loaded for this to work
			while (my $x = $_[2]->getline) {
				$tw->insert('end', $x); # assume IO::File or equiv
			}
		}
		else {
			carp(sprintf("Don't know how to load from '%s' reference", ref $_[2]));
			$ok = 0;
		}
	}
	elsif ($_[2] =~ /^\*(\w*::)+\$?\w+$/) {
		# GLOB; assume open filehandle
		# copy to scalar so that <> interprets it as a filehandle
		# and not a glob pattern. cf. perlop - I/O Operators
		my $fh = $_[2];
		local $_;
		do { $tw->insert('end', $_) } while (<$fh>);
	}
	elsif ($_[2] =~ /\n/) {
		# assume contents of slurped file
		$tw->insert('end', $_[2]);
	}		
	else {
		# assume file name
		# Need two-arg open() for perls < v5.6
		# what version added open($fh...) in place of open(FH...)
		local *FH;
		if (open(FH, "< $_[2]")) {
			local $_;
			do { $tw->insert('end', $_) } while (<FH>);
			close(FH);
		}
		else {
			carp("Can't read file '$_[2]' [$!]");
			$ok = 0;
		}
	}

	if ($tw->get('end - 2 chars', 'end') ne "\n\n") {
		# The last line of file doesn't contain a newline. This horks up 
		# synchronized scrolling, so we add one to prevent that from happening.
		$tw->insert('end', "\n");
	}	

	my $n = $ok ? @$ta       :  0;
	my $w = $ok ? length($n) : -1;

	$gw->insert('end', sprintf("%${w}i\n", $_)) foreach (1 .. $n);
	$gw->configure(-width => $w + 1);
	$gw->configure(-state => 'disabled');

	Tkx::update('idletasks');
	return $ok;
}

1;
__END__

=pod

=head1 NAME

Tkx::DiffText - The great new Tkx::DiffText!

=head1 VERSION

Version 0.01

=head1 SYNOPSIS

Quick summary of what the module does.

Perhaps a little code snippet.

    use Tkx::DiffText;

    my $foo = Tkx::DiffText->new();
    ...

=head1 AUTHOR

Michael J. Carman, C<< <mjcarman at cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-tkx-difftext at rt.cpan.org>,
or through the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Tkx-DiffText>.
I will be notified, and then you'll automatically be notified of progress on 
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Tkx::DiffText

You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Tkx-DiffText>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Tkx-DiffText>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Tkx-DiffText>

=item * Search CPAN

L<http://search.cpan.org/dist/Tkx-DiffText>

=back


=head1 ACKNOWLEDGEMENTS


=head1 COPYRIGHT & LICENSE

Copyright 2009 Michael J. Carman, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.


=cut
