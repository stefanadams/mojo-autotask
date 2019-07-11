package Mojo::Autotask::ExecuteCommand;
use Mojo::Base 'Mojo::EventEmitter';

use Mojo::URL;
use Mojo::Util 'camelize';
use Mojolicious::Validator;

use Carp;
use Scalar::Util;

has ec_url => sub { shift->zone->path('/Autotask/AutotaskExtend/ExecuteCommand.aspx') };
has zone => sub { die };

sub AUTOLOAD {
  my ($self, @args) = @_;

  my ($package, $method) = our $AUTOLOAD =~ /^(.+)::(.+)$/;
  Carp::croak "Undefined subroutine &${package}::$method called"
    unless Scalar::Util::blessed $self && $self->isa(__PACKAGE__);

  my $validator  = Mojolicious::Validator->new;
  my $v = $validator->validation;

  $v->input({Code => camelize($method), @args});
  $v->required('Code')->in(qw/OpenTicketDetail OpenProject OpenTicketTime OpenTaskDetail OpenAppointment OpenKBArticle OpenOpportunity OpenContract OpenToDo OpenSalesOrder OpenServiceCall OpenAccount OpenContact EditTimeEntry EditAccount EditContact NewAccountNote NewContact NewTicket EditInstalledProduct NewInstalledProduct TimeOffRequest IPDW/);
  return if $v->has_error;
  return $v->has_error if $v->param('Code') eq 'OpenTicketDetail' && not grep { $v->optional($_)->is_valid } qw/TicketID TicketNumber GlobalTaskID/;
  return $v->has_error if $v->param('Code') eq 'OpenTaskDetail' && not grep { $v->optional($_)->is_valid } qw/TaskID/;
  return $v->has_error if $v->param('Code') eq 'OpenProject' && not grep { $v->optional($_)->is_valid } qw/ProjectID/;
  return $v->has_error if $v->param('Code') eq 'OpenTicketTime' && not grep { $v->optional($_)->is_valid } qw/TicketID/;
  return $v->has_error if $v->param('Code') eq 'OpenAppointment' && not grep { $v->optional($_)->is_valid } qw/AppointmentID/;
  return $v->has_error if $v->param('Code') eq 'OpenKBArticle' && not grep { $v->optional($_)->is_valid } qw/ID/;
  return $v->has_error if $v->param('Code') eq 'OpenOpportunity' && not grep { $v->optional($_)->is_valid } qw/OpportunityID/;
  return $v->has_error if $v->param('Code') eq 'OpenContract' && not grep { $v->optional($_)->is_valid } qw/ContractID/;
  return $v->has_error if $v->param('Code') eq 'OpenToDo' && not grep { $v->optional($_)->is_valid } qw/ToDoID/;
  return $v->has_error if $v->param('Code') eq 'OpenSalesOrder' && not grep { $v->optional($_)->is_valid } qw/SalesOrderID/;
  return $v->has_error if $v->param('Code') eq 'OpenServiceCall' && not grep { $v->optional($_)->is_valid } qw/ServiceCallID/;
  return $v->has_error if $v->param('Code') eq 'OpenAccount' && not grep { $v->optional($_)->is_valid } qw/Phone AccountID AccountName/;
  return $v->has_error if $v->param('Code') eq 'OpenContact' && not grep { $v->optional($_)->is_valid } qw/Email ContactID/;
  return $v->has_error if $v->param('Code') eq 'EditTimeEntry' && not grep { $v->optional($_)->is_valid } qw/WorkEntryID/;
  return $v->has_error if $v->param('Code') eq 'EditAccount' && not grep { $v->optional($_)->is_valid } qw/Phone AccountID/;
  return $v->has_error if $v->param('Code') eq 'EditContact' && not grep { $v->optional($_)->is_valid } qw/ContactID Email FirstName LastName/;
  return $v->has_error if $v->param('Code') eq 'EditInstalledProduct' && not grep { $v->optional($_)->is_valid } qw/InstalledProductID/;
  return $v->has_error if $v->param('Code') eq 'NewInstalledProduct' && not grep { $v->optional($_)->is_valid } qw/Phone AccountID/;
  return $v->has_error if $v->param('Code') eq 'NewAccountNote' && not grep { $v->optional($_)->is_valid } qw/AccountID/;
  return $v->has_error if $v->param('Code') eq 'NewContact' && not grep { $v->optional($_)->is_valid } qw/Phone AccountID/;
  return $v->has_error if $v->param('Code') eq 'NewTicket' && not grep { $v->optional($_)->is_valid } qw/Phone AccountID AccountName GlobalTaskID InstalledProductID/;
  return $v->has_error if $v->param('Code') eq 'TimeOffRequest' && not grep { $v->optional($_)->is_valid } qw/ResourceID ApproverID Tier/;
  return $v->has_error if $v->param('Code') eq 'IPDW' && not grep { $v->optional($_)->is_valid } qw/wizard/;

  #warn Data::Dumper::Dumper($v->output);
  my $url = Mojo::URL->new($self->ec_url)->query($v->output);
  $self->emit(ec => $url);
  return $url;
}

1;

=encoding utf8

=head1 NAME 

Mojo::Autotask::ExecuteCommand - Mojo interface to the Autotask ExecuteCommand API

=head1 SYNOPSIS

  my $ec = Mojo::Autotask::ExecuteCommand->new;
  say $ec->open_ticket_detail(TicketID => 12345);

=head1 DESCRIPTION

The L<Autotask ExecuteCommand API|https://www.autotask.net/help/Content/AdminSetup/2ExtensionsIntegrations/APIs/ExecuteCommand/UsingExecuteCommandAPI.htm>
is used to build URLs for sharing, enabling recipients to click on to have
direct access to a very specific window of data in Autotask. Of course, the
resource, as it is an Autotask resource, is protected by normal Autotask
authentication and authorization.

=head1 EVENTS

L<Mojo::Autotask::ExecuteCommand> inherits all events from
L<Mojo::EventEmitter> and can emit the following new ones.

=head2 ec

  $ec->on(ec => sub {
    my ($ec, $url) = @_;
    ...
  });

Emitted when a new URL is built.

  $ec->on(ec => sub {
    my ($ec, $url) = @_;
    say $url;
  });

=head1 ATTRIBUTES

L<Mojo::Autotask::Limits> implements the following attributes.

=head2 ec_url

  $ec_url = $ec->ec_url;
  $ec     = $ec->ec_url(Mojo::URL->new);

The base URL for all Execute Command API URLs. Defaults to setting
'/Autotask/AutotaskExtend/ExecuteCommand.aspx' as the path to L</"zone">.

=head2 zone

  $zone = $ec->zone;
  $ec   = $ec->zone(Mojo::URL->new);

Required. This is an Autotask account-specific URL and offers no default.

=head1 METHODS

=head2 AUTOLOAD

  $url = $ec->code(Name => 'Value');

Returns an L</"ec_url"> with the validated supplied arguments as the query.

Accepts methods are L</"camelize">'d forms of valid Execute Command Codes.

=head1 LICENSE AND COPYRIGHT

Copyright (C) 2019 Stefan Adams and others.

This program is free software, you can redistribute it and/or modify it under
the terms of the Artistic License version 2.0.

=head1 SEE ALSO

L<Mojo::Autotask>

=cut
