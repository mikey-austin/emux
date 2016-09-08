package Emux::Client::Websocket;

use strict;
use warnings;
use parent 'Emux::Client';

use Protocol::WebSocket::Handshake::Server;
use Protocol::WebSocket::Frame;

sub new {
    my ($class, %args) = @_;
    my $self = $class->SUPER::new(%args);

    $self->{_websocket} = 0;
    $self->{_hs} = Protocol::WebSocket::Handshake::Server->new;
    $self->fh->blocking(0);

    return $self;
}

sub send {
    my ($self, $message) = @_;

    if ($self->{_hs}->is_done) {
        my $frame = $self->{_hs}->build_frame(
            type   => 'text',
            buffer => $message->encode
        );
        print { $self->fh } $frame->to_bytes;
    }
    else {
        $self->SUPER::send($message);
    }
}

sub receive {
    my ($self, $factory) = @_;

    # Check for an upgrade by the handshake.
    my $res;
    my ($input, $message);
    {
        local $/;
        $res = $self->fh->sysread($input, 1024);
    }
    if ($res and !$self->{_hs}->is_done) {
        if ($self->{_hs}->parse($input)) {
            $self->{_websocket} = 1;

            # Print handshake response.
            print { $self->fh } $self->{_hs}->build_frame(
                buffer => $self->{_hs}->to_string)->to_bytes
                if $self->{_hs}->is_done;

            return;
        }

        # Fallthrough, normal TCP connection.
        if ($self->{_websocket}) {
            my $frame = $self->{_hs}->build_frame;
            $frame->append($input);
            my $content = '';
            while (defined(my $body = $frame->next)) {
                $content .= $frame->new($body)->to_bytes;
            }
            $input = $content;
        }
        else {
            # Read as much as we can from the socket.
            local $/;
            while ($self->fh->sysread(my $chunk, 1024)) {
                $input .= $chunk;
            }
        }

        return Emux::Message->parse($input, $factory);
    }
}

1;
