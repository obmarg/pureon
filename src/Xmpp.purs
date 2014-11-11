module Xmpp where

import Control.Monad.Eff
import Signal

foreign import data XmppClient :: *

foreign import xmppClient """
    function xmppClient(args) {
        var Client = require('node-xmpp-client'),
            ltx = require('ltx'),
            client = new Client(args);

        client.on('online', function(){
            console.log('online');

            client.send(
                new ltx.Element('presence', { })
                    .c('show').t('chat').up()
                    .c('status').t('WOO')
            );

        });
        client.on('error', function(){console.log('error')});
        client.on('disconnect', function(){console.log('disconnect')});
        return client;
    }
""" :: forall c. c -> XmppClient


foreign import stanzaParser' """
    function stanzaParser$prime(ChatMessage) {
        return function (ComposingStarted) {
        return function (ComposingStopped) {
        return function (OtherMessage) {
        return function (UnknownStanza) {
            return function(stanza) {
                if (stanza.is('message') && stanza.attrs.type === 'chat') {
                    var body = stanza.getChild('body'),
                        composing = stanza.getChild('composing'),
                        paused = stanza.getChild('paused');

                    if ( body ) {
                        return new ChatMessage(stanza.attrs.to)(stanza.attrs.from)(stanza.getChildText('body'));
                    }

                    if ( composing ) {
                        return new ComposingStarted(stanza.attrs.from);
                    }

                    if ( paused ) {
                        return new ComposingStopped(stanza.attrs.from);
                    }

                    return new OtherMessage(stanza.attrs.to)(stanza.attrs.from);
                } else {
                    return UnknownStanza;
                }
            }
        }}}}
    }
""" :: forall a b c d e f. a -> b -> c -> d -> e -> f -> Stanza

stanzaParser = stanzaParser' ChatMessage ComposingStarted ComposingStopped OtherMessage UnknownStanza


foreign import stanzaSignal' """
    function stanzaSignal$prime(constant) {
        return function (stanzaParser) {
        return function (UnknownStanza) {
        return function (xmppClient) {
            var out = constant(UnknownStanza);
            var util = require('util');

            xmppClient.on('stanza', function(stanza){
                //console.log(util.inspect(stanza, {'depth': 6}));
                out.set(stanzaParser(stanza));
            });

            return out;
        }}}
    }
""" :: forall c x y. (c -> Signal c) -> (x -> Stanza) -> y -> XmppClient -> Signal Stanza

stanzaSignal = stanzaSignal' constant stanzaParser UnknownStanza

foreign import data SendXmpp :: !

foreign import sendChat """
    function sendChat(client) {
        return function (user) {
        return function (message) {
            return function () {
                var ltx = require('ltx'),
                    messageElement = new ltx.Element(
                        'message',
                        {'to': user,
                         'from': 'bot@rolepoint.com',
                         'type': 'chat'}
                    );
                messageElement.c('body').t(message);

                // TODO: All the examples do the sending in a setTimeout.
                //       MIGHT need to do something like that.

                console.log('sending message to ' + user);
                console.log('message: '+ message);

                client.send(messageElement);
            }
        }}
    }
""" :: forall r. XmppClient -> User -> MessageText -> Eff (chat :: SendXmpp | r) Unit

type User = String
type MessageText = String

data Stanza = ChatMessage User User MessageText
            | ComposingStarted User
            | ComposingStopped User
            | OtherMessage User User
            | UnknownStanza

getSender :: Stanza -> User
getSender (ChatMessage _ from _) = from
getSender (ComposingStarted from) = from
getSender (ComposingStopped from) = from
getSender (OtherMessage from _) = from
getSender (UnknownStanza) = ""

getMessage :: Stanza -> String
getMessage (ChatMessage _ _ msg) = msg
getMessage _ = ""
