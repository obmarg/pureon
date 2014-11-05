module Xmpp where

import Control.Monad.Eff

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
