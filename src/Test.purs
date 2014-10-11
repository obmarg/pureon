module Test where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Debug.Trace
import Data.Either
import Signal

import Node.Thunk

foreign import fs "var fs = require('fs');" :: {
  readFile :: ThunkFn1 String String
  }

readFile = runThunkFn1 fs.readFile

readFileAndPrintContents = do
  contents <- readFile "./README.md"
  --liftEff (print contents)
  return (print contents)

oldMain = runThunk readFileAndPrintContents handle
    where
        handle (Left err) = (print err)
        handle (Right result) = result

printHi :: forall i r. i -> Eff (trace :: Debug.Trace.Trace | r) Unit
printHi _ = print "Hi"

type Time = Number

foreign import everyP """
    function everyP(constant) {
        return function(t) {
            var i = 1,
                out = constant(i);
            setInterval(function() {
                i += 1;
                out.set(i);
            }, t);
            return out;
        }
    }
""" :: forall c. (c -> Signal c) -> Time -> Signal Time

every = everyP constant

frameRate = every 1000

foreign import filterSignalP """
    function filterSignalP(constant) {
    return function (predicate) {
    return function (inputSignal) {
        var out = constant(inputSignal.get());
        inputSignal.subscribe(function(newVal){
            if (predicate(newVal)) {
               out.set(newVal);
            }
        });
        return out;
    }}}
""" :: forall a. (a -> Signal a) -> (a -> Boolean) -> Signal a -> Signal a

filterSignal = filterSignalP constant
removeSignal predicate = filterSignal (predicate >>> not)

-- TODO: Need to define a foreign fn that takes
-- parameters and returns a client.
--
-- Then need to convert that to a "stanza" stream.
-- Can then further filter that.
--
-- Also have a foreign "Send" function that
-- returns an Eff.

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

type User = String
type MessageText = String

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
                // TODO: All the examples do the sending in a setTimeout.  MIGHT need to do something like that if the Eff stuff doesn't delay enough already.

                console.log('blah');
                client.send(messageElement);
            }
        }}
    }
""" :: forall r. XmppClient -> User -> MessageText -> Eff (r) Unit

-- foreign import data Stanza :: *

-- TODO: Need a better type definition of a stanza.
--       Probably a union type Message | Unknown

data Stanza = ChatMessage User User MessageText
            | ComposingStarted User
            | ComposingStopped User
            | OtherMessage User User
            | UnknownStanza

foreign import stanzaParserP """
    function stanzaParserP(ChatMessage) {
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

stanzaParser = stanzaParserP ChatMessage ComposingStarted ComposingStopped OtherMessage UnknownStanza


foreign import stanzaSignalP """
    function stanzaSignalP(constant) {
        return function (stanzaParser) {
        return function (UnknownStanza) {
        return function (xmppClient) {
            // TODO: {} isn't a valid stanza...
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

stanzaSignal = stanzaSignalP constant stanzaParser UnknownStanza

printMessages :: forall r. Stanza -> Eff (trace :: Debug.Trace.Trace | r) Unit
printMessages (ChatMessage _ from message) = print $ "Message from " ++ from ++ " - " ++ message
printMessages (ComposingStarted from) = print $ from ++ " started typing."
printMessages (ComposingStopped from) = print $ from ++ " stopped typing."
printMessages _ = print "."

isChatMessage :: Stanza -> Boolean
isChatMessage (ChatMessage _ _ _) = true
isChatMessage _ = false

messagesSignal = filterSignal isChatMessage

params = { jid: "bot@rolepoint.com", password: "test", "host": "localhost", reconnect: true }

takeActions :: forall r. XmppClient -> Stanza -> Eff (trace ::Debug.Trace.Trace | r) Unit
takeActions client (ChatMessage _ from message) = do
    sendChat client from ("What do you mean '" ++ message ++ "'?")
takeActions client _ = do
    print "WUT"

-- TODO: Basically I need to lift my "action" functions in to the signals.
--       Then merge those signals using SemiGroup.

main =
    let client = xmppClient params
        stanzas = stanzaSignal client
        messages = messagesSignal stanzas in
        do
            runSignal $ messages ~> printMessages
            runSignal $ messages ~> takeActions client
