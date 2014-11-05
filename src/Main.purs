module Main where

import Prelude
import Control.Monad.Eff
import Debug.Trace
import Data.Either
import Data.Monoid
import Data.Foldable
import Signal

import Node.Thunk

import Xmpp
import HubotInterface
import Actions

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

-- Returns a "clone" signal containing the value of the original signal.
-- Does not copy any of the subs etc, just the value.
-- Can be used for creating an "initial" signal to concat other signals.
foreign import cloneSigValP """
    function cloneSigValP(constant) {
        return function (origSig) {
            return constant(origSig.get());
        }
    }
""" :: forall c. (c -> Signal c) -> Signal c -> Signal c

cloneSigVal = cloneSigValP constant

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


echoWhatDoYouMean :: XmppClient -> Stanza -> Eff (trace ::Debug.Trace.Trace, chat :: SendXmpp) Unit
echoWhatDoYouMean client (ChatMessage _ from message) = do
    sendChat client from ("What do you mean '" ++ message ++ "'?")
echoWhatDoYouMean client _ = do
    print "WUT"


main =
    let client = xmppClient params
        stanzas = stanzaSignal client
        messages = messagesSignal stanzas
        actionsSignal = getActionFuncs "hubot-thank-you" in
        -- TODO: need a way to merge signals of lists if there isn't one already
        do
            runSignal $ messages ~> printMessages
            --runSignal $ (takeActions client [echoWhatDoYouMean] messages)
            --runSignal $ (takeActions client (getActionFuncs "hubot-thank-you") messages)
            runSignal $ (takeActions client <$> actionsSignal <*> messages)
