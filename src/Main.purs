module Main where

import Prelude
import Control.Monad.Eff
import Debug.Trace
import Data.Either
import Data.Monoid
import Data.Foldable
import Signal

import Xmpp
import HubotInterface
import Actions

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
            runSignal $ (takeActions client <$> actionsSignal <*> stanzas)
