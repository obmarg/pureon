module HubotInterface ( getActionFuncs ) where

import Control.Monad.Eff
import Data.Array
-- Temporarily using head from unsafe
import qualified Data.Array.Unsafe as Unsafe
import Data.Maybe
import Data.String.Regex
import Debug.Trace

import Signal
import Xmpp
import Actions

-- |
-- A fake Hubot "Response" object to be passed in to handlers.
--
type HubotResponder = { send :: forall r. MessageText -> Unit
                      , reply :: forall r. MessageText -> Unit
                      , emote :: forall r. MessageText -> Unit
                      , matches :: [String]
                      , random :: forall a. [a] -> a
                      }

foreign import runMsgPure """
    function runMsgPure(f) {
        f();
        return {};
    }
""" :: forall r. Eff (chat :: SendXmpp | r) Unit -> Unit

foreign import randomChoice """
    function randomChoice(choices){
        return choices[Math.floor(Math.random() * choices.length)]
    }
""" :: forall a. [a] -> a

makeResponder :: XmppClient -> Stanza -> [String] -> HubotResponder
makeResponder xmppClient stanza matches =
    { send: runMsgPure <<< sendChat xmppClient (getSender stanza)
    , reply: runMsgPure <<< sendChat xmppClient (getSender stanza)
    , emote: runMsgPure <<< sendChat xmppClient (getSender stanza)
    , matches: matches
    , random: randomChoice
    }

type Handler = HubotResponder -> Unit

-- |
-- The various actions that hubot scripts support
--
data HubotAction
    -- |
    -- Performs an action when we hear a phrase.
    --
    = Hear Regex Handler
    -- |
    -- Performs an action when we are direct messaged a phrase.
    --
    | Respond Regex Handler


foreign import getModule' """
    function getModule$prime(constant) {
    return function(Hear) {
    return function(Respond) {
    return function(name) {
        console.log("loading module " + name);

        // Register coffeescript so we can just require() modules written in
        // it.
        require('coffee-script/register');

        var Path = require('path'),
            util = require('util'),
            signal = constant([]),
            moduleContents = [];

        function hubotActionConstructor(action) {
            return function(regex, handler) {
                console.log('Adding handler: ' + regex);

                moduleContents.push(action(regex)(handler));
                console.log('handlers: ' + util.inspect(moduleContents));
                signal.set(moduleContents);
            }
        }

        var robot = {
              hear: hubotActionConstructor(Hear),
              respond: hubotActionConstructor(Respond),
              loadFile: loadFile
        }

        function loadFile(path, file) {
            var ext = Path.extname(file),
                full = Path.join(path, Path.basename(file, ext));

            console.log('loading file ' + full);

            require(full)(robot);
        }

        require(name)(robot);

        return signal;
    }}}} """ :: forall a b c. a -> b -> c -> String -> Signal [HubotAction]

getModule = getModule' constant Hear Respond

getActionFuncs :: String -> Signal [ActionFunc]
getActionFuncs moduleName = moduleActions ~> (map actionToHandler)
    where moduleActions = getModule moduleName

actionToHandler :: HubotAction -> XmppClient -> Stanza -> Eff (trace ::Debug.Trace.Trace, chat :: SendXmpp) Unit
actionToHandler action xmppClient stanza =
    case getMatches action stanza of
         Just matches -> do
             trace "Match"
             runHandler (getHandler action) (makeResponder xmppClient stanza matches)
         Nothing -> trace "No match"

getHandler :: HubotAction -> Handler
getHandler (Hear _ handler) = handler
getHandler (Respond _ handler) = handler

getMatches :: HubotAction -> Stanza -> Maybe [String]
getMatches (Hear re _) (ChatMessage _ _ message) = match re message
getMatches (Respond re _) (ChatMessage _ _ message) = match re message
getMatches _ _ = Nothing


foreign import runHandler """
    function runHandler(handler) {
        return function(responder) {
            return function(){
                // TODO: may want to catch exceptions.
                //       for now though: let it crash.
                handler(responder);

                return {};
            }
        }
    }
""" :: Handler -> HubotResponder -> Eff (trace ::Debug.Trace.Trace, chat :: SendXmpp) Unit


foreign import makeActionFunc """
    return makeActionFunc(handler) {
        return function(xmppClient) {
        return function(stanza) {
            return function(){
                handler({
                       send: client.send;
                });
            }
        }
        }
    } """ :: Handler -> XmppClient -> Stanza -> Eff (trace ::Debug.Trace.Trace, chat :: SendXmpp) Unit

-- Ok, so I want to get modules, and from there I want some ActionFuncs
-- ActionFuncs take a client, a stanza and return an Eff
--
-- So, I can get my modules and return a list of Actions.
-- Actions = Hear Regex Handler | Respond Regex Handler | etc.

-- I can then generate functions that will match and call the handler
-- following the ActionFunc interface.

-- So I can get my modules and run them through a function that
-- returns a list of ActionFuncs.
