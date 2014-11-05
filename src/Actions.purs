module Actions where

import Control.Monad.Eff
import Data.Foldable
import Debug.Trace

import Signal
import Xmpp

type ActionFunc = forall r. (XmppClient -> Stanza -> Eff (chat :: SendXmpp, trace :: Debug.Trace.Trace) Unit)

takeActions :: XmppClient -> [ActionFunc] -> Stanza -> Eff (chat :: SendXmpp, trace :: Debug.Trace.Trace) Unit
takeActions client actionFuncs stanza =
    foreachE actionFuncs (\func -> func client stanza)
