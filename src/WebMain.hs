{- boolexman -- boolean expression manipulator
Copyright (c) 2018 Mert Bora ALPER <bora@boramalper.org>

Permission to use, copy, modify, and/or distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
-}
module WebMain where

import GHCJS.Types (JSRef, JSVal)
import GHCJS.Foreign.Callback (syncCallback1', releaseCallback, Callback)
import GHCJS.Marshal.Pure
import Data.JSString (pack, unpack, JSString, JSString)

import Process

foreign import javascript unsafe "boolexman = $1"
    js_set_somethingUseful :: Callback (JSVal -> IO JSVal) -> IO JSVal

-- https://stackoverflow.com/a/31611311/4466589
main :: IO ()
main = do
    cb <- syncCallback1' somethingUseful
    js_set_somethingUseful cb
    releaseCallback cb

somethingUseful :: JSVal -> IO JSVal
somethingUseful = return . pToJSVal . pack . xxx . process  . unpack . pFromJSVal
    where
        xxx :: Either String String -> String
        xxx (Left  err) = "I" ++ err  -- (I)nline
        xxx (Right res) = "D" ++ res  -- (D)isplay
