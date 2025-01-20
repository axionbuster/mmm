{-# LANGUAGE CPP #-}

-- |
-- Module: M.IO.Internal.WinHack
-- Description: Network error recovery handlers
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- This module provides platform-specific recovery mechanisms for network errors.
-- On Windows, the 'network' package has a critical issue where socket exceptions
-- can leave the server in an unresponsive state. This module provides a forceful
-- process termination mechanism on Windows, while using
-- standard exit functions on other platforms.
--
-- This is primarily used by 'M.IO.Internal.EffectSocket.withtalkingserver'.
module M.IO.Internal.WinHack (killonexc) where

import Control.Exception
import Debug.Trace

#ifdef mingw32_HOST_OS

import Foreign hiding (void)
import Foreign.C.Types

-- | Get the current process handle (Win32 API)
foreign import ccall safe "GetCurrentProcess"
  getcurrentprocess :: Ptr () -- constant

-- | Terminate a process given its handle (Win32 API)
foreign import ccall safe "TerminateProcess"
  terminateprocess :: Ptr () -> CUInt -> IO ()

-- | Wrap an IO action with exception handling that forcefully terminates the process
-- when network errors occur. This is specifically needed because the "network" package
-- on Windows can leave sockets in an unresponsive state after exceptions, particularly
-- during accept/recv operations.
--
-- Used in 'm.io.internal.effectsocket.withtalkingserver' to ensure the server can be restarted cleanly after
-- network failures.
--
-- The handler will:
--
-- 1. Log the exception details to stderr
-- 2. Force-terminate the process via Win32 API
--
-- @
-- withtalkingserver u host port handler = do
--   withEffToIO u \run -> do
--     'killonexc' do  -- Forces process termination on network errors
--       runTCPServer host port \sock -> ...
-- @
killonexc :: IO a -> IO a
killonexc =
  handle \(e :: SomeException) -> do
    traceIO $ displayException e -- print to stderr + newline + flush
    terminateprocess getcurrentprocess 0 -- infallible according to Microsoft
    error "killonexc/Win32: impossible to get here"

#else

import System.Exit

-- | Wrap an IO action with exception handling that exits the process gracefully
-- on non-Windows platforms. While these platforms don't suffer from the "network"
-- package's socket unresponsiveness issues, this handler maintains a consistent
-- interface with the Windows version.
--
-- Used in 'm.io.internal.effectsocket.withtalkingserver' to handle network failures uniformly across platforms.
--
-- The handler will:
--
-- 1. Log the exception details to stderr
-- 2. Exit the process with exitSuccess
--
-- @
-- withtalkingserver u host port handler = do
--   withEffToIO u \run -> do
--     killonexc do  -- Graceful exit on network errors
--       runTCPServer host port \sock -> ...
-- @
killonexc :: IO a -> IO a
killonexc =
  handle \(e :: SomeException) -> do
    traceIO $ displayException e -- print to stderr + newline + flush
    void exitSuccess
    error "killonexc/Non-Win32: impossible to get here"

#endif
