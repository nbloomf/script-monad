{- |
Module      : Data.LogSeverity
Description : Syslog-style log message severities
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

Syslog-style log message severities.
-}


module Data.LogSeverity (
    LogSeverity(..)
  , colorBySeverity
) where

-- | [Syslog](https://en.wikipedia.org/wiki/Syslog) style log severities.
data LogSeverity
  = LogDebug -- ^ Debug-level messages
  | LogInfo -- ^ Informational messages
  | LogNotice -- ^ Normal but significant condition
  | LogWarning -- ^ Warning conditions
  | LogError -- ^ Error conditions
  | LogCritical -- ^ Critical conditions
  | LogAlert -- ^ Action must be taken immediately
  | LogEmergency -- ^ System is unusable
  deriving (Eq, Ord, Show)

-- | Pretty prints a simple log header.
colorBySeverity
  :: LogSeverity
  -> String -- ^ Printed before the severity label; i.e. a timestamp
  -> String
colorBySeverity severity msg = case severity of
  LogDebug -> "\x1b[1;32m" ++ msg ++ " DEBUG \x1b[0;39;49m"
  LogInfo -> "\x1b[1;32m" ++ msg ++ " INFO \x1b[0;39;49m"
  LogNotice -> "\x1b[1;34m" ++ msg ++ " NOTICE \x1b[0;39;49m"
  LogWarning -> "\x1b[1;33m" ++ msg ++ " WARNING \x1b[0;39;49m"
  LogError -> "\x1b[1;31m" ++ msg ++ " ERROR \x1b[0;39;49m"
  LogCritical -> "\x1b[1;31m" ++ msg ++ " CRITICAL \x1b[0;39;49m"
  LogAlert -> "\x1b[1;35m" ++ msg ++ " ALERT \x1b[0;39;49m"
  LogEmergency -> "\x1b[1;35m" ++ msg ++ " EMERGENCY \x1b[0;39;49m"
