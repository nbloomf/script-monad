Changelog for script-monad
==========================

0.0.2
-----

* Added
    * ScriptT: `draft` function
    * HttpT: `catchAnyError` function
    * HttpT: `logDebug`, `logInfo`, `logNotice`, `logWarning`, `logError`, `logCritical`,
      `logAlert`, `logEmergency`, and `setLogSeverity` functions
    * HttpT: `printHttpLogs`
    * HttpT: `_logMinSeverity` option
* Changed
    * HttpT: refactored logging to use syslog conventions
* Fixed
    * ScriptT: Bug in implementation of `catch` was cutting off the logs
* Removed
    * HttpT: `logEntry` function; deprecated in favor of syslog-flavored logger functions



0.0.1
-----

* Added
    * Script and ScriptT: Hand-rolled stack of error, reader, writer, state, and prompt
    * Http and HttpT: Monad transformer for HTTP sessions with batteries included
    * MockIO: Fake IO monad for testing
