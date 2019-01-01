Changelog for script-monad
==========================

0.0.3
-----

This release has some significant changes to type names and signatures. The good news is that these changes make the code simpler and more modular. The bad news is that it now uses the `QuantifiedConstraints` extension, available only in GHC >=8.6.

* Added
    * `liftScriptTT` and `liftHttpTT`
* Changed
    * Most functions now have additional `Monad` and `MonadTrans` constraints.
    * `ScriptT` is now `ScriptTT` and takes the effect monad as an explicit type parameter. Now acts like a monad transformer transformer.
    * `Script` is now `ScriptT` and takes the effect monad as an explicit type parameter, reflecting its status as a monad transformer
    * `HttpT` is now `HttpTT` and takes the effect monad as an explicit type parameter. Now acts like a monad transformer transformer.
    * `Http` is now `HttpT` and takes the effect monad as an explicit type parameter, reflecting its status as a monad transformer
    * `execScriptTM` is now `execScriptTT` and does not take an explicit `lift` parameter, using the generic `MonadTrans` instance instead.
    * `checkScriptTM` is now `checkScriptTT` and does not take an explicit `lift` parameter, using the generic `MonadTrans` instance instead.
    * `execHttpTM` is now `execHttpTT` and does not take an explicit `lift` parameter, using the generic `MonadTrans` instance instead.
    * `checkHttpTM` is now `checkHttpTT` and does not take an explicit `lift` parameter, using the generic `MonadTrans` instance instead.
* Removed
    * `Script.lift`, in favor of a generic `MonadTrans` instance
    * `liftHttpT`, in favor of a generic `MonadTrans` instance
    * `execScriptT`, `execScript`, `checkScript`, and `checkScript`, which use a pure evaluator. These are subsumed by `ScriptTT` where the base monad is `Identity`.
    * `execScriptM` and `checkScriptM`, which are subsumed by `ScriptTT` with the `IdentityT` transformer.
    * `execHttpM` and `checkHttpM`, which are subsumed by `HttpTT` with the `IdentityT` transformer.



0.0.2.1
-------

* Added
    * Semigroup instance for `W`



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
