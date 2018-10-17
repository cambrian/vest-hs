# vest-base

Vest standard library. Use with
```
import Vest
```

The other top-level modules package other imports in a friendly way (since Haskell has poor support
for module hierarchies) and are intended to be imported qualified. E.g.
```
import qualified Db
import qualified Stream
```

The Test module is an exception and should be imported unqualified.

(built on top of Protolude).
