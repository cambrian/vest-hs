# vest-base

Vest standard library (built on Protolude). Usage:
```
import Vest
```

The other top-level modules package imports in a friendly way (since Haskell has poor support for
module hierarchies) and are intended to be imported qualified. For instance:
```
import qualified Db
import qualified Stream
```

The `Test` module is an exception and should be imported unqualified.
