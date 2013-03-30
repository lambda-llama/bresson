```
 ____   ____     ___  _____ _____  ___   ____
|    \ |    \   /  _]/ ___// ___/ /   \ |    \
|  o  )|  D  ) /  [_(   \_(   \_ |     ||  _  |
|     ||    / |    _]\__  |\__  ||  O  ||  |  |
|  O  ||    \ |   [_ /  \ |/  \ ||     ||  |  |
|     ||  .  \|     |\    |\    ||     ||  |  |
|_____||__|\_||_____| \___| \___| \___/ |__|__|
```

`bresson` is an implementation of [BSON] [bson], a binary format for storing
objects, used by [MongoDB] [mongodb].

Why is `bresson` **better** than the official [`bson-haskell`] [bson-haskell]
driver? Here's why:

* Stores document in a [hash table][hashtable], instead of a list, which
  obviously improves lookup time for large documents.
* Uses [`text`] [text] for string-like data, instead of an unsupported
  [`compact-string`] [ustring].
* Provides separate classes `ToBson` and `FromBson`, instead of a single
  `Val` class.
* Well tested
  ([![Build Status][travis-img]][travis])
  and community maintained!

[bson]: http://bsonspec.org/#/specification
[mongodb]: http://mongodb.com
[bson-haskell]: https://github.com/mongodb/bson-haskell
[hashtable]: http://hackage.haskell.org/package/unordered-containers-0.2.3.0
[text]: http://hackage.haskell.org/package/text-0.11.2.3
[ustring]: http://hackage.haskell.org/package/compact-string-fix-0.3.2
[travis]: http://travis-ci.org/knsd/bresson
[travis-img]: https://secure.travis-ci.org/knsd/bresson.png

Example
-------

If you ever used [`bson-haskell`] [bson-haskell], the API should look
familiar, except for the `!?` operator, which allows retrieving nested
labels:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Binary (encode, decode)
import Data.Bson (BsonDocument, document, (=:), (!?))
import qualified Data.ByteString.Lazy as L

buffer :: L.ByteString
buffer = encode $ document [ "foo" =: "bar"
                           , "bar" =: document [ "boo" =: 42 ]
                           ]

doc :: BsonDocument
doc = decode buffer

value :: Int
value = doc !? "foo.bar.boo"  -- ==> 42
```

Documentation
-------------

Documentation for latest successful build is available [here] [docs]. Stable documentation
will be available as soon as first stable version will be released.

[docs]: http://knsd.github.com/bresson/docs/

Benchmarks
----------

There are two benchmarks currently:

* `fromBson . toBson` benchmarks on various types, [latest build] [benchmark]
* `encode . decode` benchmark against `bson`] [bson-hackage] library, [latest build] [benchmark-against-bson]

[bson-hackage]: http://hackage.haskell.org/package/bson
[benchmark]: http://knsd.github.com/bresson/bresson-benchmarks.html
[benchmark-against-bson]: http://knsd.github.com/bresson/bresson-benchmark-against-bson.html
