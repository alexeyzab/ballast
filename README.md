Ballast [![TravisCI](https://travis-ci.org/bitemyapp/ballast.svg)](https://travis-ci.org/bitemyapp/ballast)
==========


A Haskell wrapper for the Shipwire API
==============================================

Example Usage
---------------
You should set up two ENV variables: `SHIPWIRE_USER` and `SHIPWIRE_PASS` that represent your Shipwire account's email and password.

```haskell
main :: IO ()
main = do
  let config <- sandboxEnvConfig
  result <- shipwire config $ getReceivings -&- (ExpandReceivingsParam [ExpandAll])
  return result
```

Testing
---------

The TravisCI tests are run using [Stack](http://docs.haskellstack.org/en/stable/README.html). You should use Stack instead of `cabal` to build and test Ballast to avoid compatibility problems.

Steps to run the tests locally:
  1. Create a sandbox [Shipwire account](https://beta.shipwire.com/).
  2. Go to [Warehouse
     overview](https://merchant.beta.shipwire.com/merchants/account/warehouses/warehouseid/)
     and add a warehouse named `Test Warehouse`.
  3. Accept the `accurate dimensions` agreement by [creating a dummy
     product](https://merchant.beta.shipwire.com/app/products).
  2. Install [Stack] (http://docs.haskellstack.org/en/stable/README.html#how-to-install)
  3. In your local Ballast directory, run `stack setup && stack build`.
  5. Run `stack test` in your local Ballast directory.
