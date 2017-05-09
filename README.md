Ballast [![TravisCI](https://travis-ci.org/bitemyapp/ballast.svg)](https://travis-ci.org/bitemyapp/ballast)
==========

![Ship cross-section showing ballast](./ballast.jpg)
<sup><sub>Image courtesy University of Edinburgh</sub></sup>


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
  
What's implemented so far
---------------------------
The core functionality is complete. What's left is to add request and return types for the remaining endpoints.

- [ ] **Order**
  - [x] *GET	/api/v3/orders*

  - [x] *GET	/api/v3/orders/{id} or /api/v3/orders/E{externalId}*

  - [x] *POST	/api/v3/orders*

  - [ ] *PUT	/api/v3/orders/{id} or /api/v3/orders/E{externalId}*

  - [x] *POST	/api/v3/orders/{id}/cancel or /api/v3/orders/E{externalId}/cancel*

  - [ ] *GET	/api/v3/orders/{id}/items or /api/v3/orders/E{externalId}/items*

  - [ ] *GET	/api/v3/orders/{id}/holds or /api/v3/orders/E{externalId}/holds*

  - [x] *GET	/api/v3/orders/{id}/trackings or /api/v3/orders/E{externalId}/trackings*

  - [ ] *GET	/api/v3/orders/{id}/splitOrders or /api/v3/orders/E{externalId}/splitOrders*

  - [ ] *GET	/api/v3/orders/{id}/pieces or /api/v3/orders/E{externalId}/pieces*

  - [ ] *GET	/api/v3/orders/{id}/extendedAttributes or /api/v3/orders/E{externalId}/extendedAttributes*

  - [ ] *GET	/api/v3/orders/{id}/returns or /api/v3/orders/E{externalId}/returns*

  - [ ] *POST	/api/v3/orders/{id}/holds/clear or /api/v3/orders/E{externalId}/holds/clear*

  - [ ] *POST	/api/v3/orders/{id}/markProcessed or /api/v3/orders/E{externalId}/markProcessed*

  - [ ] *POST	/api/v3/orders/{id}/markSubmitted or /api/v3/orders/E{externalId}/markSubmitted*

  - [ ] *POST	/api/v3/orders/{id}/markComplete or /api/v3/orders/E{externalId}/markComplete*

- [ ] **Purchase Order**
  - [ ] *GET	/api/v3/purchaseOrders*
  
  - [ ] *GET	/api/v3/purchaseOrders/{id} or /api/v3/purchaseOrders/E{externalId}*
  
  - [ ] *POST	/api/v3/purchaseOrders*
  
  - [ ] *PUT	/api/v3/purchaseOrders/{id} or /api/v3/purchaseOrders/E{externalId}*
  
  - [ ] *POST	/api/v3/purchaseOrders/{id}/cancel or /api/v3/purchaseOrders/E{externalId}/cancel*
  
  - [ ] *POST	/api/v3/purchaseOrders/{id}/hold or /api/v3/purchaseOrders/E{externalId}/hold*
  
  - [ ] *POST	/api/v3/purchaseOrders/{id}/hold/clear or /api/v3/purchaseOrders/E{externalId}/hold/clear*
  
  - [ ] *GET	/api/v3/purchaseOrders/{id}/items or /api/v3/purchaseOrders/E{externalId}/items*
  
  - [ ] *GET	/api/v3/purchaseOrders/{id}/trackings or /api/v3/purchaseOrders/E{externalId}/trackings*
  
  - [ ] *POST	/api/v3/purchaseOrders/{id}/approve or /api/v3/purchaseOrders/E{externalId}/approve*
  
- [ ] **Container (API v3.1)**
  - [ ] *GET	/api/v3.1/containers?isActive={isActive}&type={type}&warehouseIds={warehouseIds}&warehouseExternalIds={warehouseExternalIds}*
  
  - [ ] *POST	/api/v3.1/containers*
  
  - [ ] *PUT	/api/v3.1/containers/{id}*
  
  - [ ] *GET	/api/v3.1/containers/{id}*
  
- [x] **Address Validation (API v3.1)**
  - [x] *POST	/api/v3.1/addressValidation*
  
- [ ] **Stock**
  - [x] *GET	/api/v3/stock*
  
  - [ ] *POST	/api/v3/stock/adjust*
  
- [ ] **Rate**
  - [x] *POST	/api/v3/rate*
  
  - [ ] *POST	/api/v3.1/rate*
  
- [x] **Receiving**
  - [x] *GET	/api/v3/receivings*
  
  - [x] *POST	/api/v3/receivings*
  
  - [x] *GET	/api/v3/receivings/{id}*
  
  - [x] *PUT	/api/v3/receivings/{id}*
  
  - [x] *POST	/api/v3/receivings/{id}/cancel*
  
  - [x] *POST	/api/v3/receivings/{id}/labels/cancel*
  
  - [x] *GET	/api/v3/receivings/{id}/holds*
  
  - [x] *GET	/api/v3/receivings/{id}/instructionsRecipients*
  
  - [x] *GET	/api/v3/receivings/{id}/items*
  
  - [x] *GET	/api/v3/receivings/{id}/shipments*
  
  - [x] *GET	/api/v3/receivings/{id}/trackings*
  
  - [x] *GET	/api/v3/receivings/{id}/labels*
  
- [ ] **Return**
  - [ ] *GET	/api/v3/returns*
  
  - [ ] *POST	/api/v3/returns*
  
  - [ ] *GET	/api/v3/returns/{id}*
  
  - [ ] *POST	/api/v3/returns/{id}/cancel*
  
  - [ ] *GET	/api/v3/returns/{id}/holds*
  
  - [ ] *GET	/api/v3/returns/{id}/items*
  
  - [ ] *GET	/api/v3/returns/{id}/trackings*
  
  - [ ] *GET	/api/v3/returns/{id}/labels*
  
- [x] **Product**
  - [x] *GET	/api/v3/products*
  
  - [x] *POST	/api/v3/products*
  
  - [x] *PUT	/api/v3/products*
  
  - [x] *PUT	/api/v3/products/{id}*
  
  - [x] *GET	/api/v3/products/{id}*
  
  - [x] *POST	/api/v3/products/retire*
  
- [ ] **Vendor (API v3.1)**
  - [ ] *GET	/api/v3.1/vendors?id={id}&externalId={externalId}&name={name}&status={status}*
  
  - [ ] *POST	/api/v3.1/vendors*
  
  - [ ] *PUT	/api/v3.1/vendors/{id}*
  
  - [ ] *GET	/api/v3.1/vendors/{id}*
  
  - [ ] *POST	/api/v3.1/vendors/{id}/retire*
  
  - [ ] *GET	/api/v3.1/vendors/{id}/extendedAttributes*
  
- [ ] **Webhook**
  - [ ] *GET	/api/v3/webhooks*
  
  - [ ] *POST	/api/v3/webhooks*
  
  - [ ] *GET	/api/v3/webhooks/{id}*
  
  - [ ] *PUT	/api/v3/webhooks/{id}*
  
  - [ ] *DELETE	/api/v3/webhooks/{id}*
  
  - [ ] *GET	/api/v3/secret*
  
  - [ ] *POST	/api/v3/secret*
  
  - [ ] *GET	/api/v3/secret/{id}*
  
  - [ ] *DELETE	/api/v3/secret/{id}*
